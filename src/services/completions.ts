// fixme - use non-relative paths, which requires we get ts-node to resolve the paths during testing
// we can get it to compile with tsc with non-relative paths, but loading it during testing does not work
import { Project } from "../compiler/project"
import { Node, NodeKind, CallExpression, CfTag, StaticallyKnownScopeName, SymbolTable, SymTabEntry, SimpleStringLiteral, SourceFile } from "../compiler/node"
import { CfFileType, TokenType } from "../compiler/scanner";
import { isFunctionOverloadSet, isFunctionSignature, isLiteralType, isStruct, TypeFlags, _Type } from "../compiler/types";
import { isExpressionContext, isCfScriptTagBlock, stringifyCallExprArgName, getSourceFile, cfcIsDescendantOf, isPublicMethod } from "../compiler/utils";
import { tagNames } from "./tagnames";
import { Checker } from "../compiler/checker";

export const enum CompletionItemKind {
    tagName,
    function,
    variable,
    structMember,
    stringLiteral
}

export interface CompletionItem {
    label: string,
    kind: CompletionItemKind,
    detail?: string,
    insertText?: string,
    sortText?: string,
}

function getCallExprArgIndex(callExpr: CallExpression, node: Node) {
    const index = callExpr.args.findIndex((v) => v.expr === node);
    return index === -1 ? undefined : index;
}

function getStringLiteralCompletions(checker: Checker, sourceFile: SourceFile, node: SimpleStringLiteral) : CompletionItem[] | undefined {
    if (node.parent?.kind === NodeKind.callArgument && node.parent.parent?.kind === NodeKind.callExpression) {
        const ziArgIndex = getCallExprArgIndex(node.parent.parent, node);
        if (ziArgIndex === undefined) return undefined;
        const symbol = checker.getSymbol(node.parent.parent.left, sourceFile);
        if (!symbol) return undefined;

        let strings : string[] = [];
        if (isFunctionOverloadSet(symbol.symTabEntry.type)) {
            for (const overload of symbol.symTabEntry.type.overloads) {
                const type = overload.params[ziArgIndex]?.type;
                if (!type) continue;
                if (type.flags & TypeFlags.string && isLiteralType(type)) {
                    strings.push(type.literalValue as string);
                }
            }
        }
        else if (isFunctionSignature(symbol.symTabEntry.type)) {
            return undefined; // not yet impl'd
        }

        return strings.length > 0 ? strings.map((s) => {
            return {
                label: s,
                kind: CompletionItemKind.stringLiteral
            }
        }) : undefined;
    }

    return undefined;
}

export function getCompletions(project: Project, fsPath: string, targetIndex: number, triggerCharacter: string | null) : CompletionItem[] {
    if (!project) return [];

    const parsedSourceFile = project.getParsedSourceFile(fsPath);
    const checker = project.__unsafe_dev_getChecker();
    const node = project.getNodeToLeftOfCursor(fsPath, targetIndex); // fixme: probably generally want "getInterestingNodeLeftOfCursor" to not grab terminals, but all the ".parent.parent..." chains would have to be fixed up

    if (!parsedSourceFile || !node) return [];

    if ((node.kind === NodeKind.terminal || node.kind === NodeKind.textSpan) && node.parent?.kind === NodeKind.simpleStringLiteral) {
        const checker = project.__unsafe_dev_getChecker();
        const sourceFile = getSourceFile(node);
        if (checker && sourceFile) {
            return getStringLiteralCompletions(checker, sourceFile, node.parent) ?? [];
        }
        else {
            return [];
        }
    }
    // after the above, we've handled all the string completions
    if (triggerCharacter === "\"" || triggerCharacter === "'") {
        return [];
    }

    let callExpr : CallExpression | null = (node.parent?.parent?.kind === NodeKind.callArgument && !node.parent.parent.equals)
        ? node.parent.parent.parent as CallExpression // inside a named argument `foo(a|)
        : (node.kind === NodeKind.terminal && node.token.type === TokenType.LEFT_PAREN && node.parent?.kind === NodeKind.callExpression)
        ? node.parent as CallExpression // right on `foo(|`
        : (node.parent?.kind === NodeKind.terminal && node.parent.token.type === TokenType.LEFT_PAREN && node.parent.parent?.kind === NodeKind.callExpression)
        ? node.parent.parent // on whitespace after `foo(   |`
        : (node.parent?.kind === NodeKind.terminal && node.parent.token.type === TokenType.COMMA && node.parent.parent?.parent?.kind === NodeKind.callExpression)
        ? node.parent.parent.parent // after a comma `foo(arg0, |`
        : null;

    // a whitespace or left-paren trigger character is only used for showing named parameters inside a call argument list
    if ((triggerCharacter === " " || triggerCharacter === "(") && !callExpr) {
        if (!callExpr) return [];
    }

    const expressionContext = isExpressionContext(node);

    if (!expressionContext) {
        if (node.parent?.kind === NodeKind.tag && (node === node.parent.tagStart || node === node.parent.tagName)) {
            return tagNames.map((name) : CompletionItem => {
                return { 
                    label: "cf" + name,
                    kind: CompletionItemKind.tagName,
                    detail: "cflsp:<<taginfo?>>"
                }
            });
        }
        return [];
    }

    if (isCfScriptTagBlock(node)) {
        let justCfScriptCompletion = false;
        // if we got </cf then we are in an unfinished tag node
        if (node.parent?.kind === NodeKind.tag && node.parent?.which === CfTag.Which.end) {
            justCfScriptCompletion = true;
        }
        // if we got got an identifier but the previous text is "</" (not valid in any expression) then just provide a cfscript completion
        else if (node.parent?.kind === NodeKind.identifier && node.range.fromInclusive >= 2) {
            const text = parsedSourceFile.scanner.getSourceText();
            if (text[node.range.fromInclusive-2] === "<" && text[node.range.fromInclusive-1] === "/") {
                justCfScriptCompletion = true;
            }
        }

        if (justCfScriptCompletion) {
            return [{
                label: "cfscript",
                kind: CompletionItemKind.tagName,
                detail: "cflsp:<<taginfo?>>",
                insertText: "cfscript>",
            }];
        }
    }
    
    const result : CompletionItem[] = [];

    // `foo(bar = baz, |)`
    // `foo(b|`
    // `foo( |)`
    // NOT `foo(bar = |`, that should be an expression completion
    if (callExpr) {
        const sig = checker.getCachedEvaluatedNodeType(callExpr.left, parsedSourceFile);
        if (!sig || !isFunctionSignature(sig)) return [];

        const yetToBeUsedParams = new Set<string>(sig.params.map(param => param.canonicalName));
        for (const arg of callExpr.args) {
            const argName = stringifyCallExprArgName(arg);
            if (argName) yetToBeUsedParams.delete(argName.canonical);
        }

        const detail = callExpr.parent?.kind === NodeKind.new ? "named constructor argument" : "named function argument";

        for (const param of sig.params) {
            if (!yetToBeUsedParams.has(param.canonicalName)) continue;
            result.push({
                label: param.uiName + "=",
                kind: CompletionItemKind.variable,
                detail: detail,
                sortText: "000_" + param.uiName, // we'd like param name suggestions first
            });
        }
    }

    if (node.parent?.kind === NodeKind.indexedAccessChainElement) {
        // get the type one level before the current
        // x.y| -- we want the type of `x` for completions, not `y`

        // fixme: unify "type" vs. "SymbolTable" disparity, we have symbol tables pretending to be structs for typechecking purposes but is that necessary? or can everything be a struct or ... ?
        let typeinfo : _Type | SymbolTable | undefined = project.__unsafe_dev_getChecker().getCachedEvaluatedNodeType(node.parent.parent, parsedSourceFile);
        
        // the first symbol table we get is a cfStruct or null; after that, we will start getting actual symbol tables
        // we can check the first cfstruct we get for "cfc-ness"
        const forCfcCompletions = parsedSourceFile.cfFileType === CfFileType.cfc;
        const sourceFileIsCfcDescendantOfSymbolTableFile = typeinfo.cfc ? cfcIsDescendantOf(typeinfo.cfc, parsedSourceFile) : false;

        let workingSourceFile = typeinfo.cfc ?? parsedSourceFile;
        const result : CompletionItem[] = [];

        while (typeinfo) {
            let underlyingMembers : ReadonlyMap<string, SymTabEntry>;
            if (typeinfo instanceof Map) {
                underlyingMembers = typeinfo;
            }
            else if (isStruct(typeinfo)) {
                underlyingMembers = typeinfo.members;
            }
            else {
                break; // unreachable
            }

            for (const symTabEntry of underlyingMembers.values()) {
                if (symTabEntry.canonicalName === "init" && forCfcCompletions) { continue };
                if (forCfcCompletions && isFunctionSignature(symTabEntry.type) && !isPublicMethod(symTabEntry.type) && !sourceFileIsCfcDescendantOfSymbolTableFile) { continue; }
                result.push({
                    label: symTabEntry.uiName,
                    kind: isFunctionSignature(symTabEntry.type)
                        ? CompletionItemKind.function
                        : CompletionItemKind.structMember,
                    detail: ""
                })
            }

            // if our first symbol was for a cfc exported/public symbol, then we can climb the hierarchy and offer public members of parent components, too
            if (forCfcCompletions && workingSourceFile.cfc?.extends) {
                workingSourceFile = workingSourceFile.cfc.extends;
                typeinfo = workingSourceFile.containedScope.this; // the first typeinfo was guaranteed to be a `cfStruct`; from here and for every subsequent iteration, we get a SymbolTable instaed
            }
            else {
                break;
            }
        }

        return result;
    }

    // things that are always visible, but with low priority in relation to the user's identifiers
    // probably better to bind these as identifiers in the appropriate symbol tables
    if (getSourceFile(node)?.cfFileType === CfFileType.cfc) {
        result.push({label: "this", kind: CompletionItemKind.variable, sortText: "z"});
    }
    result.push({label: "variables", kind: CompletionItemKind.variable, sortText: "z"});
    result.push({label: "url", kind: CompletionItemKind.variable, sortText: "z"});
    result.push({label: "request", kind: CompletionItemKind.variable, sortText: "z"});
    result.push({label: "cgi", kind: CompletionItemKind.variable, sortText: "z"});

    // we're in a primary expression context, where we need to do symbol lookup in all visible scopes
    // e.g,. `x = | + y`
    const allVisibleNames = (function (node: Node | null) {
        const result = new Map<string, [StaticallyKnownScopeName, CompletionItemKind, number]>();
        let scopeDistance = 0; // keep track of "how far away" some name is, in terms of parent scopes; we can then offer closer names first
        while (node) {
            if (node.containedScope) {
                for (const searchScope of ["local", "arguments", "variables"] as StaticallyKnownScopeName[]) {
                    const symTab = node.containedScope[searchScope];
                    if (!symTab) continue;
                    for (const symTabEntry of symTab.values()) {
                        const completionKind = isFunctionSignature(symTabEntry.type)
                            ? CompletionItemKind.function
                            : CompletionItemKind.variable;
                        result.set(symTabEntry.uiName, [searchScope, completionKind, scopeDistance]);
                    }
                }
                scopeDistance++;
            }

            // if we're at the root of a sourcefile, try to climb into the parent CFC if it exists
            // otherwise, climb to ancestor container or direct parent node
            node = (node.kind === NodeKind.sourceFile && node.cfc?.extends)
                ? node.cfc.extends
                : node.containedScope?.container || node.parent;
        }
        return result;
    })(node);

    for (const [varName, [_, completionKind, scopeDistance]] of allVisibleNames) { 
        result.push({
            label: varName,
            kind: completionKind,
            // sort the first two scopes to the top of the list; the rest get lexically sorted as one agglomerated scope
            sortText: (scopeDistance === 0 ? 'a' : scopeDistance === 1 ? 'b' : 'c') + scopeDistance
        });
    }

    return result;
}