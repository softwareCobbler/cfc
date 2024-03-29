// fixme - use non-relative paths, which requires we get ts-node to resolve the paths during testing
// we can get it to compile with tsc with non-relative paths, but loading it during testing does not work
import { Project } from "../compiler/project"
import { Node, NodeKind, CallExpression, CfTag, StaticallyKnownScopeName, SymbolTable, SymTabEntry, SimpleStringLiteral, SourceFile } from "../compiler/node"
import { CfFileType, SourceRange, TokenType } from "../compiler/scanner";
import { cfFunctionSignatureParam, SymbolTableTypeWrapper, TypeFlags, Type, TypeKind, BuiltinType, isStructLikeOrArray, cfFunctionSignature } from "../compiler/types";
import { isExpressionContext, isCfScriptTagBlock, stringifyCallExprArgName, getSourceFile, cfcIsDescendantOf, isPublicMethod, getTriviallyComputableString } from "../compiler/utils";
import { Checker } from "../compiler/checker";

export const enum CompletionItemKind {
    tagName,
    function,
    variable,
    structMember,
    stringLiteral
}

interface InsertReplaceEdit {
    newText: string;
    /**
     * The range if the insert is requested
     */
    insert: SourceRange;
    /**
     * The range if the replace is requested.
     */
    replace: SourceRange;
    range: SourceRange;
}

export interface CompletionItem {
    label: string,
    kind: CompletionItemKind,
    detail?: string,
    insertText?: string,
    sortText?: string,
    textEdit?: InsertReplaceEdit
}

function getCallExprArgIndex(callExpr: CallExpression, node: Node) {
    const index = callExpr.args.findIndex((v) => v.expr === node);
    return index === -1 ? undefined : index;
}

function getParamTypeByName(signature: cfFunctionSignature, canonicalArgName: string) : Type | undefined {
    for (const param of signature.params) {
        if (param.canonicalName === canonicalArgName) {
            return param.paramType;
        }
    }
    return undefined;
}

function extractTopLevelStringsFromType(type: Type) : string[] {
    switch (type.kind) {
        case TypeKind.union: {
            return type.types.map(extractTopLevelStringsFromType).flat();
        }
        case TypeKind.literal: {
            if (typeof type.literalValue === "string") {
                return [type.literalValue];
            }
            return [];
        }
        default: {
            return [];
        }
    }
}

function getStringLiteralCompletions(checker: Checker, sourceFile: SourceFile, node: SimpleStringLiteral) : CompletionItem[] | undefined {
    const strings = new Set<string>();

    if (node.parent?.kind === NodeKind.callArgument && node.parent.parent?.kind === NodeKind.callExpression) {
        if (node.parent.name) {
            const canonicalParamName = getTriviallyComputableString(node.parent.name)?.toLowerCase();
            if (!canonicalParamName) return [];
            const calledSignature = checker.getCachedEvaluatedNodeType(node.parent.parent.left, sourceFile);
            if (!calledSignature) return undefined;

            if (calledSignature.kind === TypeKind.functionOverloadSet) { // this was primarily to investigate wirebox typename completions in `getInstance`
                // for (const overload of type.overloads) {
                //     const type = overload.params.find(param => param.canonicalName === paramName)?.paramType;
                //     if (!type) continue;
                //     if (type.kind === TypeKind.literal && type.underlyingType === BuiltinType.string) {
                //         strings.add(type.literalValue as string);
                //     }
                // }
                return undefined;
            }
            else if (calledSignature.kind === TypeKind.functionSignature) {
                const paramType = getParamTypeByName(calledSignature, canonicalParamName);
                if (!paramType) {
                    return undefined;
                }
                return extractTopLevelStringsFromType(paramType).map((s) => ({label: s, kind: CompletionItemKind.stringLiteral}));
            }
            else {
                // no-op -- ?
            }
        }
        else {
            const ziArgIndex = getCallExprArgIndex(node.parent.parent, node);
            if (ziArgIndex === undefined) return undefined;
            // const symbol = checker.getSymbol(node.parent.parent.left, sourceFile);
            // if (!symbol) return undefined;
            const type = checker.getCachedEvaluatedNodeType(node.parent.parent.left, sourceFile);
            if (!type) return undefined;

            if (type.kind === TypeKind.functionOverloadSet) { // this was primarily to investigate wirebox typename completions in `getInstance`
                // for (const overload of type.overloads) {
                //     const type = overload.params[ziArgIndex]?.paramType;
                //     if (!type) continue;
                //     if (type.kind === TypeKind.literal && type.underlyingType === BuiltinType.string) {
                //         strings.add(type.literalValue as string);
                //     }
                // }
            }
            else if (type.kind === TypeKind.functionSignature) {
                return undefined; // not yet impl'd
            }
            else if (type.kind === TypeKind.genericFunctionSignature) {
                const argType = type.params[ziArgIndex]?.paramType;
                if (!argType || argType.kind !== TypeKind.typeId) {
                    return undefined;
                }
                const typeParam = type.typeParams.find(typeParam => typeParam.name === argType.name);
                if (!typeParam) {
                    return undefined;
                }
                if (typeParam.extends?.kind === TypeKind.keyof && typeParam.extends.concrete) {
                    for (const keyName of typeParam.extends.keyNames) {
                        strings.add(keyName);
                    }
                }

                // no-op -- ?
            }
        }
    }
    else if (node.parent?.kind === NodeKind.tagAttribute) {
        const attrType = checker.getCachedEvaluatedNodeType(node.parent, sourceFile);
        if (attrType.kind === TypeKind.union) {
            
            for (const member of attrType.types) {
                if (member.kind === TypeKind.literal && member.underlyingType === BuiltinType.string) {
                    strings.add(member.literalValue as string);
                }
                if (member === BuiltinType.boolean) {
                    strings.add("yes");
                    strings.add("no");
                }
            }
        }
    }

    if (strings.size === 0) {
        return undefined;
    }
    else {
        const result : CompletionItem[] = [];
        strings.forEach((s) => {
            //const v = new SourceRange(node.range.fromInclusive+1, node.range.fromInclusive + s.length);
            result.push({
                label: s,
                kind: CompletionItemKind.stringLiteral,
                // textEdit: {
                //     newText: s,
                //     range: v,
                //     insert: v,
                //     replace: v
                // }
            });
        });

        return result.sort((l,r) => l.label < r.label ? -1 : l.label === r.label ? 0 : 1);
    }
}

export function getCompletions(project: Project, fsPath: string, targetIndex: number, triggerCharacter: string | null) : CompletionItem[] {
    if (!project) return [];

    const parsedSourceFile = project.getParsedSourceFile(fsPath);
    const checker = project.__unsafe_dev_getChecker();
    const node = project.getNodeToLeftOfCursor_forCompletion(fsPath, targetIndex); // fixme: probably generally want "getInterestingNodeLeftOfCursor" to not grab terminals, but all the ".parent.parent..." chains would have to be fixed up

    if (!parsedSourceFile || !node) return [];

    // check that targetIndex is inside the range of the stringLiteral, because trailing trivia is bound to the string literal
    // So, `"abcdefg" <text-span of whitespace here>`
    // the text-span's parent is the string literal, but we obviously don't want to offer completions from that position
    if ((node.kind === NodeKind.terminal || node.kind === NodeKind.textSpan) && node.parent?.kind === NodeKind.simpleStringLiteral && node.parent.range.includes(targetIndex)) {
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

    const expressionContext = isExpressionContext(node);

    if (!expressionContext) {
        const maybeTagCall = node.parent?.kind === NodeKind.terminal && node.parent.parent?.kind === NodeKind.tag
            ? node.parent.parent
            : node.parent?.kind === NodeKind.tagAttribute && node.parent.parent?.kind === NodeKind.tag
            ? node.parent.parent
            : node.parent?.kind === NodeKind.terminal && node.parent.parent?.kind === NodeKind.tagAttribute && node.parent.parent.parent?.kind === NodeKind.tag
            ? node.parent.parent.parent
            : node.parent?.kind === NodeKind.terminal && node.parent.parent?.parent?.kind === NodeKind.tagAttribute && node.parent.parent.parent.parent?.kind === NodeKind.tag
            ? node.parent.parent.parent.parent
            : null;

        if (!maybeTagCall) {
            return [];
        }

        const sig = checker.getCachedEvaluatedNodeType(maybeTagCall, parsedSourceFile);
        if (sig.kind !== TypeKind.functionSignature) {
            return [];
        }

        return namedCallArgumentCompletions(sig.params, new Set(sig.params.map(param => param.canonicalName)), "Tag attribute");
    }

    const callExpr : CallExpression | null = (node.parent?.parent?.kind === NodeKind.callArgument && !node.parent.parent.equals)
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

    // fixme: hoist out
    function namedCallArgumentCompletions(params: readonly cfFunctionSignatureParam[], yetToBeUsedParams: ReadonlySet<string>, detail: string) : CompletionItem[] {
        const result : CompletionItem[] = [];
        for (const param of params) {
            if (!yetToBeUsedParams.has(param.canonicalName)) continue;
            if (!param.uiName) continue;
            if (param.flags & TypeFlags.spread) continue; // don't show a name for a spread arg
            result.push({
                label: param.uiName + "=",
                kind: CompletionItemKind.variable,
                detail: detail,
                sortText: "000_" + param.uiName, // we'd like param name suggestions first
            });
        }
        return result;
    }

    // `foo(bar = baz, |)`
    // `foo(b|`
    // `foo( |)`
    // NOT `foo(bar = |`, that should be an expression completion
    if (callExpr) {
        const sig = checker.getCachedEvaluatedNodeType(callExpr.left, parsedSourceFile);
        if (sig.kind === TypeKind.functionOverloadSet) return [];
        if (sig.kind === TypeKind.functionSignature) {
            const yetToBeUsedParams = new Set<string>(sig.params.map(param => param.canonicalName));
            for (const arg of callExpr.args) {
                const argName = stringifyCallExprArgName(arg);
                if (argName) yetToBeUsedParams.delete(argName.canonical);
            }

            const detail = callExpr.parent?.kind === NodeKind.new ? "named constructor argument" : "named function argument";
            result.push(...namedCallArgumentCompletions(sig.params, yetToBeUsedParams, detail));
        }
    }

    if (node.kind === NodeKind.indexedAccessChainElement || node.parent?.kind === NodeKind.indexedAccessChainElement) {
        // get the type one level before the current
        // x.y|  --> for x
        // x.|   --> for x
        // x.y.| --> for x.y
        const targetIndexedAccessNode = (() => {
            if (node.kind === NodeKind.indexedAccessChainElement) {
                return node.parent;
            }
            else if (node.parent?.kind === NodeKind.indexedAccessChainElement) {
                return node.parent.parent;
            }
            else {
                throw "unreachable";
            }
        })();

        // fixme: unify "type" vs. "SymbolTable" disparity, we have symbol tables pretending to be structs for typechecking purposes but is that necessary? or can everything be a struct or ... ?
        let typeinfo : Type | SymbolTable | undefined = project.__unsafe_dev_getChecker().getCachedEvaluatedNodeType(targetIndexedAccessNode, parsedSourceFile);
        
        // the first symbol table we get is a cfStruct or null; after that, we will start getting actual symbol tables
        let parsedSourceFileIsDescendantOfTypeinfoCfc : boolean;
        let workingSourceFile : SourceFile;

        if (typeinfo.kind === TypeKind.cfc) {
            parsedSourceFileIsDescendantOfTypeinfoCfc = cfcIsDescendantOf(typeinfo.cfc, parsedSourceFile);
            workingSourceFile = typeinfo.cfc;
        }
        else {
            parsedSourceFileIsDescendantOfTypeinfoCfc = false;
            workingSourceFile = parsedSourceFile;
        }

        const result : CompletionItem[] = [];

        while (typeinfo) {
            let underlyingMembers : ReadonlyMap<string, SymTabEntry>;
            let interfaceExtension : ReadonlyMap<string, SymTabEntry> | undefined = undefined;
            let currentStructLikeIsCfc = false;

            if (typeinfo instanceof Map) {
                currentStructLikeIsCfc = true; // we got a symboltable, which we only get in cases of climbing into a parent CFC
                underlyingMembers = typeinfo;
                if (workingSourceFile.cfFileType === CfFileType.cfc && workingSourceFile.containedScope.typeinfo.mergedInterfaces.has("this")) {
                    interfaceExtension = workingSourceFile.containedScope.typeinfo.mergedInterfaces.get("this")!.members;
                }
            }
            else if (isStructLikeOrArray(typeinfo)) {
                underlyingMembers = typeinfo.members;
                currentStructLikeIsCfc = typeinfo.kind === TypeKind.cfc;
                if (typeinfo.kind === TypeKind.symbolTableTypeWrapper) interfaceExtension = (typeinfo as SymbolTableTypeWrapper).interfaceExtension?.members;
            }
            else {
                break; // unreachable
            }

            const runOne = (symTabEntry: SymTabEntry) => {
                if (symTabEntry.canonicalName === "init" && currentStructLikeIsCfc) return; // don't need to show init, we can still check its signature though?
                if ( currentStructLikeIsCfc && symTabEntry.firstLexicalType?.kind === TypeKind.functionSignature && !isPublicMethod(symTabEntry.firstLexicalType)) {
                    if (!parsedSourceFileIsDescendantOfTypeinfoCfc) return; // don't offer completions for non-public members for non-descendants
                }
                const effectiveType = symTabEntry.links?.effectiveDeclaredType ?? symTabEntry.firstLexicalType;
                result.push({
                    label: symTabEntry.uiName,
                    kind: effectiveType?.kind === TypeKind.functionSignature || effectiveType?.kind === TypeKind.genericFunctionSignature
                        ? CompletionItemKind.function
                        : CompletionItemKind.structMember,
                    detail: ""
                })
            }
            
            underlyingMembers.forEach(runOne);
            interfaceExtension?.forEach(runOne);

            // climb the hierarchy and offer public members of parent components, too
            if (currentStructLikeIsCfc && workingSourceFile.cfc?.extends) {
                workingSourceFile = workingSourceFile.cfc.extends;
                typeinfo = workingSourceFile.containedScope.this; // the first typeinfo was guaranteed to be a Struct; from here and for every subsequent iteration, we get a SymbolTable instaed
            }
            else {
                break;
            }
        }

        return result;
    }

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
                    
                    const iterableSymTabEntries = [...symTab.values()];
                    if (node.containedScope.typeinfo.mergedInterfaces.has(searchScope)) {
                        iterableSymTabEntries.push(...node.containedScope.typeinfo.mergedInterfaces.get(searchScope)!.members.values());
                    }

                    for (const symTabEntry of iterableSymTabEntries) {
                        const completionKind = symTabEntry.firstLexicalType?.kind === TypeKind.functionSignature
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
                : node.containedScope?.parentContainer || node.parent;
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