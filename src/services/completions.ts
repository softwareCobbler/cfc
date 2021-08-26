// fixme - use non-relative paths, which requires we get ts-node to resolve the paths during testing
// we can get it to compile with tsc with non-relative paths, but loading it during testing does not work
import { Project } from "../compiler/project"
import { Node, NodeKind, CallExpression, CfTag, StaticallyKnownScopeName } from "../compiler/node"
import { TokenType } from "../compiler/scanner";
import { isCfc, isFunctionSignature, isStruct } from "../compiler/types";
import { isExpressionContext, isCfScriptTagBlock } from "../compiler/utils";
import { tagNames } from "./tagnames";

export const enum CompletionItemKind {
    tagName,
    function,
    variable,
    structMember,
}

export interface CompletionItem {
    label: string,
    kind: CompletionItemKind,
    detail?: string,
    insertText?: string,
    sortText?: string,
}

export function getCompletions(project: Project, fsPath: string, targetIndex: number, triggerCharacter: string | null) {
    if (!project) return [];

    const parsedSourceFile = project.getParsedSourceFile(fsPath);
    const checker = project.__unsafe_dev_getChecker();
    const node = project.getNodeToLeftOfCursor(fsPath, targetIndex);

    if (!parsedSourceFile || !node) return [];

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
        for (const arg of callExpr.args) if (arg.name?.canonicalName) yetToBeUsedParams.delete(arg.name?.canonicalName)

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

        const typeinfo = project.__unsafe_dev_getChecker().getCachedEvaluatedNodeType(node.parent.parent, parsedSourceFile);
        const result : CompletionItem[] = [];

        if (isStruct(typeinfo)) {
            for (const symTabEntry of typeinfo.members.values()) {
                if (symTabEntry.canonicalName === "init" && isCfc(typeinfo)) { continue };
                result.push({
                    label: symTabEntry.uiName,
                    kind: isFunctionSignature(symTabEntry.type)
                        ? CompletionItemKind.function
                        : CompletionItemKind.structMember,
                    detail: ""
                })
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
                    for (const symTabEntry of symTab.values()) {
                        const completionKind = isFunctionSignature(symTabEntry.type)
                            ? CompletionItemKind.function
                            : CompletionItemKind.variable;
                        result.set(symTabEntry.uiName, [searchScope, completionKind, scopeDistance]);
                    }
                }
                scopeDistance++;
            }
            node = node.containedScope?.container || node.parent;
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