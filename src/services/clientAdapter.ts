/**
 * Maps our internal representation of text and diagnostic ranges and etc. to some client's expecations about
 * how those things should be represented
 * 
 * this is a "module interface"
 *   - unenforceable but required:
 *     an "implementing module" must export the single constant value `adapter : ClientAdapter`
 *     it is then `require("path-to-impl").adapter`'d where it is needed
 *
 * So basically instead of "module.default" we look for "module.adapter"
 */

import { AnnotatedChar, SourceRange } from "../compiler/scanner"
import { Diagnostic } from "../compiler/node";
import { CompletionItem } from "../services/completions";
import { AbsPath } from "../compiler/utils";

export type AnnotatedCharGetter = (pos: number) => AnnotatedChar

export interface ClientAdapter {
    diagnostic: (getAnnotatedChar: AnnotatedCharGetter, diagnostic: Diagnostic) => unknown
    completionItem: (getAnnotatedChar: AnnotatedCharGetter, completionItem: CompletionItem) => unknown
    sourceLocation: (getAnnotatedChar: AnnotatedCharGetter, fsPath: AbsPath, sourceRange: SourceRange) => unknown
}

export function asClientAdapter<T extends ClientAdapter>(v: T) {
    return v;
}

/**
 * Identity adapter is for use in debug scenarios where we don't need an adapter
 */
export const identityAdapter = asClientAdapter(Object.freeze({
    diagnostic: (_getAnnotatedChar: AnnotatedCharGetter, diagnostic: Diagnostic) => diagnostic,
    completionItem: (_getAnnotatedChar: AnnotatedCharGetter, completionItem: CompletionItem) => completionItem,
    sourceLocation: (_getAnnotatedChar: AnnotatedCharGetter, _fsPath: AbsPath, sourceRange: SourceRange) => sourceRange
}));

/**
 * "adapter" is to conform with module interface requirements, this exports the identity adapter
 */
export const adapter = identityAdapter;
