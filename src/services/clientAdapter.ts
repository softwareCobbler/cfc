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