import { Diagnostic } from "../compiler/node";
import { AnnotatedChar } from "../compiler/scanner"

export type AnnotatedCharGetter = (pos: number) => AnnotatedChar

export interface ClientAdapter {
    diagnostic: (getAnnotatedChar: AnnotatedCharGetter, diagnostic: Diagnostic) => unknown
}
