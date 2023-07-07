/**
 * Maps our internal representation of text and diagnostic ranges and etc. to some client's expecations about
 * how those things should be represented
 * 
 * this is a "module interface"
 *   - unenforceable but required:
 *     an "implementing module" must export the single constant value `adapter : ClientAdapter`
 *     it is then `require("path-to-impl").adapter`'d where it is needed
 */

import { SourceRange } from "../compiler/scanner"
import { Diagnostic } from "../compiler/node";
import { CompletionItem } from "../services/completions";
import { AbsPath } from "../compiler/utils";

export type PosMapper<T> = (pos: number) => T

export interface ClientAdapter<T> {
    diagnostic: (posMapper: PosMapper<T>, diagnostic: Diagnostic) => unknown
    completionItem: (posMapper: PosMapper<T>, completionItem: CompletionItem) => unknown
    sourceLocation: (posMapper: PosMapper<T>, fsPath: AbsPath, sourceRange: SourceRange) => unknown
}
