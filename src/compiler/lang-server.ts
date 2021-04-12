import * as fs from "fs";
import { Scanner } from "./scanner";
import { Tokenizer } from "./tokenizer";
import { Parser, Diagnostic } from "./parser";

export function naiveGetFileDiagnostics(filename: string) : readonly Diagnostic[] {
    const sourceText = fs.readFileSync(filename, {encoding: "ascii"});
    const scanner = Scanner("<cfif x EQ 4></cfhttp>")
    const tokenizer = new Tokenizer(scanner);
    const parser = Parser(tokenizer);
    parser.parseTags();
    return parser.getDiagnostics();
}