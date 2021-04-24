// quick scratch debug;
// throw some text into the scanner,
// set the parser to either CFM/CFC mode,
// rebuild and then run the debugger
import { Scanner, TokenizerMode, Parser } from "../compiler";
import { CfFileType } from "../compiler/parser";

//const scanner = Scanner(`<cfset x = function foo(a, b = 42 & 0){}>`);
const scanner = Scanner(`<cfset testName "">`);

const parser = Parser()
    .setScanner(scanner)
    .setDebug(true);

parser.parse(CfFileType.cfm);
    
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}