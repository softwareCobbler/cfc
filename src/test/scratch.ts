import { Scanner, TokenizerMode, Parser } from "../compiler";
import { CfFileType } from "../compiler/parser";

//const scanner = Scanner(`<cfset x = function foo(a, b = 42 & 0){}>`);
const scanner = Scanner(`
<cfoutput>#arguments.message#</cfoutput>
`);

const parser = Parser()
    .setScanner(scanner)
    .setDebug(true);

parser.parse(CfFileType.cfc);
    
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}