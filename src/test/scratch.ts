import { Scanner, TokenizerMode, Parser } from "../compiler";
import { CfFileType } from "../compiler/parser";

//const scanner = Scanner(`<cfset x = function foo(a, b = 42 & 0){}>`);
const scanner = Scanner(`
<cfcomponent>
    <cfscript>
        final var d = x:1;
    </cfscript>
</cfcomponent>
`);

const parser = Parser()
    .setScanner(scanner)
    .setDebug(true);

parser.parse(CfFileType.cfc);
    
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}
    