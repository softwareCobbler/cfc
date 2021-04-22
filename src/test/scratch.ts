import { Scanner, TokenizerMode, Parser } from "../compiler";

//const scanner = Scanner(`<cfset x = function foo(a, b = 42 & 0){}>`);
const scanner = Scanner(`
<cfscript>
x = (function () {}).v(;
</cfscript>
`);

const parser = Parser()
    .setScanner(scanner, TokenizerMode.tag)
    .setDebug(true);

parser.parseTags();
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}
    