import { Scanner, Tokenizer, TokenizerMode, Parser } from "../compiler";

//const scanner = Scanner(`<cfset x = function foo(a, b = 42 & 0){}>`);
const scanner = Scanner(`
<cfscript>
    var x = {a.b.c: 1, #b()#: 2,};
</cfscript>
`);

const tokenizer = new Tokenizer(scanner);
const parser = Parser()
    .setTokenizer(tokenizer, TokenizerMode.tag)
    .setDebug(true);

parser.parseTags();
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}
    