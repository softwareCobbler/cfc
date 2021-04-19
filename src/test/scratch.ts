import { Scanner, Tokenizer, TokenizerMode, Parser } from "../compiler";

//const scanner = Scanner(`<cfset x = function foo(a, b = 42 & 0){}>`);
const scanner = Scanner(`
<cfset f = function() {
    if (x == 4 || x == 5) {}
}>`);

const tokenizer = new Tokenizer(scanner);
const parser = Parser()
    .setTokenizer(tokenizer, TokenizerMode.tag)
    .setDebug(true);

parser.parseTags();
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}
    