import { Scanner, Tokenizer, TokenizerMode, Parser } from "../compiler";

//const scanner = Scanner(`<cfset x = function foo(a, b = 42 & 0){}>`);
const scanner = Scanner(`
<cfset x = function() {
    switch (foo) {
        case x: 0;
        case y: {

        }
        depfault: {

        }
    }
}>`);

const tokenizer = new Tokenizer(scanner);
const parser = Parser()
    .setTokenizer(tokenizer, TokenizerMode.tag)
    .setDebug(true);

parser.parseTags();
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}
    