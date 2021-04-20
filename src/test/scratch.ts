import { Scanner, Tokenizer, TokenizerMode, Parser } from "../compiler";

//const scanner = Scanner(`<cfset x = function foo(a, b = 42 & 0){}>`);
const scanner = Scanner(`
<cffunction name="x">
    <cfoutput>
    <cfset x = function() {
        do {
            while (x ?: 4) {
                if (x == 4 && x is 4) {
                    if (x + 4) {

                    }
                }
            }
            m = 4 ?: 3;
        } while (false);
    }>
    </cfoutput>
    <cfscript>
    
</cffunction>`);

const tokenizer = new Tokenizer(scanner);
const parser = Parser()
    .setTokenizer(tokenizer, TokenizerMode.tag)
    .setDebug(true);

parser.parseTags();
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}
    