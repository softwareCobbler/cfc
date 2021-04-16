
import { parse } from "node:path";
import { Scanner, Tokenizer, Parser } from "../compiler";

const scanner = Scanner(`
<cffunction name="m">
    <cfargument name="x" required=#true#>
    <cfoutput>#x + </cfoutput>
</cffunction>
`);

const tokenizer = new Tokenizer(scanner);
const parser = Parser(tokenizer);
parser.parseTags();
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}
    