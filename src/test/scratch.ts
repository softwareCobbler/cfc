
import { parse } from "node:path";
import { Scanner, Tokenizer, Parser } from "../compiler";

const scanner = Scanner(`
<cfoutput>
    #x + y + z#
</cfoutput>

<cffunction name="UH">
    <cfargument x>
</cffunction>
`);

const tokenizer = new Tokenizer(scanner);
const parser = Parser(tokenizer);
parser.parseTags();
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}
    