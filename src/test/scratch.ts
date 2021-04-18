
import { parse } from "node:path";
import { Scanner, Tokenizer, Parser } from "../compiler";

const scanner = Scanner(`<cf! name="#x#">
<cfargument name="foo" required=#x#>
<cfargument name="bar" required=#y#>
<cfargument name="baz" required=#z#>
<cfset v = (x + ) => 4>
<cfif x IS 4>
    <cfoutput>
        <cfif X EQ 4>
        </cfif>
    </cfoutPut>
</cfif>
</cffunction>`);

const tokenizer = new Tokenizer(scanner);
const parser = Parser(tokenizer);
parser.parseTags();
for (const diag of parser.getDiagnostics()) {
    console.log(diag);
}
    