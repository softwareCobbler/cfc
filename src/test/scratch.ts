
import { Scanner, Tokenizer, Parser } from "../compiler";

const scanner = Scanner(`
<cfoutput>
    #x + y#
</cfoutput>
`);
const tokenizer = new Tokenizer(scanner);
const parser = Parser(tokenizer);
parser.parseTags();
    