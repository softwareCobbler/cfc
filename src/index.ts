namespace cf {

const scanner = new Scanner("x<cfif x EQ 4>")
const tokenizer = new Tokenizer(scanner);
const parser = new Parser(tokenizer);
parser.parseTags();

}