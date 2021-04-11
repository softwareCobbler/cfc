namespace cf {

const scanner = new Scanner("<cfif x EQ 4></cfif>")
const tokenizer = new Tokenizer(scanner);
const parser = Parser(tokenizer);
parser.parseTags();

}