namespace cf {

const scanner = Scanner("<cfif x EQ 4></cfhttp>")
const tokenizer = new Tokenizer(scanner);
const parser = Parser(tokenizer);
parser.parseTags();

for (const diagnostic of parser.getDiagnostics()) {
    console.log(diagnostic);
}

console.log("fin")

}