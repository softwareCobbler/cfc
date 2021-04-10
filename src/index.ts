namespace cf {
const scanner = new Scanner("x<cfif x EQ 4>")
const tokenizer = new Tokenizer(scanner);
let token;
while ((token = tokenizer.next(TokenizerMode.tag)).type != TokenType.EOF) {
    console.log(TokenTypeUiString[token.type]);
}

}