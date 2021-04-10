namespace cf {
export const enum TokenType {
    NIL,
    EOF, // like EOF but the EOF macro is reserved
    BOM_UTF_8, // ef bb ef

    WHITESPACE,
    LEXEME,
    NUMBER,

    AMPERSAND,
    AMPERSAND_EQUAL,
    CARET,
    COMMA,
    DBL_AMPERSAND,
    DBL_EQUAL,
    DBL_FORWARD_SLASH,
    DBL_MINUS,
    DBL_PIPE,
    DBL_PLUS,
    DOT,
    EQUAL,
    EQUAL_RIGHT_ANGLE,
    EXCLAMATION,
    EXCLAMATION_EQUAL,
    FORWARD_SLASH,
    FORWARD_SLASH_EQUAL,
    FORWARD_SLASH_STAR,
    HASH,
    LEFT_ANGLE,
    LEFT_ANGLE_EQUAL,
    LEFT_BRACE,
    LEFT_BRACKET,
    LEFT_PAREN,
    MINUS,
    MINUS_EQUAL,
    PERCENT,
    PIPE,
    PLUS,
    PLUS_EQUAL,
    QUOTE_SINGLE,
    QUOTE_DOUBLE,
    RIGHT_ANGLE,
    RIGHT_ANGLE_EQUAL,
    RIGHT_BRACE,
    RIGHT_BRACKET,
    RIGHT_PAREN,
    STAR,
    STAR_EQUAL,

    CF_START_TAG_START, // <\s*cf
    CF_END_TAG_START,   // <\s*/\s*cf
    CF_TAG_END_VOID,    // /\s*>
    CF_TAG_COMMENT_START,
    CF_TAG_COMMENT_END,

    LIT_AND,
    LIT_CONTAINS,
    LIT_DOES_NOT_CONTAIN,
    LIT_EQ,
    LIT_GT,
    LIT_GTE,
    LIT_GE,
    LIT_INCLUDES,
    LIT_IS,
    LIT_IS_NOT,
    LIT_LT,
    LIT_LE,
    LIT_LTE,
    LIT_MOD,
    LIT_NEQ,
    LIT_XOR,

    KW_BREAK,
    KW_CASE,
    KW_CATCH,
    KW_DEFAULT,
    KW_DO,
    KW_ELSE,
    KW_FINAL,
    KW_FOR,
    KW_FUNCTION,
    KW_IF,
    KW_IMPORT,
    KW_NEW,
    KW_RETURN,
    KW_TRY,
    KW_VAR,
    KW_WHILE,
};

export const TokenTypeUiString : Record<TokenType, string> = {
    [TokenType.NIL]:                  "nil",
    [TokenType.EOF]:                  "eof",
    [TokenType.BOM_UTF_8]:            "utf8-bom",

    [TokenType.WHITESPACE]:           "<whitespace>",
    [TokenType.LEXEME]:               "<lexeme>",
    [TokenType.NUMBER]:               "<number>",

    [TokenType.AMPERSAND]:            "&",
    [TokenType.AMPERSAND_EQUAL]:      "&=",
    [TokenType.CARET]:                "^",
    [TokenType.COMMA]:                ",",
    [TokenType.DBL_AMPERSAND]:        "&&",
    [TokenType.DBL_EQUAL]:            "==",
    [TokenType.DBL_FORWARD_SLASH]:    "//",
    [TokenType.DBL_MINUS]:            "--",
    [TokenType.DBL_PIPE]:             "||",
    [TokenType.DBL_PLUS]:             "++",
    [TokenType.DOT]:                  ".",
    [TokenType.EQUAL]:                "=",
    [TokenType.EQUAL_RIGHT_ANGLE]:    "=>",
    [TokenType.EXCLAMATION]:          "!",
    [TokenType.EXCLAMATION_EQUAL]:    "!=",
    [TokenType.FORWARD_SLASH]:        "/",
    [TokenType.FORWARD_SLASH_EQUAL]:  "/=",
    [TokenType.FORWARD_SLASH_STAR]:   "/*",
    [TokenType.HASH]:                 "#",
    [TokenType.LEFT_ANGLE]:           "<",
    [TokenType.LEFT_ANGLE_EQUAL]:     "<=",
    [TokenType.LEFT_BRACE]:           "{",
    [TokenType.LEFT_BRACKET]:         "[",
    [TokenType.LEFT_PAREN]:           "(",
    [TokenType.MINUS]:                "-",
    [TokenType.MINUS_EQUAL]:          "-=",
    [TokenType.PERCENT]:              "%",
    [TokenType.PIPE]:                 "|",
    [TokenType.PLUS]:                 "+",
    [TokenType.PLUS_EQUAL]:           "+=",
    [TokenType.QUOTE_SINGLE]:         "'",
    [TokenType.QUOTE_DOUBLE]:         "\"",
    [TokenType.RIGHT_ANGLE]:          ">",
    [TokenType.RIGHT_ANGLE_EQUAL]:    ">=",
    [TokenType.RIGHT_BRACE]:          "}",
    [TokenType.RIGHT_BRACKET]:        "]",
    [TokenType.RIGHT_PAREN]:          ")",
    [TokenType.STAR]:                 "*",
    [TokenType.STAR_EQUAL]:           "*=",

    [TokenType.CF_START_TAG_START]:   "<cf",
    [TokenType.CF_END_TAG_START]:     "</cf",
    [TokenType.CF_TAG_END_VOID]:      "/>",
    [TokenType.CF_TAG_COMMENT_START]: "<!---",
    [TokenType.CF_TAG_COMMENT_END]:   "--->",

    [TokenType.LIT_AND]:              "and",
    [TokenType.LIT_CONTAINS]:         "contains",
    [TokenType.LIT_DOES_NOT_CONTAIN]: "does not contain",
    [TokenType.LIT_EQ]:               "eq",
    [TokenType.LIT_GT]:               "gt",
    [TokenType.LIT_GTE]:              "gte",
    [TokenType.LIT_GE]:               "ge",
    [TokenType.LIT_INCLUDES]:         "includes",
    [TokenType.LIT_IS]:               "is",
    [TokenType.LIT_IS_NOT]:           "is not",
    [TokenType.LIT_LT]:               "lt",
    [TokenType.LIT_LE]:               "le",
    [TokenType.LIT_LTE]:              "lte",
    [TokenType.LIT_MOD]:              "mod",
    [TokenType.LIT_NEQ]:              "neq",
    [TokenType.LIT_XOR]:              "xor",

    [TokenType.KW_BREAK]:             "break",
    [TokenType.KW_CASE]:              "case",
    [TokenType.KW_CATCH]:             "catch",
    [TokenType.KW_DEFAULT]:           "default",
    [TokenType.KW_DO]:                "do",
    [TokenType.KW_ELSE]:              "else",
    [TokenType.KW_FINAL]:             "final",
    [TokenType.KW_FOR]:               "for",
    [TokenType.KW_FUNCTION]:          "function",
    [TokenType.KW_IF]:                "if",
    [TokenType.KW_IMPORT]:            "import",
    [TokenType.KW_NEW]:               "new",
    [TokenType.KW_RETURN]:            "return",
    [TokenType.KW_TRY]:               "try",
    [TokenType.KW_VAR]:               "var",
    [TokenType.KW_WHILE]:             "while",
} as const;

class SourceRange {
    fromInclusive: number;
    toExclusive: number;

    constructor(fromInclusive: number, toExclusive: number) {
        this.fromInclusive = fromInclusive;
        this.toExclusive = toExclusive;
    }

    static Nil() {
        return new SourceRange(-1, -1);
    }

    isNil() {
        return this.fromInclusive == -1 && this.toExclusive == -1;
    }
}

class Token {
    type: TokenType;
    range: SourceRange;

    constructor(type: TokenType, fromInclusive: number, toExclusive: number);
    constructor(type: TokenType, range: SourceRange);
    constructor(type: TokenType, fromOrRange: number | SourceRange, toExclusive?: number) {
        this.type = type;
        if (typeof fromOrRange === "number") {
            this.range = new SourceRange(fromOrRange, toExclusive!);
        }
        else {
            this.range = fromOrRange;
        }
    }

    static Nil() {
        return new Token(TokenType.NIL, SourceRange.Nil());
    }
}

export const enum TokenizerMode { tag, script };

export class Tokenizer {
    private scanner_ : Scanner;
    private token_ : Token = Token.Nil();
    constructor(scanner: Scanner) {
        this.scanner_ = scanner;
    }

    prime(mode: TokenizerMode) {
        // ?
    }

    private setToken(type: TokenType, from: number, to: number) {
        this.token_ = new Token(type, from, to);
        return this.token_;
    }

    getIndex() {
        return this.scanner_.getIndex();
    }

    consumeCurrentCharAs(type: TokenType) {
        const from = this.getIndex();
        this.scanner_.next();
        return this.setToken(type, from, this.getIndex());
    }

    private lexeme() {
        const from = this.getIndex();
        if (!this.scanner_.maybeEat(/[$_a-z][$_a-z0-9]*/iy)) throw "expected a lexeme"
        return this.setToken(TokenType.LEXEME, from, this.getIndex());
    }

    next(mode: TokenizerMode) : Token {
        if (!this.scanner_.hasNext()) {
            return this.token_;
        }

        const tag = mode == TokenizerMode.tag;
        const script = mode == TokenizerMode.script;
        const from = this.getIndex();

        const c = this.scanner_.peek()!.codepoint;
        switch (c) {
            case 0:
                return this.setToken(TokenType.EOF, from, from);
            case AsciiMap.LEFT_ANGLE:
                if (tag && this.scanner_.maybeEat(/<\s*cf/iy)) return this.setToken(TokenType.CF_START_TAG_START, from, this.getIndex());
                else if (tag && this.scanner_.maybeEat(/<\s*\/\s*cf/iy)) return this.setToken(TokenType.CF_END_TAG_START, from, this.getIndex());
                else if (tag && this.scanner_.maybeEat(/<!---/iy)) return this.setToken(TokenType.CF_TAG_COMMENT_START, from, this.getIndex());
                else if (this.scanner_.maybeEat(/<=/iy)) return this.setToken(TokenType.LEFT_ANGLE_EQUAL, from, this.getIndex());
                else return this.consumeCurrentCharAs(TokenType.LEFT_ANGLE);
            case AsciiMap.RIGHT_ANGLE:
                if (script && this.scanner_.maybeEat(/>=/iy)) return this.setToken(TokenType.RIGHT_ANGLE_EQUAL, from, this.getIndex());
                return this.consumeCurrentCharAs(TokenType.RIGHT_ANGLE);
            case AsciiMap.EQUAL:
                if (this.scanner_.maybeEat(/==/iy)) return this.setToken(TokenType.DBL_EQUAL, from, this.getIndex());
                else if (this.scanner_.maybeEat(/=>/iy)) return this.setToken(TokenType.EQUAL_RIGHT_ANGLE, from, this.getIndex());
                else return this.consumeCurrentCharAs(TokenType.EQUAL);
            case AsciiMap.HASH:          return this.consumeCurrentCharAs(TokenType.HASH);
            case AsciiMap.PERCENT:       return this.consumeCurrentCharAs(TokenType.PERCENT);
            case AsciiMap.LEFT_PAREN:    return this.consumeCurrentCharAs(TokenType.LEFT_PAREN);
            case AsciiMap.RIGHT_PAREN:   return this.consumeCurrentCharAs(TokenType.RIGHT_PAREN);
            case AsciiMap.LEFT_BRACKET:  return this.consumeCurrentCharAs(TokenType.LEFT_BRACKET);
            case AsciiMap.RIGHT_BRACKET: return this.consumeCurrentCharAs(TokenType.RIGHT_BRACKET);
            case AsciiMap.LEFT_BRACE:    return this.consumeCurrentCharAs(TokenType.LEFT_BRACE);
            case AsciiMap.RIGHT_BRACE:   return this.consumeCurrentCharAs(TokenType.RIGHT_BRACE);
            case AsciiMap.CARET:         return this.consumeCurrentCharAs(TokenType.CARET);
            case AsciiMap.QUOTE_SINGLE:  return this.consumeCurrentCharAs(TokenType.QUOTE_SINGLE);
            case AsciiMap.QUOTE_DOUBLE:  return this.consumeCurrentCharAs(TokenType.QUOTE_DOUBLE);
            case AsciiMap.COMMA:         return this.consumeCurrentCharAs(TokenType.COMMA);
            case AsciiMap.SPACE: //    [[fallthrough]];
            case AsciiMap.TAB:   // \t [[fallthrough]];
            case AsciiMap.CR:    // \r [[fallthrough]];
            case AsciiMap.LF:    // \n [[fallthrough]];
                // more of a "definite" eat here
                this.scanner_.maybeEat(/\s+/iy);
                return this.setToken(TokenType.WHITESPACE, from, this.getIndex());
            case AsciiMap.EXCLAMATION:
                if (this.scanner_.maybeEat(/!=/iy)) return this.setToken(TokenType.EXCLAMATION_EQUAL, from, this.getIndex());
                else return this.consumeCurrentCharAs(TokenType.EXCLAMATION);
            case AsciiMap.PIPE:
                if (this.scanner_.maybeEat(/\|\|/iy)) return this.setToken(TokenType.DBL_PIPE, from, this.getIndex());
                else return this.consumeCurrentCharAs(TokenType.PIPE);
            case AsciiMap.DOT:
                if (this.scanner_.maybeEat(/\\.\\d+/iy)) return this.setToken(TokenType.NUMBER, from, this.getIndex());
                return this.consumeCurrentCharAs(TokenType.DOT);
            case AsciiMap.PLUS:
                if (this.scanner_.maybeEat(/++/iy)) return this.setToken(TokenType.DBL_PLUS, from, this.getIndex());
                if (this.scanner_.maybeEat(/+=/iy)) return this.setToken(TokenType.PLUS_EQUAL, from, this.getIndex());
                else return this.consumeCurrentCharAs(TokenType.PLUS);
            case AsciiMap.MINUS:
                if (this.scanner_.maybeEat(/--->/iy)) return this.setToken(TokenType.CF_TAG_COMMENT_END, from, this.getIndex());
                if (this.scanner_.maybeEat(/--/iy)) return this.setToken(TokenType.DBL_MINUS, from, this.getIndex());
                if (this.scanner_.maybeEat(/-=/iy)) return this.setToken(TokenType.MINUS_EQUAL, from, this.getIndex());
                else return this.consumeCurrentCharAs(TokenType.MINUS);
            case AsciiMap.AMPERSAND:
                if (this.scanner_.maybeEat(/&&/iy)) return this.setToken(TokenType.DBL_AMPERSAND, from, this.getIndex());
                if (this.scanner_.maybeEat(/&=/iy)) return this.setToken(TokenType.AMPERSAND_EQUAL, from, this.getIndex());
                else return this.consumeCurrentCharAs(TokenType.AMPERSAND);
            case AsciiMap.STAR:
                if (this.scanner_.maybeEat(/\*=/iy)) return this.setToken(TokenType.STAR_EQUAL, from, this.getIndex());
                else return this.consumeCurrentCharAs(TokenType.STAR);
            case AsciiMap.FORWARD_SLASH:
                if (script && this.scanner_.maybeEat(/\/\//iy)) return this.setToken(TokenType.DBL_FORWARD_SLASH, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/\/*/iy)) return this.setToken(TokenType.FORWARD_SLASH_STAR, from, this.getIndex());
                else if (tag && this.scanner_.maybeEat(/\/\s*>/iy)) return this.setToken(TokenType.CF_TAG_END_VOID, from, this.getIndex());
                else if (this.scanner_.maybeEat(/\/=/iy)) return this.setToken(TokenType.FORWARD_SLASH_EQUAL, from, this.getIndex());
                else return this.consumeCurrentCharAs(TokenType.FORWARD_SLASH);
            case AsciiMap.a: // [[fallthrough]];
            case AsciiMap.A:
                if (this.scanner_.maybeEat(/and/iy)) return this.setToken(TokenType.LIT_AND, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.b: // [[fallthrough]];
            case AsciiMap.B:
                if (script && this.scanner_.maybeEat(/break/iy)) return this.setToken(TokenType.KW_BREAK, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.c: // [[fallthrough]];
            case AsciiMap.C:
                if (script && this.scanner_.maybeEat(/catch/iy)) return this.setToken(TokenType.KW_CATCH, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/case/iy)) return this.setToken(TokenType.KW_CASE, from, this.getIndex());
                else if (this.scanner_.maybeEat(/contains/iy)) return this.setToken(TokenType.LIT_CONTAINS, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.d: // [[fallthrough]];
            case AsciiMap.D:
                if (this.scanner_.maybeEat(/does\\s+not\\s+contain/iy)) return this.setToken(TokenType.LIT_DOES_NOT_CONTAIN, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/default/iy)) return this.setToken(TokenType.KW_DEFAULT, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/do/iy)) return this.setToken(TokenType.KW_DO, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.e: // [[fallthrough]];
            case AsciiMap.E:
                if (this.scanner_.maybeEat(/eq/iy)) return this.setToken(TokenType.LIT_EQ, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/else/iy)) return this.setToken(TokenType.KW_ELSE, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.f: // [[fallthrough]];
            case AsciiMap.F:
                if (this.scanner_.maybeEat(/final/iy)) return this.setToken(TokenType.KW_FINAL, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/for/iy)) return this.setToken(TokenType.KW_FOR, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/function/iy)) return this.setToken(TokenType.KW_FUNCTION, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.g: // [[fallthrough]];
            case AsciiMap.G:
                if (this.scanner_.maybeEat(/gte/iy)) return this.setToken(TokenType.LIT_GTE, from, this.getIndex());
                else if (this.scanner_.maybeEat(/gt/iy)) return this.setToken(TokenType.LIT_GT, from, this.getIndex());
                else if (this.scanner_.maybeEat(/ge/iy)) return this.setToken(TokenType.LIT_GE, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.i: // [[fallthrough]];
            case AsciiMap.I:
                if (script && this.scanner_.maybeEat(/if/iy)) return this.setToken(TokenType.KW_IF, from, this.getIndex());
                else if (script && this.scanner_.maybeEat(/import/iy)) return this.setToken(TokenType.KW_IMPORT, from, this.getIndex());
                else if (this.scanner_.maybeEat(/is\s+not/iy)) return this.setToken(TokenType.LIT_IS_NOT, from, this.getIndex());
                else if (this.scanner_.maybeEat(/is/iy)) return this.setToken(TokenType.LIT_IS, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.l: // [[fallthrough]];
            case AsciiMap.L:
                if (this.scanner_.maybeEat(/lte/iy)) return this.setToken(TokenType.LIT_LTE, from, this.getIndex());
                else if (this.scanner_.maybeEat(/le/iy)) return this.setToken(TokenType.LIT_LE, from, this.getIndex());
                else if (this.scanner_.maybeEat(/lt/iy)) return this.setToken(TokenType.LIT_LT, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.m: // [[fallthrough]];
            case AsciiMap.M:
                if (this.scanner_.maybeEat(/mod/iy)) return this.setToken(TokenType.LIT_MOD, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.n: // [[fallthrough]];
            case AsciiMap.N:
                if (this.scanner_.maybeEat(/neq/iy)) return this.setToken(TokenType.LIT_NEQ, from, this.getIndex());
                else if (this.scanner_.maybeEat(/new/iy)) return this.setToken(TokenType.KW_NEW, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.r: // [[fallthrough]];
            case AsciiMap.R:
                if (script && this.scanner_.maybeEat(/return/iy)) return this.setToken(TokenType.KW_RETURN, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.t: // [[fallthrough]];
            case AsciiMap.T:
                if (script && this.scanner_.maybeEat(/try/iy)) return this.setToken(TokenType.KW_TRY, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.v: // [[fallthrough]];
            case AsciiMap.V:
                if (this.scanner_.maybeEat(/var/iy)) return this.setToken(TokenType.KW_VAR, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.w: // [[fallthrough]];
            case AsciiMap.W:
                if (script && this.scanner_.maybeEat(/while/iy)) return this.setToken(TokenType.KW_WHILE, from, this.getIndex());
                else return this.lexeme();
            case AsciiMap.x: // [[fallthrough]];
            case AsciiMap.X:
                if (this.scanner_.maybeEat(/xor/iy)) return this.setToken(TokenType.LIT_XOR, from, this.getIndex());
                else return this.lexeme();
            /*case '\xEF':
                if (this.scanner_.maybeEat(/\xEF\xBB\xBF/, TokenType.BOM_UTF_8)) return token_;*/
            default:
                if (this.scanner_.maybeEat(/\d+e[+-]?\d+(\.\d+)?|\d+(\.\d+)?/iy)) return this.setToken(TokenType.NUMBER, from, this.getIndex());
                return this.lexeme();
        }
        console.log("no match");
        this.scanner_.next();
        return this.token_;
    }
}
}