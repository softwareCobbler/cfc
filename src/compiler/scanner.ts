export const enum AsciiMap {
    TAB = 9,
    LF = 10, // \n
    CR = 13, // \r
    SPACE = 32,
    EXCLAMATION,
    QUOTE_DOUBLE,
    HASH,
    DOLLAR,
    PERCENT,
    AMPERSAND,
    QUOTE_SINGLE,
    LEFT_PAREN,
    RIGHT_PAREN,
    STAR,
    PLUS,
    COMMA,
    MINUS,
    DOT,
    FORWARD_SLASH,
    _0,
    _1,
    _2,
    _3,
    _4,
    _5,
    _6,
    _7,
    _8,
    _9,
    COLON,
    SEMICOLON,
    LEFT_ANGLE,
    EQUAL,
    RIGHT_ANGLE,
    QUESTION_MARK,
    AT,
    A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,
    LEFT_BRACKET,
    BACK_SLASH,
    RIGHT_BRACKET,
    CARET,
    UNDERSCORE,
    BACKTICK,
    a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
    LEFT_BRACE,
    PIPE,
    RIGHT_BRACE,
    TILDE
}

export const enum TokenType {
    NIL,
    EOF,
    BOM_UTF_8, // ef bb ef

    WHITESPACE,
    LEXEME,
    NUMBER,

    AMPERSAND,
    AMPERSAND_EQUAL,
    CARET,
    COLON,
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
    QUESTION_MARK,
    QUESTION_MARK_COLON,
    QUOTE_SINGLE,
    QUOTE_DOUBLE,
    RIGHT_ANGLE,
    RIGHT_ANGLE_EQUAL,
    RIGHT_BRACE,
    RIGHT_BRACKET,
    RIGHT_PAREN,
    SEMICOLON,
    STAR,
    STAR_FORWARD_SLASH,
    STAR_EQUAL,

    CF_START_TAG_START, // <\s*cf
    CF_END_TAG_START,   // <\s*/\s*cf
    CF_TAG_END_VOID,    // /\s*>
    CF_TAG_COMMENT_START,
    CF_TAG_COMMENT_END,

    _FIRST_LIT,
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
    LIT_NOT,
    LIT_OR,
    LIT_XOR,
    _LAST_LIT,

    _FIRST_KW,
    KW_BREAK,
    KW_CASE,
    KW_CATCH,
    KW_DEFAULT,
    KW_DO,
    KW_ELSE,
    KW_FALSE,
    KW_FINAL,
    KW_FOR,
    KW_FUNCTION,
    KW_IF,
    KW_IMPORT,
    KW_NEW,
    KW_RETURN,
    KW_SWITCH,
    KW_TRUE,
    KW_TRY,
    KW_VAR,
    KW_WHILE,
    _LAST_KW,
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
    [TokenType.COLON]:                ":",
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
    [TokenType.QUESTION_MARK]:        "?",
    [TokenType.QUESTION_MARK_COLON]:  "?:",
    [TokenType.QUOTE_SINGLE]:         "'",
    [TokenType.QUOTE_DOUBLE]:         "\"",
    [TokenType.RIGHT_ANGLE]:          ">",
    [TokenType.RIGHT_ANGLE_EQUAL]:    ">=",
    [TokenType.RIGHT_BRACE]:          "}",
    [TokenType.RIGHT_BRACKET]:        "]",
    [TokenType.RIGHT_PAREN]:          ")",
    [TokenType.SEMICOLON]:            ";",
    [TokenType.STAR]:                 "*",
    [TokenType.STAR_FORWARD_SLASH]:   "*/",
    [TokenType.STAR_EQUAL]:           "*=",

    [TokenType.CF_START_TAG_START]:   "<cf",
    [TokenType.CF_END_TAG_START]:     "</cf",
    [TokenType.CF_TAG_END_VOID]:      "/>",
    [TokenType.CF_TAG_COMMENT_START]: "<!---",
    [TokenType.CF_TAG_COMMENT_END]:   "--->",

    [TokenType._FIRST_LIT]:           "<<IMMEDIATELY-BEFORE-FIRST-LITERAL>>",
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
    [TokenType.LIT_NOT]:              "not",
    [TokenType.LIT_OR]:               "or",
    [TokenType.LIT_XOR]:              "xor",
    [TokenType._LAST_LIT]:            "<<IMMEDIATELY-AFTER-LAST-LITERAL>>",

    [TokenType._FIRST_KW]:            "<<IMMEDIATELY-BEFORE-FIRST-KW>>",
    [TokenType.KW_BREAK]:             "break",
    [TokenType.KW_CASE]:              "case",
    [TokenType.KW_CATCH]:             "catch",
    [TokenType.KW_DEFAULT]:           "default",
    [TokenType.KW_DO]:                "do",
    [TokenType.KW_ELSE]:              "else",
    [TokenType.KW_FALSE]:             "false",
    [TokenType.KW_FINAL]:             "final",
    [TokenType.KW_FOR]:               "for",
    [TokenType.KW_FUNCTION]:          "function",
    [TokenType.KW_IF]:                "if",
    [TokenType.KW_IMPORT]:            "import",
    [TokenType.KW_NEW]:               "new",
    [TokenType.KW_RETURN]:            "return",
    [TokenType.KW_SWITCH]:            "switch",
    [TokenType.KW_TRUE]:              "true",
    [TokenType.KW_TRY]:               "try",
    [TokenType.KW_VAR]:               "var",
    [TokenType.KW_WHILE]:             "while",
    [TokenType._LAST_KW]:            "<<IMMEDIATELY-AFTER-LAST-KW>>",
} as const;

export interface AnnotatedChar {
    codepoint: number,
    index: number,
    col: number,
    line: number
}

//
// @fixme -- instead of a 'Nil' token; just use null; but this requires some wide reaching changes,
// and we'd rather not have to write `range!` everywhere to declare that it is non-null, so a design that guarantees a source range is not null
// from within the type system would be nice
//
export class SourceRange {
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

export function Scanner(sourceText_: string) {
    const sourceText = sourceText_;
    const annotatedChars = annotate(sourceText);
    let end = annotatedChars.length;
    let index = 0;
    let lastScannedText = "";
    let token : Token = NilToken;

    //
    // @fixme:
    // will work for ascii or utf16, but multibyte utf8 will probably break
    //
    function annotate(text: string) {
        let index = 0;
        let col = 0;
        let line = 0;
        const result : AnnotatedChar[] = [];
        for (const c of text) {
            result.push({
                codepoint: c.charCodeAt(0),
                index,
                col,
                line
            });
            if (c == '\n') {
                line += 1;
                col = 0;
            }
            else {
                col += 1;
            }
            index += 1;
        }

        // push "eof"
        result.push({
            codepoint: 0,
            index,
            col,
            line
        });

        return result;
    }

    // for now this is really just utf8 position for just ascii...
    function getUtf16Position(index: number) {
        return annotatedChars[index];
    }

    function getIndex() {
        return index;
    }
    function restoreIndex(restoreIndex: number) {
        index = restoreIndex;
    }

    function getArtificalEndLimit() {
        return end;
    }
    function setArtificialEndLimit(offset: number) {
        end = offset;
    }
    function clearArtificalEndLimit() {
        end = annotatedChars.length;
    }

    function hasNext(jump: number = 0) : boolean {
        return index + jump < end;
    }

    function peekChar(jump: number = 0) : AnnotatedChar | null {
        if (hasNext(jump)) {
            return annotatedChars[index + jump];
        }
        return null;
    }

    function nextChar() : AnnotatedChar {
        if (!hasNext()) {
            throw "scanner : call to next after EOF"
        }
        return annotatedChars[index++];
    }

    function peekToken(jump: number, mode: TokenizerMode) : Token {
        const saveIndex = getIndex();
        let result : Token;

        do {
            result = nextToken(mode);
        } while (jump--);

        restoreIndex(saveIndex);

        return result;
    }

    function nextToken(mode: TokenizerMode) : Token {
        if (!hasNext()) {
            // we've already seen a fin token;
            // or, we've hit the artificial end limit
            // either way, the only valid result is now FIN
            return Token(
                TokenType.EOF,
                "",
                token.range
            );
        }

        const tag = mode == TokenizerMode.tag;
        const script = mode == TokenizerMode.script;
        const from = getIndex();

        const c = peekChar()!.codepoint;
        switch (c) {
            case 0:
                return setToken(TokenType.EOF, from, from);
            case AsciiMap.LEFT_ANGLE:
                if (tag && maybeEat(/<\s*cf/iy)) return setToken(TokenType.CF_START_TAG_START, from, getIndex());
                // it would be nice to only eat "</cf" in tag mode, but we need to match it in script mode, so we can recognize when
                // a </cfscript> block is finished
                else if (maybeEat(/<\s*\/\s*cf/iy)) return setToken(TokenType.CF_END_TAG_START, from, getIndex());
                else if (tag && maybeEat(/<!---/iy)) return setToken(TokenType.CF_TAG_COMMENT_START, from, getIndex());
                else if (maybeEat(/<=/iy)) return setToken(TokenType.LEFT_ANGLE_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.LEFT_ANGLE);
            case AsciiMap.RIGHT_ANGLE:
                if (script && maybeEat(/>=/iy)) return setToken(TokenType.RIGHT_ANGLE_EQUAL, from, getIndex());
                return consumeCurrentCharAs(TokenType.RIGHT_ANGLE);
            case AsciiMap.EQUAL:
                if (maybeEat(/==/iy)) return setToken(TokenType.DBL_EQUAL, from, getIndex());
                else if (maybeEat(/=>/iy)) return setToken(TokenType.EQUAL_RIGHT_ANGLE, from, getIndex());
                else return consumeCurrentCharAs(TokenType.EQUAL);
            case AsciiMap.HASH:          return consumeCurrentCharAs(TokenType.HASH);
            case AsciiMap.PERCENT:       return consumeCurrentCharAs(TokenType.PERCENT);
            case AsciiMap.LEFT_PAREN:    return consumeCurrentCharAs(TokenType.LEFT_PAREN);
            case AsciiMap.RIGHT_PAREN:   return consumeCurrentCharAs(TokenType.RIGHT_PAREN);
            case AsciiMap.LEFT_BRACKET:  return consumeCurrentCharAs(TokenType.LEFT_BRACKET);
            case AsciiMap.RIGHT_BRACKET: return consumeCurrentCharAs(TokenType.RIGHT_BRACKET);
            case AsciiMap.LEFT_BRACE:    return consumeCurrentCharAs(TokenType.LEFT_BRACE);
            case AsciiMap.RIGHT_BRACE:   return consumeCurrentCharAs(TokenType.RIGHT_BRACE);
            case AsciiMap.CARET:         return consumeCurrentCharAs(TokenType.CARET);
            case AsciiMap.QUOTE_SINGLE:  return consumeCurrentCharAs(TokenType.QUOTE_SINGLE);
            case AsciiMap.QUOTE_DOUBLE:  return consumeCurrentCharAs(TokenType.QUOTE_DOUBLE);
            case AsciiMap.COMMA:         return consumeCurrentCharAs(TokenType.COMMA);
            case AsciiMap.COLON:         return consumeCurrentCharAs(TokenType.COLON);
            case AsciiMap.SEMICOLON:     return consumeCurrentCharAs(TokenType.SEMICOLON);
            case AsciiMap.QUESTION_MARK:
                // note we do not scan a QUESTION_MARK_COLON token here,
                // it appears that it is not actually a token, and it is valid to have a comment between the "?" and ":"
                // so it is generated if necessary during parsing, if we recognize "<question-mark><trivia>?<colon>"
                return consumeCurrentCharAs(TokenType.QUESTION_MARK);
            case AsciiMap.SPACE: //    [[fallthrough]];
            case AsciiMap.TAB:   // \t [[fallthrough]];
            case AsciiMap.CR:    // \r [[fallthrough]];
            case AsciiMap.LF:    // \n [[fallthrough]];
                // more of a "definite" eat here
                maybeEat(/\s+/iy);
                return setToken(TokenType.WHITESPACE, from, getIndex());
            case AsciiMap.EXCLAMATION:
                if (maybeEat(/!=/iy)) return setToken(TokenType.EXCLAMATION_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.EXCLAMATION);
            case AsciiMap.PIPE:
                if (maybeEat(/\|\|/iy)) return setToken(TokenType.DBL_PIPE, from, getIndex());
                else return consumeCurrentCharAs(TokenType.PIPE);
            case AsciiMap.DOT:
                if (maybeEat(/\.\d+/iy)) return setToken(TokenType.NUMBER, from, getIndex());
                return consumeCurrentCharAs(TokenType.DOT);
            case AsciiMap.PLUS:
                if (maybeEat(/\+\+/iy)) return setToken(TokenType.DBL_PLUS, from, getIndex());
                if (maybeEat(/\+=/iy)) return setToken(TokenType.PLUS_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.PLUS);
            case AsciiMap.MINUS:
                if (maybeEat(/--->/iy)) return setToken(TokenType.CF_TAG_COMMENT_END, from, getIndex());
                if (maybeEat(/--/iy)) return setToken(TokenType.DBL_MINUS, from, getIndex());
                if (maybeEat(/-=/iy)) return setToken(TokenType.MINUS_EQUAL, from, getIndex());
                if (tryEatNumber(from)) return token;
                else return consumeCurrentCharAs(TokenType.MINUS);
            case AsciiMap.AMPERSAND:
                if (maybeEat(/&&/iy)) return setToken(TokenType.DBL_AMPERSAND, from, getIndex());
                if (maybeEat(/&=/iy)) return setToken(TokenType.AMPERSAND_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.AMPERSAND);
            case AsciiMap.STAR:
                if (maybeEat(/\*=/iy)) return setToken(TokenType.STAR_EQUAL, from, getIndex());
                if (maybeEat(/\*\//iy)) return setToken(TokenType.STAR_FORWARD_SLASH, from, getIndex());
                else return consumeCurrentCharAs(TokenType.STAR);
            case AsciiMap.FORWARD_SLASH:
                if (script && maybeEat(/\/\//iy)) return setToken(TokenType.DBL_FORWARD_SLASH, from, getIndex());
                else if (script && maybeEat(/\/*/iy)) return setToken(TokenType.FORWARD_SLASH_STAR, from, getIndex());
                else if (tag && maybeEat(/\/\s*>/iy)) return setToken(TokenType.CF_TAG_END_VOID, from, getIndex());
                else if (maybeEat(/\/=/iy)) return setToken(TokenType.FORWARD_SLASH_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.FORWARD_SLASH);
            case AsciiMap.a: // [[fallthrough]];
            case AsciiMap.A:
                if (maybeEat(/and/iy)) return setToken(TokenType.LIT_AND, from, getIndex());
                else return lexeme();
            case AsciiMap.b: // [[fallthrough]];
            case AsciiMap.B:
                if (script && maybeEat(/break/iy)) return setToken(TokenType.KW_BREAK, from, getIndex());
                else return lexeme();
            case AsciiMap.c: // [[fallthrough]];
            case AsciiMap.C:
                if (script && maybeEat(/catch/iy)) return setToken(TokenType.KW_CATCH, from, getIndex());
                else if (script && maybeEat(/case/iy)) return setToken(TokenType.KW_CASE, from, getIndex());
                else if (maybeEat(/contains/iy)) return setToken(TokenType.LIT_CONTAINS, from, getIndex());
                else return lexeme();
            case AsciiMap.d: // [[fallthrough]];
            case AsciiMap.D:
                if (maybeEat(/does\s+not\s+contain/iy)) return setToken(TokenType.LIT_DOES_NOT_CONTAIN, from, getIndex());
                else if (script && maybeEat(/default/iy)) return setToken(TokenType.KW_DEFAULT, from, getIndex());
                else if (script && maybeEat(/do/iy)) return setToken(TokenType.KW_DO, from, getIndex());
                else return lexeme();
            case AsciiMap.e: // [[fallthrough]];
            case AsciiMap.E:
                if (maybeEat(/eq/iy)) return setToken(TokenType.LIT_EQ, from, getIndex());
                else if (script && maybeEat(/else/iy)) return setToken(TokenType.KW_ELSE, from, getIndex());
                else return lexeme();
            case AsciiMap.f: // [[fallthrough]];
            case AsciiMap.F:
                if (maybeEat(/false/iy)) return setToken(TokenType.KW_FALSE, from, getIndex());
                else if (maybeEat(/final/iy)) return setToken(TokenType.KW_FINAL, from, getIndex());
                else if (script && maybeEat(/for/iy)) return setToken(TokenType.KW_FOR, from, getIndex());
                else if (maybeEat(/function/iy)) return setToken(TokenType.KW_FUNCTION, from, getIndex());
                else return lexeme();
            case AsciiMap.g: // [[fallthrough]];
            case AsciiMap.G:
                if (maybeEat(/gte/iy)) return setToken(TokenType.LIT_GTE, from, getIndex());
                else if (maybeEat(/gt/iy)) return setToken(TokenType.LIT_GT, from, getIndex());
                else if (maybeEat(/ge/iy)) return setToken(TokenType.LIT_GE, from, getIndex());
                else return lexeme();
            case AsciiMap.i: // [[fallthrough]];
            case AsciiMap.I:
                if (script && maybeEat(/if/iy)) return setToken(TokenType.KW_IF, from, getIndex());
                else if (script && maybeEat(/import/iy)) return setToken(TokenType.KW_IMPORT, from, getIndex());
                else if (maybeEat(/is\s+not/iy)) return setToken(TokenType.LIT_IS_NOT, from, getIndex());
                else if (maybeEat(/is/iy)) return setToken(TokenType.LIT_IS, from, getIndex());
                else return lexeme();
            case AsciiMap.l: // [[fallthrough]];
            case AsciiMap.L:
                if (maybeEat(/lte/iy)) return setToken(TokenType.LIT_LTE, from, getIndex());
                else if (maybeEat(/le/iy)) return setToken(TokenType.LIT_LE, from, getIndex());
                else if (maybeEat(/lt/iy)) return setToken(TokenType.LIT_LT, from, getIndex());
                else return lexeme();
            case AsciiMap.m: // [[fallthrough]];
            case AsciiMap.M:
                if (maybeEat(/mod/iy)) return setToken(TokenType.LIT_MOD, from, getIndex());
                else return lexeme();
            case AsciiMap.n: // [[fallthrough]];
            case AsciiMap.N:
                if (maybeEat(/neq/iy)) return setToken(TokenType.LIT_NEQ, from, getIndex());
                else if (maybeEat(/new/iy)) return setToken(TokenType.KW_NEW, from, getIndex());
                else if (maybeEat(/not/iy)) return setToken(TokenType.LIT_NOT, from, getIndex());
                else return lexeme();
            case AsciiMap.o: // [[fallthrough]];
            case AsciiMap.O:
                if (maybeEat(/or/iy)) return setToken(TokenType.LIT_OR, from, getIndex());
                else return lexeme();
            case AsciiMap.r: // [[fallthrough]];
            case AsciiMap.R:
                if (script && maybeEat(/return/iy)) return setToken(TokenType.KW_RETURN, from, getIndex());
                else return lexeme();
            case AsciiMap.s:
            case AsciiMap.S:
                if (script && maybeEat(/switch/iy)) return setToken(TokenType.KW_SWITCH, from, getIndex());
                else return lexeme();
            case AsciiMap.t: // [[fallthrough]];
            case AsciiMap.T:
                if (maybeEat(/true/iy)) return setToken(TokenType.KW_TRUE, from, getIndex());
                else if (script && maybeEat(/try/iy)) return setToken(TokenType.KW_TRY, from, getIndex());
                else return lexeme();
            case AsciiMap.v: // [[fallthrough]];
            case AsciiMap.V:
                if (maybeEat(/var/iy)) return setToken(TokenType.KW_VAR, from, getIndex());
                else return lexeme();
            case AsciiMap.w: // [[fallthrough]];
            case AsciiMap.W:
                if (script && maybeEat(/while/iy)) return setToken(TokenType.KW_WHILE, from, getIndex());
                else return lexeme();
            case AsciiMap.x: // [[fallthrough]];
            case AsciiMap.X:
                if (maybeEat(/xor/iy)) return setToken(TokenType.LIT_XOR, from, getIndex());
                else return lexeme();
            /*case '\xEF':
                if (maybeEat(/\xEF\xBB\xBF/, TokenType.BOM_UTF_8)) return token_;*/
            default:
                if (tryEatNumber(from)) return token;
                return lexeme();
        }
    }

    function maybeEat(pattern: RegExp) {
        pattern.lastIndex = index;
        const match = pattern.exec(sourceText);
        if (match) {
            index += match[0].length;
            lastScannedText = match[0]; // any perf boost to taking a slice of sourceText? is match[0] already a ref to sourceText's underlying storage?
            return true;
        }
        else {
            return false;
        }
    }
    
    function setToken(type: TokenType, from: number, to: number, text = lastScannedText) {
        token = Token(type, text, from, to);
        return token;
    }

    function currentToken() {
        return token;
    }

    function consumeCurrentCharAs(type: TokenType) {
        const from = getIndex();
        nextChar();
        return setToken(type, from, getIndex(), getTextSlice(new SourceRange(from, from+1)));
    }

    function getTokenText(token: Token) {
        const saveIndex = getIndex();
        const result : string[] = [];
        restoreIndex(token.range.fromInclusive);
        while(getIndex() != token.range.toExclusive) {
            result.push(
                String.fromCharCode(nextChar().codepoint))
        }
        restoreIndex(saveIndex);
        return result.join("");
    }

    function lexeme() {
        const from = getIndex();
        if (!maybeEat(/[$_a-z][$_a-z0-9]*/iy)) throw "expected a lexeme"
        return setToken(TokenType.LEXEME, from, getIndex());
    }

    function tryEatNumber(from: number) : Token | undefined {
        if (maybeEat(/-?\d+e[+-]?\d+(\.\d+)?|-?\d+(\.\d+)?|-?\.\d+/iy)) {
            return setToken(TokenType.NUMBER, from, getIndex());
        }
        return undefined;
    }

    function getTextSlice(range: SourceRange) {
        return sourceText.slice(range.fromInclusive, range.toExclusive);
    }

    function getLastScannedText() {
        return lastScannedText;
    }

    return {
        getLastScannedText,
        getTextSlice,
        getIndex,
        hasNext,
        peek: peekToken,
        next: nextToken,
        setArtificialEndLimit,
        getArtificalEndLimit,
        clearArtificalEndLimit,
        restoreIndex,
        maybeEat,
        getUtf16Position,
        currentToken,
        getTokenText
    }
}
export type Scanner = ReturnType<typeof Scanner>;

export interface Token {
    type: TokenType;
    range: SourceRange;
    text: string;
}

export function Token(type: TokenType, text: string, fromInclusive: number, toExclusive: number) : Token;
export function Token(type: TokenType, text: string, range: SourceRange) : Token;
export function Token(type: TokenType, text: string, fromOrRange: number | SourceRange, toExclusive?: number) : Token {
    type = type;
    text = text;
    if (typeof fromOrRange === "number") {
        return {
            type,
            text,
            range: new SourceRange(fromOrRange, toExclusive!)
        }
    }
    else {
        return {
            type,
            text,
            range: fromOrRange
        }
    }
}

export const NilToken : Readonly<Token> = Token(TokenType.NIL, "", SourceRange.Nil());

export const enum TokenizerMode { tag, script }
