export const enum AsciiMap {
    TAB = 9,
    NEWLINE = 10,         // \n
    CARRIAGE_RETURN = 13, // \r
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
    CHAR, // any single char that we didn't otherwise handle
    EOF,
    BOM_UTF_8,  // ef bb ef
    BOM_UTF_16, // fe ff

    WHITESPACE,
    LEXEME,
    NUMBER,

    AMPERSAND,
    AMPERSAND_EQUAL,
    CARET,
    COLON,
    DBL_COLON,
    COMMA,
    DBL_AMPERSAND,
    DBL_EQUAL,
    DBL_FORWARD_SLASH,
    DBL_MINUS,
    DBL_PIPE,
    DBL_PLUS,
    DOT,
    DOT_DOT_DOT,
    EQUAL,
    EQUAL_RIGHT_ANGLE,
    EXCLAMATION,
    EXCLAMATION_EQUAL,
    EXCLAMATION_DBL_EQUAL,
    FORWARD_SLASH,
    BACK_SLASH,
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
    PERCENT_EQUAL,
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
    TRIPLE_EQUAL,

    CF_START_TAG_START, // <\s*cf
    CF_END_TAG_START,   // <\s*/\s*cf
    CF_TAG_COMMENT_START,
    CF_TAG_COMMENT_END,

    _FIRST_LIT,
    LIT_AND,
    LIT_CONTAINS,
    LIT_DOES_NOT_CONTAIN,
    LIT_EQ,
    LIT_EQV, // binary operator 'equivalent'
    LIT_GT,
    LIT_GTE,
    LIT_GE,
    LIT_IMP, // binary operator 'implies'
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
    KW_CONTINUE,
    KW_DEFAULT,
    KW_DO,
    KW_ELSE,
    KW_FALSE,
    KW_FINAL,
    KW_FINALLY,
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
    [TokenType.CHAR]:                 "char",
    [TokenType.EOF]:                  "eof",
    [TokenType.BOM_UTF_8]:            "utf-8-bom",
    [TokenType.BOM_UTF_16]:           "utf-16-bom",

    [TokenType.WHITESPACE]:           "<whitespace>",
    [TokenType.LEXEME]:               "<lexeme>",
    [TokenType.NUMBER]:               "<number>",

    [TokenType.AMPERSAND]:            "&",
    [TokenType.AMPERSAND_EQUAL]:      "&=",
    [TokenType.CARET]:                "^",
    [TokenType.COLON]:                ":",
    [TokenType.DBL_COLON]:            "::", 
    [TokenType.COMMA]:                ",",
    [TokenType.DBL_AMPERSAND]:        "&&",
    [TokenType.DBL_EQUAL]:            "==",
    [TokenType.DBL_FORWARD_SLASH]:    "//",
    [TokenType.DBL_MINUS]:            "--",
    [TokenType.DBL_PIPE]:             "||",
    [TokenType.DBL_PLUS]:             "++",
    [TokenType.DOT]:                  ".",
    [TokenType.DOT_DOT_DOT]:          "...",
    [TokenType.EQUAL]:                "=",
    [TokenType.EQUAL_RIGHT_ANGLE]:    "=>",
    [TokenType.EXCLAMATION]:          "!",
    [TokenType.EXCLAMATION_EQUAL]:    "!=",
    [TokenType.EXCLAMATION_DBL_EQUAL]:    "!==",
    [TokenType.FORWARD_SLASH]:        "/",
    [TokenType.BACK_SLASH]:           "\\",
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
    [TokenType.PERCENT_EQUAL]:        "%=",
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
    [TokenType.TRIPLE_EQUAL]:         "===",

    [TokenType.CF_START_TAG_START]:   "<cf",
    [TokenType.CF_END_TAG_START]:     "</cf",
    [TokenType.CF_TAG_COMMENT_START]: "<!---",
    [TokenType.CF_TAG_COMMENT_END]:   "--->",

    [TokenType._FIRST_LIT]:           "<<IMMEDIATELY-BEFORE-FIRST-LITERAL>>",
    [TokenType.LIT_AND]:              "and",
    [TokenType.LIT_CONTAINS]:         "contains",
    [TokenType.LIT_DOES_NOT_CONTAIN]: "does not contain",
    [TokenType.LIT_EQ]:               "eq",
    [TokenType.LIT_EQV]:              "eqv",
    [TokenType.LIT_GT]:               "gt",
    [TokenType.LIT_GTE]:              "gte",
    [TokenType.LIT_GE]:               "ge",
    [TokenType.LIT_IMP]:              "imp",
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
    [TokenType.KW_CONTINUE]:          "continue",
    [TokenType.KW_DEFAULT]:           "default",
    [TokenType.KW_DO]:                "do",
    [TokenType.KW_ELSE]:              "else",
    [TokenType.KW_FALSE]:             "false",
    [TokenType.KW_FINAL]:             "final",
    [TokenType.KW_FINALLY]:           "finally",
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
    [TokenType._LAST_KW]:             "<<IMMEDIATELY-AFTER-LAST-KW>>",
} as const;

export const TokenTypeUiStringReverse = reverseMap(TokenTypeUiString);

function reverseMap<K extends number | string, V extends number | string>(source: Record<K, V>) : Record<V,K> {
    const result : Partial<Record<V,K>> = {};
    for (const key of (Object.keys(source) as unknown as (keyof typeof source)[])) {
        result[source[key]] = key;
    }
    return result as Record<V,K>;
}

const xxx = {
    startTagStart: /<\s*cf/iy,
    endTagStart: /<\s*\/\s*cf/iy,
    tagCommentStart: /<!---/iy,
    leftAngleEqual: /<=/iy,
    rightAngleEqual: />=/iy,
    tripleEqual: /===/iy,
    doubleEqual: /==/iy,
    equalRightAngle: /=>/iy,
    percentEqual: /%=/iy,
    dotDotDot: /\.\.\./iy,
    docBlock: /[ \t]*((\r|\n|\r\n)[ \t]*(\*[ \t])?)*/iy,
    whitespace: /\s+/iy,
    exclamationDoubleEqual: /!==/iy,
    exclamationEqual: /!=/iy,
    doublePipe: /\|\|/iy,

    dblPlus: /\+\+/iy,
    plusEqual: /\+=/iy,
    tagCommentEnd: /--->/iy,
    minusMinus: /--/iy,
    minusEqual: /-=/iy,
    dblAmpersand: /&&/iy,
    ampersandEqual: /&=/iy,
    starEqual: /\*=/iy,
    starForwardSlash: /\*\//iy,
    dblForwardSlash: /\/\//iy,
    forwardSlashStar: /\/\*/iy,
    forwardSlashEqual: /\/=/iy,
    and: /and\b/iy,
    break: /break\b/iy,
    catch: /catch\b/iy,
    case: /case\b/iy,
    contains: /contains\b/iy,
    continue: /continue\b/iy,
    doesNotContain: /does\s+not\s+contain\b/iy,
    default: /default\b/iy,
    do: /do\b/iy,
    eqv: /eqv\b/iy,
    eq: /eq\b/iy,
    else: /else\b/iy,
    false: /false\b/iy,
    finally: /finally\b/iy,
    final: /final\b/iy,
    for: /for\b/iy,
    function: /function\b/iy,
    gte: /gte\b/iy,
    gt: /gt\b/iy,
    ge: /ge\b/iy,
    if: /if\b/iy,
    import: /import\b/iy,
    imp: /imp\b/iy,
    isNot: /is\s+not\b/iy,
    is: /is\b/iy,
    lte: /lte\b/iy,
    le: /le\b/iy,
    lt: /lt\b/iy,
    mod: /mod\b/iy,
    neq: /neq\b/iy,
    new: /new\b/iy,
    not: /not\b/iy,
    or: /or\b/iy,
    return: /return\b/iy,
    switch: /switch\b/iy,
    true: /true\b/iy,
    try: /try\b/iy,
    var: /var\b/iy,
    while: /while\b/iy,
    xor: /xor\b/iy,
    ident: /[$_a-z][$_a-z0-9]*/iy,
    number: /\d+e[+-]?\d+(\.\d+)?\b|\d+(\.\d+)?\b/iy,
    dblColon: /::/iy,
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

    static copy(rhs: SourceRange) {
        return new SourceRange(rhs.fromInclusive, rhs.toExclusive);
    }

    static Nil() {
        return new SourceRange(-1, -1);
    }

    isNil() {
        return this.fromInclusive == -1 && this.toExclusive == -1;
    }

    includes(index: number) : boolean {
        return this.fromInclusive <= index && index < this.toExclusive
    }

    isDisjointFrom(other: SourceRange) : boolean {
        return (this.fromInclusive >= other.toExclusive) // definitely entirely beyond other
            || (this.fromInclusive < other.fromInclusive && this.toExclusive <= other.fromInclusive) // entirely before other
    }

    isDisjointFromAndNotAbutting(other: SourceRange) : boolean {
        return (this.fromInclusive > other.toExclusive)
            || (this.fromInclusive < other.fromInclusive && this.toExclusive < other.fromInclusive)
    }

    size() {
        return this.toExclusive - this.fromInclusive;
    }
}

type char = string; // with the intent being "exactly one character" (and non-empty!)

export const enum CfFileType { /* first is non-zero */ cfm = 1, cfc, dCfm };

let debugScanner = false;
export function setScannerDebug(isDebug: boolean) {
    debugScanner = isDebug;
}

export function Scanner(source_: string | Buffer) {
    let sourceText! : string;
    let sourceEncoding : "utf-8" | "utf-16-le" | "utf-16-be" = "utf-8";
    let hadBom = false;

    // these were once "annotated" with line/col info, but that's really expensive to do
    const annotatedChars = annotate(source_);

    let end = annotatedChars.length;
    let index = 0;
    let lastScannedText = "";

    //
    // if we get text, we assume it is already uft16-le, which is what node does by default
    // and vscode pushes text to us in this manner
    // if we want control over BOMs, read the file as bytes and provide the buffer;
    // we record if there was a BOM and what encoding we're in, but drop the BOM (if there was one)
    // before we start parsing, so we don't have to consier tokenizing it or having it end up in an identifier
    //
    function annotate(bytesOrAlreadyUtf16: string | Buffer) : string {
        if (typeof bytesOrAlreadyUtf16 === "string") { // already UTF-16
            if (bytesOrAlreadyUtf16.charCodeAt(0) === 0xFEFF) {
                hadBom = true;
            }
            sourceEncoding = "utf-16-le";
            sourceText = hadBom ? bytesOrAlreadyUtf16.slice(1) : bytesOrAlreadyUtf16;
            return annotateWorker(bytesOrAlreadyUtf16);
        }

        const bytes = bytesOrAlreadyUtf16; // alias; this is zero cost, right ?

        // defaults
        sourceEncoding = "utf-8";
        hadBom = false;

        if (bytes.length >= 3) {
            if (bytes[0] === 0xEF && bytes[1] === 0xBB && bytes[2] === 0xBF) {
                sourceEncoding = "utf-8";
                hadBom = true;
            }
        }
        else if (bytes.length >= 2) {
            if (bytes[0] === 0xFE && bytes[1] === 0xFF) {
                sourceEncoding = "utf-16-be";
                hadBom = true;
                // swap endianness from big to little
                for (let i = 0; i < bytes.length; i += 2) {
                    let t = bytes[i+1];
                    bytes[i+1] = bytes[i];
                    bytes[i] = t;
                }
            }
            else if (bytes[0] === 0xFF && bytes[1] === 0xFE) {
                sourceEncoding = "utf-16-le"
            }
        }
        
        sourceText = sourceEncoding === "utf-8"
            ? bytes.slice(hadBom ? 3 : 0).toString("utf-8")
            : bytes.slice(2).toString("utf16le");

        return annotateWorker(sourceText);
    }

    /**
     * build the (codepoint, col, line) tuples for each codepoint in the source text
     */
    function annotateWorker(utf16: string) : string {
        return utf16;
        // let index = 0;
        // let col = 0;
        // let line = 0;
        // const result : AnnotatedChar[] = [];
        // for (const char of utf16) {
        //     for (let i = 0; i < char.length; i++) {
        //         const c = char.charCodeAt(i);
        //         result.push({
        //             codepoint: c,
        //             // index,
        //             // col,
        //             // line
        //         });
        //         if (c == AsciiMap.NEWLINE) {
        //             line += 1;
        //             col = 0;
        //         }
        //         else {
        //             col += 1;
        //         }
        //         index += 1;
        //     }
        // }

        // // push "eof"
        // result.push({
        //     codepoint: 0,
        //     // index,
        //     // col,
        //     // line
        // });

        // return result;
    }

    // for now this is really just utf8 position for just ascii...
    function getUtf16Position(index: number) {
        return annotatedChars.codePointAt(index)!
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
            return annotatedChars.codePointAt(index + jump)!;
        }
        return null;
    }
type AnnotatedChar = number;
    function nextChar() : AnnotatedChar {
        if (!hasNext()) {
            throw "scanner : call to next after EOF"
        }
        return annotatedChars.codePointAt(index++)!;
    }

    function peekToken(jump: number, mode: ScannerMode) : Token {
        const saveIndex = getIndex();
        let result : Token;

        do {
            result = nextToken(mode);
        } while (jump--);

        restoreIndex(saveIndex);

        return result;
    }

    function nextToken(mode: ScannerMode) : Token {
        if (!hasNext()) {
            // we've already seen a fin token;
            // or, we've hit the artificial end limit
            // either way, the only valid result is now FIN
            return Token(
                TokenType.EOF,
                "",
                new SourceRange(end, end)
            );
        }

        const tag = !!(mode & ScannerMode.tag);
        const script = !!(mode & ScannerMode.script);
        const docBlock = !!(mode & ScannerMode.docBlock);
        const from = getIndex();

        const c = peekChar()!;
        switch (c) {
            case 0:
                return makeToken(TokenType.EOF, from, from, '');
            case AsciiMap.LEFT_ANGLE:
                if (tag && maybeEat(xxx.startTagStart)) return makeToken(TokenType.CF_START_TAG_START, from, getIndex());
                // it would be nice to only eat "</cf" in tag mode, but we need to match it in script mode, so we can recognize when
                // a </cfscript> block is finished
                else if (maybeEat(xxx.endTagStart)) return makeToken(TokenType.CF_END_TAG_START, from, getIndex());
                else if (tag && maybeEat(xxx.tagCommentStart)) return makeToken(TokenType.CF_TAG_COMMENT_START, from, getIndex());
                else if (maybeEat(xxx.leftAngleEqual)) return makeToken(TokenType.LEFT_ANGLE_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.LEFT_ANGLE);
            case AsciiMap.RIGHT_ANGLE:
                if (script && maybeEat(xxx.rightAngleEqual)) return makeToken(TokenType.RIGHT_ANGLE_EQUAL, from, getIndex());
                return consumeCurrentCharAs(TokenType.RIGHT_ANGLE);
            case AsciiMap.EQUAL:
                if (maybeEat(xxx.tripleEqual)) return makeToken(TokenType.TRIPLE_EQUAL, from, getIndex());
                else if (maybeEat(xxx.doubleEqual)) return makeToken(TokenType.DBL_EQUAL, from, getIndex());
                else if (maybeEat(xxx.equalRightAngle)) return makeToken(TokenType.EQUAL_RIGHT_ANGLE, from, getIndex());
                else return consumeCurrentCharAs(TokenType.EQUAL);
            case AsciiMap.PERCENT:
                if (maybeEat(xxx.percentEqual)) return makeToken(TokenType.PERCENT_EQUAL, from, getIndex());
                return consumeCurrentCharAs(TokenType.PERCENT);
            case AsciiMap.HASH:          return consumeCurrentCharAs(TokenType.HASH);
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
            case AsciiMap.COLON:
                if (maybeEat(xxx.dblColon)) return makeToken(TokenType.DBL_COLON, from, getIndex());
                else return consumeCurrentCharAs(TokenType.COLON);
            case AsciiMap.SEMICOLON:     return consumeCurrentCharAs(TokenType.SEMICOLON);
            case AsciiMap.DOT:           
                if (maybeEat(xxx.dotDotDot)) return makeToken(TokenType.DOT_DOT_DOT, from, getIndex());
                else return consumeCurrentCharAs(TokenType.DOT);
            case AsciiMap.QUESTION_MARK:
                // note we do not scan a QUESTION_MARK_COLON token here,
                // it appears that it is not actually a token, and it is valid to have a comment between the "?" and ":"
                // so it is generated if necessary during parsing, if we recognize "<question-mark><trivia>?<colon>"
                return consumeCurrentCharAs(TokenType.QUESTION_MARK);
            case AsciiMap.SPACE:           //    [[fallthrough]];
            case AsciiMap.TAB:             // \t [[fallthrough]];
            case AsciiMap.CARRIAGE_RETURN: // \r [[fallthrough]];
            case AsciiMap.NEWLINE:         // \n [[fallthrough]];
                // `maybeEat` is more of a "definite" eat here
                if (docBlock) {
                    // eat whitespace, and if we get a newline, nextline's whitespace, up to the first "*" if it exists, and then subsequent whitespace
                    // if we get another newline, repeat the process
                    if (maybeEat(xxx.docBlock)) return makeToken(TokenType.WHITESPACE, from, getIndex());
                    // shouldn't be necessary,
                    // but have to prove the above always consumes at least one character (the pattern itself is like x*(y(z)?)* which is "possibly match none")?
                    else return consumeCurrentCharAs(TokenType.WHITESPACE);
                }
                maybeEat(xxx.whitespace);
                return makeToken(TokenType.WHITESPACE, from, getIndex());
            case AsciiMap.EXCLAMATION:
                if (maybeEat(xxx.exclamationDoubleEqual)) return makeToken(TokenType.EXCLAMATION_DBL_EQUAL, from, getIndex());
                else if (maybeEat(xxx.exclamationEqual)) return makeToken(TokenType.EXCLAMATION_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.EXCLAMATION);
            case AsciiMap.PIPE:
                if (maybeEat(xxx.doublePipe)) return makeToken(TokenType.DBL_PIPE, from, getIndex());
                else return consumeCurrentCharAs(TokenType.PIPE);
            case AsciiMap.PLUS:
                if (maybeEat(xxx.dblPlus)) return makeToken(TokenType.DBL_PLUS, from, getIndex());
                if (maybeEat(xxx.plusEqual)) return makeToken(TokenType.PLUS_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.PLUS);
            case AsciiMap.MINUS:
                if (maybeEat(xxx.tagCommentEnd)) return makeToken(TokenType.CF_TAG_COMMENT_END, from, getIndex());
                if (maybeEat(xxx.minusMinus)) return makeToken(TokenType.DBL_MINUS, from, getIndex());
                if (maybeEat(xxx.minusEqual)) return makeToken(TokenType.MINUS_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.MINUS);
            case AsciiMap.AMPERSAND:
                if (maybeEat(xxx.dblAmpersand)) return makeToken(TokenType.DBL_AMPERSAND, from, getIndex());
                if (maybeEat(xxx.ampersandEqual)) return makeToken(TokenType.AMPERSAND_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.AMPERSAND);
            case AsciiMap.STAR:
                if (maybeEat(xxx.starEqual)) return makeToken(TokenType.STAR_EQUAL, from, getIndex());
                if (maybeEat(xxx.starForwardSlash)) return makeToken(TokenType.STAR_FORWARD_SLASH, from, getIndex());
                else return consumeCurrentCharAs(TokenType.STAR);
            case AsciiMap.FORWARD_SLASH:
                if (script && maybeEat(xxx.dblForwardSlash)) return makeToken(TokenType.DBL_FORWARD_SLASH, from, getIndex());
                else if (script && maybeEat(xxx.forwardSlashStar)) return makeToken(TokenType.FORWARD_SLASH_STAR, from, getIndex());
                else if (maybeEat(xxx.forwardSlashEqual)) return makeToken(TokenType.FORWARD_SLASH_EQUAL, from, getIndex());
                else return consumeCurrentCharAs(TokenType.FORWARD_SLASH);
            case AsciiMap.BACK_SLASH:
                return consumeCurrentCharAs(TokenType.BACK_SLASH);
            case AsciiMap.a: // [[fallthrough]];
            case AsciiMap.A:
                if (maybeEat(xxx.and)) return makeToken(TokenType.LIT_AND, from, getIndex());
                else return lexeme();
            case AsciiMap.b: // [[fallthrough]];
            case AsciiMap.B:
                if (script && maybeEat(xxx.break)) return makeToken(TokenType.KW_BREAK, from, getIndex());
                else return lexeme();
            case AsciiMap.c: // [[fallthrough]];
            case AsciiMap.C:
                if (script && maybeEat(xxx.catch)) return makeToken(TokenType.KW_CATCH, from, getIndex());
                else if (script && maybeEat(xxx.case)) return makeToken(TokenType.KW_CASE, from, getIndex());
                else if (maybeEat(xxx.contains)) return makeToken(TokenType.LIT_CONTAINS, from, getIndex());
                else if (script && maybeEat(xxx.continue)) return makeToken(TokenType.KW_CONTINUE, from, getIndex());
                else return lexeme();
            case AsciiMap.d: // [[fallthrough]];
            case AsciiMap.D:
                if (maybeEat(xxx.doesNotContain)) return makeToken(TokenType.LIT_DOES_NOT_CONTAIN, from, getIndex());
                else if (script && maybeEat(xxx.default)) return makeToken(TokenType.KW_DEFAULT, from, getIndex());
                else if (script && maybeEat(xxx.do)) return makeToken(TokenType.KW_DO, from, getIndex());
                else return lexeme();
            case AsciiMap.e: // [[fallthrough]];
            case AsciiMap.E:
                if (maybeEat(xxx.eqv)) return makeToken(TokenType.LIT_EQV, from, getIndex());
                else if (maybeEat(xxx.eq)) return makeToken(TokenType.LIT_EQ, from, getIndex());
                else if (script && maybeEat(xxx.else)) return makeToken(TokenType.KW_ELSE, from, getIndex());
                else return lexeme();
            case AsciiMap.f: // [[fallthrough]];
            case AsciiMap.F:
                if (maybeEat(xxx.false)) return makeToken(TokenType.KW_FALSE, from, getIndex());
                else if (script && maybeEat(xxx.finally)) return makeToken(TokenType.KW_FINALLY, from, getIndex());
                else if (maybeEat(xxx.final)) return makeToken(TokenType.KW_FINAL, from, getIndex());
                else if (script && maybeEat(xxx.for)) return makeToken(TokenType.KW_FOR, from, getIndex());
                else if (maybeEat(xxx.function)) return makeToken(TokenType.KW_FUNCTION, from, getIndex());
                else return lexeme();
            case AsciiMap.g: // [[fallthrough]];
            case AsciiMap.G:
                if (maybeEat(xxx.gte)) return makeToken(TokenType.LIT_GTE, from, getIndex());
                else if (maybeEat(xxx.gt)) return makeToken(TokenType.LIT_GT, from, getIndex());
                else if (maybeEat(xxx.ge)) return makeToken(TokenType.LIT_GE, from, getIndex());
                else return lexeme();
            case AsciiMap.i: // [[fallthrough]];
            case AsciiMap.I:
                if (script && maybeEat(xxx.if)) return makeToken(TokenType.KW_IF, from, getIndex());
                else if (script && maybeEat(xxx.import)) return makeToken(TokenType.KW_IMPORT, from, getIndex());
                else if (maybeEat(xxx.imp)) return makeToken(TokenType.LIT_IMP, from, getIndex());
                else if (maybeEat(xxx.isNot)) return makeToken(TokenType.LIT_IS_NOT, from, getIndex());
                else if (maybeEat(xxx.is)) return makeToken(TokenType.LIT_IS, from, getIndex());
                else return lexeme();
            case AsciiMap.l: // [[fallthrough]];
            case AsciiMap.L:
                if (maybeEat(xxx.lte)) return makeToken(TokenType.LIT_LTE, from, getIndex());
                else if (maybeEat(xxx.le)) return makeToken(TokenType.LIT_LE, from, getIndex());
                else if (maybeEat(xxx.lt)) return makeToken(TokenType.LIT_LT, from, getIndex());
                else return lexeme();
            case AsciiMap.m: // [[fallthrough]];
            case AsciiMap.M:
                if (maybeEat(xxx.mod)) return makeToken(TokenType.LIT_MOD, from, getIndex());
                else return lexeme();
            case AsciiMap.n: // [[fallthrough]];
            case AsciiMap.N:
                if (maybeEat(xxx.neq)) return makeToken(TokenType.LIT_NEQ, from, getIndex());
                else if (maybeEat(xxx.new)) return makeToken(TokenType.KW_NEW, from, getIndex());
                else if (maybeEat(xxx.not)) return makeToken(TokenType.LIT_NOT, from, getIndex());
                else return lexeme();
            case AsciiMap.o: // [[fallthrough]];
            case AsciiMap.O:
                if (maybeEat(xxx.or)) return makeToken(TokenType.LIT_OR, from, getIndex());
                else return lexeme();
            case AsciiMap.r: // [[fallthrough]];
            case AsciiMap.R:
                if (script && maybeEat(xxx.return)) return makeToken(TokenType.KW_RETURN, from, getIndex());
                else return lexeme();
            case AsciiMap.s:
            case AsciiMap.S:
                if (script && maybeEat(xxx.switch)) return makeToken(TokenType.KW_SWITCH, from, getIndex());
                else return lexeme();
            case AsciiMap.t: // [[fallthrough]];
            case AsciiMap.T:
                if (maybeEat(xxx.true)) return makeToken(TokenType.KW_TRUE, from, getIndex());
                else if (script && maybeEat(xxx.try)) return makeToken(TokenType.KW_TRY, from, getIndex());
                else return lexeme();
            case AsciiMap.v: // [[fallthrough]];
            case AsciiMap.V:
                if (maybeEat(xxx.var)) return makeToken(TokenType.KW_VAR, from, getIndex());
                else return lexeme();
            case AsciiMap.w: // [[fallthrough]];
            case AsciiMap.W:
                if (script && maybeEat(xxx.while)) return makeToken(TokenType.KW_WHILE, from, getIndex());
                else return lexeme();
            case AsciiMap.x: // [[fallthrough]];
            case AsciiMap.X:
                if (maybeEat(xxx.xor)) return makeToken(TokenType.LIT_XOR, from, getIndex());
                else return lexeme();
            default:
                return tryEatNumber(from) || lexeme();
        }
    }

    /**
     * target is expected to be a string of length 1, or a TokenType
     * if a char, it is case-sensitive
     * this leaves the scanner primed to scan the target char or TokenType on the next call to `nextToken`
     * if we're already primed to scan the target char or token type we do not move
     */
    function scanToNext(target: char) : void;
    function scanToNext(target: TokenType[], mode: ScannerMode) : void;
    function scanToNext(target: char | TokenType[], mode?: ScannerMode) : void {
        if (typeof target === "string") {
            if (sourceText[index] === target) {
                return;
            }
            const targetCodepoint = target.charCodeAt(0);
            while (true) { // will bail on peekChar returning null
                const nextCodepoint = peekChar();
                if (!nextCodepoint || nextCodepoint === targetCodepoint) {
                    break;
                }
                else {
                    nextChar();
                }
            }
        }
        else {
            let lastIndex = getIndex();
            let token = peekToken(0, mode!);
            while (!target.includes(token.type) && token.type !== TokenType.EOF) {
                lastIndex = getIndex();
                token = nextToken(mode!);
            }
            restoreIndex(lastIndex); // prime the scanner so it's ready to scan this token on subsequent call to next; might be EOF, that's OK
        }
    }

    function isAsciiAlpha(c: number) {
        return (c >= AsciiMap.A && c <= AsciiMap.Z)
            || (c >= AsciiMap.a && c <= AsciiMap.z);
    }

    function isAsciiDigit(c: number) {
        return c >= AsciiMap._0 && c <= AsciiMap._9;
    }

    function isTagNameStart() {
        const codePoint = annotatedChars.codePointAt(index)!;
        return isAsciiAlpha(codePoint)
            || codePoint === AsciiMap.UNDERSCORE
    }

    function isTagNameRest() {
        const codePoint = annotatedChars.codePointAt(index)!
        return isAsciiAlpha(codePoint)
            || isAsciiDigit(codePoint)
            || codePoint === AsciiMap.UNDERSCORE
    }

    function isTagAttributeNameStart(codePoint: number) {
        return isAsciiAlpha(codePoint)
            || codePoint === AsciiMap.UNDERSCORE
            || codePoint === AsciiMap.DOLLAR;
    }

    function isTagAttributeNameRest(codePoint: number, allowDot: boolean) {
        return isAsciiAlpha(codePoint)
            || isAsciiDigit(codePoint)
            || codePoint === AsciiMap.UNDERSCORE
            || codePoint === AsciiMap.MINUS
            || codePoint === AsciiMap.DOLLAR
            || codePoint === AsciiMap.COLON
            || (codePoint === AsciiMap.DOT && allowDot);
    }

    function scanTagName() : Token | null {
        if (!isTagNameStart()) return null;
        const from = index;
        index += 1;
        while (isTagNameRest()) {
            index += 1;
        }
        return makeToken(TokenType.LEXEME, from, getIndex());
    }

    function scanTagAttributeName(allowDot: boolean) : Token | null {
        if (!hasNext() || !isTagAttributeNameStart(annotatedChars.codePointAt(index)!)) return null;
        const from = index;
        index += 1;
        while (isTagAttributeNameRest(annotatedChars.codePointAt(index)!, allowDot)) {
            index += 1;
        }
        lastScannedText = sourceText.slice(from, index);
        return makeToken(TokenType.LEXEME, from, getIndex());
    }

    function isTagCommentOpen() {
        if (index + "<!---".length >= annotatedChars.length) {
            return false;
        }
        return annotatedChars.codePointAt(index + 0) === AsciiMap.LEFT_ANGLE
            && annotatedChars.codePointAt(index + 1) === AsciiMap.EXCLAMATION
            && annotatedChars.codePointAt(index + 2) === AsciiMap.MINUS
            && annotatedChars.codePointAt(index + 3) === AsciiMap.MINUS
            && annotatedChars.codePointAt(index + 4) === AsciiMap.MINUS;
    }

    function isTagCommentClose() {
        if (index + "--->".length > annotatedChars.length) {
            return false;
        }
        return annotatedChars.codePointAt(index + 0) === AsciiMap.MINUS
            && annotatedChars.codePointAt(index + 1) === AsciiMap.MINUS
            && annotatedChars.codePointAt(index + 2) === AsciiMap.MINUS
            && annotatedChars.codePointAt(index + 3) === AsciiMap.RIGHT_ANGLE
    }

    /**
     * this is helpful in avoiding the possibility of a tag comment being like "----->"
     * which could reasonably be matched as [DBL_MINUS, DBL_MINUS, MINUS, RIGHT_ANGLE]
     */
    function scanToNextTagCommentToken() : void {
        while (index < annotatedChars.length) {
            if (isTagCommentOpen() || isTagCommentClose()) {
                return;
            }
            else {
                index++;
            }
        }
    }

    function isLexemeLikeStructKeyStart(c: number) : boolean {
        return isAsciiAlpha(c)
            || isAsciiDigit(c)
            || c === AsciiMap.UNDERSCORE
            || c === AsciiMap.DOLLAR;
    }

    function isLexemeLikeStructKeyRest(c: number) : boolean {
        return isLexemeLikeStructKeyStart(c) || c === AsciiMap.DOT;
    }

    function scanLexemeLikeStructKey() : Token | null {
        if (!isLexemeLikeStructKeyStart(annotatedChars.codePointAt(index)!)) {
            return null;
        }

        const from = index;
        index += 1;

        while (isLexemeLikeStructKeyRest(annotatedChars.codePointAt(index)!)) {
            index += 1;
        }

        return makeToken(TokenType.LEXEME, from, index);
    }

    function isIdentifierStart(codePoint: number) {
        return isAsciiAlpha(codePoint)
            || codePoint === AsciiMap.UNDERSCORE
            || codePoint === AsciiMap.DOLLAR
            || codePoint > 128; // waaaaaay to wide a unicode acceptance range
    }

    function isIdentifierRest(codePoint: number) {
        return isAsciiAlpha(codePoint)
            || isAsciiDigit(codePoint)
            || codePoint === AsciiMap.UNDERSCORE
            || codePoint === AsciiMap.DOLLAR
            || codePoint > 128;
    }

    function scanIdentifierWorker() : string | null {
        if (!isIdentifierStart(annotatedChars.codePointAt(index)!)) return null;
        const from = index;
        index += 1;
        while (isIdentifierRest(annotatedChars.codePointAt(index)!)) {
            index += 1;
        }
        return sourceText.slice(from, index);
    }

    function scanIdentifier() : Token | null {
        const from = index;
        const identifierText = scanIdentifierWorker();
        if (!identifierText) {
            return null;
        }
        else {
            lastScannedText = identifierText;
            return makeToken(TokenType.LEXEME, from, getIndex());
        }
    }

    function scanDocBlockAttrName() : string | null {
        let withExclamation = false;
        if (annotatedChars.codePointAt(index)! === AsciiMap.AT) {
            const startPos = index;
            index++;
            if (annotatedChars.codePointAt(index)! === AsciiMap.EXCLAMATION) { // support constructs like "@!type any", have to disambiguate with real CF attribute syntax
                index++;
                withExclamation = true;
            }
            const identifierText = scanIdentifierWorker();
            if (!identifierText) {
                index = startPos;
                return null;
            }
            else {
                return withExclamation
                    ? "!" + identifierText
                    : identifierText;
            }
        }
        return null;
    }

    // caller should have set the end limit to the appropriate position
    // so that `hasNext()` returns false on the character that would otherwise be the "*/" comment terminator
    function scanDocBlockAttrText() : string {
        let start = index;
        let justWhitespaceThisLine = true;
        let inPrefix = false;
        const result : string[] = [];
        while (hasNext()) {
            if (annotatedChars.codePointAt(index)! === AsciiMap.SPACE || annotatedChars.codePointAt(index)! === AsciiMap.TAB) {
                index += 1;
                continue;
            }
            else if (annotatedChars.codePointAt(index)! === AsciiMap.NEWLINE) {
                justWhitespaceThisLine = true;
                inPrefix = true;
                index += 1;
                result.push(sourceText.slice(start, index));
                start = index;
                continue;
            }
            else if (annotatedChars.codePointAt(index)! === AsciiMap.CARRIAGE_RETURN && annotatedChars.codePointAt(index+1)! === AsciiMap.NEWLINE) {
                justWhitespaceThisLine = true;
                inPrefix = true;
                index += 2;
                result.push(sourceText.slice(start, index));
                start = index;
                continue;
            }
            else if (annotatedChars.codePointAt(index)! === AsciiMap.STAR) {
                index += 1;
                if (inPrefix) {
                    start = index;
                    inPrefix = false;
                }
                else {
                    justWhitespaceThisLine = false;
                }
            }
            else if (annotatedChars.codePointAt(index)! === AsciiMap.AT) {
                if (!justWhitespaceThisLine) {
                    index += 1;
                }
                else {
                    break;
                }
            }
            else {
                index += 1;
                justWhitespaceThisLine = false;
                continue;
            }
        }
        if (!justWhitespaceThisLine) result.push(sourceText.slice(start, index));
        const text = result.join("");
        lastScannedText = text;
        return text;
    }

    function isIdentifier() {
        return isIdentifierStart(annotatedChars.codePointAt(index)!);
    }

    function maybeEat(pattern: RegExp) {
        pattern.lastIndex = index;
        const match = pattern.exec(sourceText);
        if (match && match[0].length > 0) {
            index += match[0].length;
            lastScannedText = match[0]; // any perf boost to taking a slice of sourceText? is match[0] already a ref to sourceText's underlying storage?
            return true;
        }
        else {
            return false;
        }
    }
    
    function makeToken(tokenType: TokenType, from: number, to: number, text: string = lastScannedText): Token {
        const token = Token(tokenType, text, from!, to!);


        if (debugScanner) {
            // const annotatedFrom = getAnnotatedChar(from);
            // token.__debug_line = annotatedFrom.line + 1;
            // token.__debug_col = annotatedFrom.col + 1;
            token.__debug_type = TokenTypeUiString[tokenType];
        }

        return token;
    }

    function consumeCurrentCharAs(type: TokenType) {
        const from = getIndex();
        nextChar();
        return makeToken(type, from, getIndex(), getTextSlice(new SourceRange(from, from+1)));
    }

    function getTokenText(token: Token) {
        const saveIndex = getIndex();
        const result : string[] = [];
        restoreIndex(token.range.fromInclusive);
        while(getIndex() != token.range.toExclusive) {
            result.push(
                String.fromCharCode(nextChar()))
        }
        restoreIndex(saveIndex);
        return result.join("");
    }

    function lexeme() {
        const from = getIndex();
        if (maybeEat(xxx.ident)) {
            const index = getIndex();
            lastScannedText = getTextSlice(new SourceRange(from, index))
            return makeToken(TokenType.LEXEME, from, index);
        }
        else {
            nextChar();
            const index = getIndex();
            lastScannedText = getTextSlice(new SourceRange(from, index))
            return makeToken(TokenType.CHAR, from, getIndex());
        }
    }

    function tryEatNumber(from: number) : Token | undefined {
        if (maybeEat(xxx.number)) {
            return makeToken(TokenType.NUMBER, from, getIndex());
        }
        return undefined;
    }

    function getTextSlice(range: SourceRange) {
        return sourceText.slice(range.fromInclusive, range.toExclusive);
    }

    function getLastScannedText() {
        return lastScannedText;
    }

    function getAnnotatedChar(index: number) : AnnotatedChar {
        const clampedIndex = index < annotatedChars.length
            ? index
            : annotatedChars.length-1;
        return annotatedChars.codePointAt(clampedIndex)!;
    }

    return {
        advance: (n = 1) : boolean => {
            const startIndex = index;
            index = Math.min(index + n, end);
            return index !== startIndex;
        },
        getLastScannedText,
        getTextSlice,
        getIndex,
        hasNext,
        peek: peekToken,
        peekChar: (jump: number) => {
            const codepoint = peekChar(jump);
            return codepoint ? String.fromCharCode(codepoint) : "";
        },
        nextToken,
        scanToNext,
        setArtificialEndLimit,
        getArtificalEndLimit,
        clearArtificalEndLimit,
        restoreIndex,
        maybeEat,
        getUtf16Position,
        getTokenText,
        getAnnotatedChar,
        scanTagName,
        scanTagAttributeName,
        scanToNextTagCommentToken,
        scanLexemeLikeStructKey,
        scanIdentifier,
        isIdentifier,
        scanDocBlockAttrName,
        scanDocBlockAttrText,
        getSourceText: () => sourceText
    }
}
export type Scanner = ReturnType<typeof Scanner>;

export interface Token {
    type: TokenType;
    range: SourceRange;
    text: string;
    __debug_line?: number;
    __debug_col?: number;
    __debug_type?: string;
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

export const NilToken = (pos: number, text: string = "") => Token(TokenType.NIL, text, new SourceRange(pos, pos));

export const enum ScannerMode { tag = 1 << 1, script = 1 << 2, docBlock = 1 << 3 }
