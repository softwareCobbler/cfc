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
    function peek(jump: number = 0) : AnnotatedChar | null {
        if (hasNext(jump)) {
            return annotatedChars[index + jump];
        }
        return null;
    }
    function next() : AnnotatedChar {
        if (!hasNext()) {
            throw "scanner : call to next after EOF"
        }
        return annotatedChars[index++];
    }

    function maybeEat(pattern: RegExp) {
        pattern.lastIndex = index;
        const match = pattern.exec(sourceText);
        if (match) {
            index += match[0].length;
            return true;
        }
        else {
            return false;
        }
    }

    return {
        getIndex,
        peek,
        hasNext,
        next,
        setArtificialEndLimit,
        getArtificalEndLimit,
        clearArtificalEndLimit,
        restoreIndex,
        maybeEat,
    }
}
export type Scanner = ReturnType<typeof Scanner>;
