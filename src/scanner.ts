namespace cf {
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

export class Scanner {
    private annotatedChars_ : AnnotatedChar[];
    private sourceText_ : string;
    private end_ : number;
    private index_ = 0;
    constructor(sourceText : string) {
        this.sourceText_ = sourceText;
        this.annotatedChars_ = this.annotate(this.sourceText_);
        this.end_ = this.annotatedChars_.length;
    }

    //
    // @fixme:
    // will work for ascii or utf16, but multibyte utf8 will probably break
    //
    private annotate(text: string) {
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

    getIndex() {
        return this.index_;
    }
    restoreIndex(index: number) {
        this.index_ = index;
    }

    getArtificalEndLimit() {
        return this.end_;
    }
    setArtificialEndLimit(offset: number) {
        this.end_ = offset;
    }
    clearArtificalEndLimit() {
        this.end_ = this.annotatedChars_.length;
    }

    hasNext(jump: number = 0) : boolean {
        return this.index_ + jump < this.end_;
    }
    peek(jump: number = 0) : AnnotatedChar | null {
        if (this.hasNext(jump)) {
            return this.annotatedChars_[this.index_ + jump];
        }
        return null;
    }
    next() : AnnotatedChar {
        if (!this.hasNext()) {
            throw "scanner : call to next after EOF"
        }
        return this.annotatedChars_[this.index_++];
    }

    maybeEat(pattern: RegExp) {
        pattern.lastIndex = this.index_;
        const match = pattern.exec(this.sourceText_);
        if (match) {
            this.index_ += match[0].length;
            return true;
        }
        else {
            return false;
        }
    }
}
} // namespace cf