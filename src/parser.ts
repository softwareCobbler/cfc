namespace cf {
const enum NodeFlags {
    none    = 0,
    error   = 0x00000001,
    missing = 0x00000002
}

class NodeBase {
    parent: NodeBase | null;
    range: SourceRange;
    tagOrigin: {
        startTag: NodeBase | null,
        endTag: NodeBase | null
    };
    flags: NodeFlags = NodeFlags.none;

    constructor(sourceRange: SourceRange) {
        this.parent = null;
        this.range = sourceRange,
        this.tagOrigin = {startTag: null, endTag: null}
    }
}

function mergeRanges(...nodes : NodeBase[]) : SourceRange {
    const result = SourceRange.Nil();
    if (nodes.length === 0) {
        return result;
    }

    let gotStart = false;
    for (const node of nodes) {
        let thisRange : SourceRange;
        if (node instanceof NodeList) {
            thisRange = mergeRanges(...node.list);
        }
        else {
            thisRange = node.range;
        }

        if (!gotStart) {
            result.fromInclusive = thisRange.fromInclusive;
            gotStart = true;
        }
        else {
            result.toExclusive = thisRange.toExclusive;
        }
    }

    if (result.fromInclusive == -1 || result.toExclusive == -1) throw "bad range merge ?"

    return result;
}

class NodeList<T extends NodeBase> extends NodeBase {    
    list : T[];
    constructor(list: T[] = []) {
        super(SourceRange.Nil());
        this.list = list;
    }

    Nil<T extends NodeBase>() {
        return new NodeList<T>();
    }
}

class Terminal extends NodeBase {
    token: Token;
    trivia: NodeList<NodeBase>;
    constructor(token: Token, trivia?: NodeList<NodeBase>) {
        super(token.range);
        this.token = token;
        this.trivia = trivia ?? new NodeList<NodeBase>();
    }

    static Nil() {
        return new Terminal(
            new Token(
                TokenType.NIL,
                SourceRange.Nil()));
    }
}

class TextSpan extends NodeBase {
    // all we're interested in is range
    constructor(sourceRange: SourceRange) {
        super(sourceRange);
    }
}

class HashWrappedNode extends NodeBase {
    leftHash: Terminal;
    node: NodeBase;
    rightHash: Terminal;
    constructor(leftHash: Terminal, node: NodeBase, rightHash: Terminal) {
        super(mergeRanges(leftHash, node, rightHash));
        this.leftHash = leftHash;
        this.node = node;
        this.rightHash = rightHash;
    }
}

class TagAttribute extends NodeBase {
    name: Terminal;
    equals: Terminal | null;
    expr: NodeBase | null;

    constructor(name: Terminal);
    constructor(name: Terminal, equals: Terminal, expr: NodeBase);
    constructor(name: Terminal, equals?: Terminal, expr?: NodeBase | undefined) {
        if (name && equals && expr) {
            super(mergeRanges(name, expr));
        }
        else {
            super(name.range);
        }
        this.name = name;
        this.equals = equals ?? null;
        this.expr = expr ?? null;
    }
}

export namespace CfTag {
    //
    // end tags are expected to be "common" tags; they should not have attributes or etc.
    //
    export abstract class TagBase extends NodeBase {
        which: "start" | "end";
        tagStart: Terminal;         // <cf | </cf
        tagName: Terminal;          // terminal for "script" | "if" | "param", etc.; the "cf" is implied
        voidSlash: Terminal | null; // trailing "/" in "/>", if present
        tagEnd: Terminal;           // ">"
        canonicalName: string;      // string representation of tagName
        constructor(
            which: "start" | "end",
            tagStart: Terminal,
            tagName: Terminal,
            voidSlash: Terminal | null,
            tagEnd: Terminal,
            canonicalName: string) {
            super(mergeRanges(tagStart, tagEnd));
            this.which = which;
            this.tagStart = tagStart;
            this.tagName = tagName;
            this.voidSlash = voidSlash;
            this.tagEnd = tagEnd;
            this.canonicalName = canonicalName;
        }
    }
    export class Common extends TagBase { // most tags
        attrs: NodeList<TagAttribute>;
        constructor(which: "start", tagStart: Terminal, tagName: Terminal, voidSlash: Terminal | null, tagEnd: Terminal, canonicalName: string, attrs: NodeList<TagAttribute>);
        constructor(which: "end", tagStart: Terminal, tagName: Terminal, voidSlash: Terminal | null, tagEnd: Terminal, canonicalName: string);
        constructor(
            which: "start" | "end",
            tagStart: Terminal,
            tagName: Terminal,
            voidSlash: Terminal | null,
            tagEnd: Terminal,
            canonicalName: string,
            attrs?: NodeList<TagAttribute>) {
            super(which, tagStart, tagName, voidSlash, tagEnd, canonicalName);
            this.attrs = attrs ?? new NodeList<TagAttribute>();
        }
    }
    export class ScriptLike extends TagBase { // cfif, cfset
        expr: NodeBase;
        constructor(
            which: "start",
            tagStart: Terminal,
            tagName: Terminal,
            voidSlash: Terminal | null,
            tagEnd: Terminal,
            canonicalName: string,
            expr: NodeBase) {
            super(which, tagStart, tagName, voidSlash, tagEnd, canonicalName);
            this.expr = expr;
        }
    }
    export class Script extends TagBase { // cfscript
        stmtList: NodeList<NodeBase>;
        constructor(
            which: "start",
            tagStart: Terminal,
            tagName: Terminal,
            voidSlash: Terminal | null,
            tagEnd: Terminal,
            canonicalName: string,
            stmtList: NodeList<NodeBase>) {
            super(which, tagStart, tagName, voidSlash, tagEnd, canonicalName);
            this.stmtList = stmtList;
        }
    }
    export class Text extends TagBase { // text ranges
        // just interested in the node's range and uniquely identifying it as tag-text
        constructor(range: SourceRange) {
            const nilTerminal = Terminal.Nil();
            super("start", nilTerminal, nilTerminal, null, nilTerminal, "");
            this.range = range;
        }
    }
    export class Comment extends TagBase { // nested <!--- ... ---> blocks
        body: NodeList<TagBase>;
        constructor(
            which: "start",
            tagStart: Terminal,
            tagName: Terminal,
            voidSlash: Terminal | null,
            tagEnd: Terminal,
            canonicalName: string,
            body: NodeList<TagBase>) {
            super(which, tagStart, tagName, voidSlash, tagEnd, canonicalName);
            this.body = body;
        }
    }
}

const enum UnaryOperatorPos { pre, post };
const enum UnaryOpType { inc, dec, pos, neg };
class UnaryOperator {
    pos: UnaryOperatorPos;
    optype: UnaryOpType;
    operator: Terminal;
    expr: NodeBase;
    constructor(expr: NodeBase, op: Terminal);
    constructor(op: Terminal, expr: NodeBase);
    constructor(arg0: Terminal | NodeBase, arg1: Terminal | NodeBase) {
        if (arg0 instanceof Terminal) {
            this.pos = UnaryOperatorPos.pre;
            this.optype = UnaryOperator.tokenTypeToOpType(arg0.token.type);
            this.operator = arg0;
            this.expr = arg1;
        }
        else {
            this.pos = UnaryOperatorPos.post;
            this.optype = UnaryOperator.tokenTypeToOpType((arg1 as Terminal).token.type);
            this.operator = arg1 as Terminal;
            this.expr = arg0;
        }
    }
    static tokenTypeToOpType(tokenType: TokenType) {
		switch (tokenType) {
			case TokenType.DBL_MINUS: return UnaryOpType.dec;
			case TokenType.DBL_PLUS:  return UnaryOpType.inc;
			case TokenType.MINUS:     return UnaryOpType.neg;
			case TokenType.PLUS:      return UnaryOpType.pos;
			default: break;
		}
        throw "bad unary op type transform";
	}
}
const enum BinaryOpType {
    add, sub, mul, div, mod, exp, cat, eq, neq, lt, lte, gt, gte, 
}
class BinaryOperator extends NodeBase {
    optype: BinaryOpType;
    left: NodeBase;
    operator: Terminal;
    right: NodeBase;

    constructor(left: NodeBase, operator: Terminal, right: NodeBase) {
        super(mergeRanges(left, right));
        this.left = left;
        this.operator = operator;
        this.right = right;
        this.optype = BinaryOperator.tokenTypeToOpType(operator.token.type);
    }

    static tokenTypeToOpType(tokenType: TokenType) {
        switch (tokenType) {
            case TokenType.PLUS:          		return BinaryOpType.add;
            case TokenType.MINUS:         		return BinaryOpType.sub;
            case TokenType.STAR:          		return BinaryOpType.mul;
            case TokenType.FORWARD_SLASH: 		return BinaryOpType.div;
            case TokenType.PERCENT:       		return BinaryOpType.mod;
            case TokenType.CARET:         		return BinaryOpType.exp;
            case TokenType.AMPERSAND:     		return BinaryOpType.cat;

            case TokenType.DBL_EQUAL:       	return BinaryOpType.eq;
            case TokenType.LIT_EQ:        		return BinaryOpType.eq;
            case TokenType.EXCLAMATION_EQUAL:	return BinaryOpType.neq;
            case TokenType.LIT_NEQ:				return BinaryOpType.neq;

            case TokenType.LEFT_ANGLE:    		return BinaryOpType.lt;
            case TokenType.LIT_LT:              return BinaryOpType.lt;
            case TokenType.LEFT_ANGLE_EQUAL: 	return BinaryOpType.lte;
            case TokenType.LIT_LTE:				return BinaryOpType.lte;
            case TokenType.LIT_LE:				return BinaryOpType.lte;

            case TokenType.RIGHT_ANGLE:    		return BinaryOpType.gt;
            case TokenType.LIT_GT:				return BinaryOpType.gt;
            case TokenType.RIGHT_ANGLE_EQUAL: 	return BinaryOpType.gte;
            case TokenType.LIT_GTE:				return BinaryOpType.gte;
            case TokenType.LIT_GE:				return BinaryOpType.gte;
            default: break;
        }
        throw "bad binary op type transform";
    }
}

class StringLiteral extends NodeBase {
    //
    // it is possible to have an "unquoted" string literal, e.g.,
    // for tag attribute values like <cffunction name="x" foo=bar>
    // where bar is an unquoted string literal value
    // a null delimiter indicates this case
    //
    delimiter: TokenType.QUOTE_DOUBLE | TokenType.QUOTE_SINGLE | null;
    leftQuote: Terminal | null;
    elements: NodeList<NodeBase>;
    rightQuote: Terminal | null;

    constructor(unquotedStringLike: Terminal);
    constructor(quoteType: TokenType.QUOTE_DOUBLE | TokenType.QUOTE_SINGLE, leftQuote: Terminal, elements: NodeList<NodeBase>, rightQuote: Terminal);
    constructor(arg0: Terminal | TokenType.QUOTE_DOUBLE | TokenType.QUOTE_SINGLE, leftQuote?: Terminal, elements?: NodeList<NodeBase>, rightQuote?: Terminal) {
        if (arg0 instanceof Terminal) {
            super(arg0.range);
            this.delimiter = null;
            this.leftQuote = null;
            this.elements = new NodeList<NodeBase>([arg0]);
            this.rightQuote = null
        }
        else {
            super(mergeRanges(leftQuote!, rightQuote!));
            this.delimiter = arg0;
            this.leftQuote = leftQuote!;
            this.elements = elements!;
            this.rightQuote = rightQuote!;
        }
    }
}

const enum ParseOptions {
    none     = 0,
    noTrivia = 0,
    withTrivia        = 0x00000001,
    allowHashWrapped  = 0x00000002,
    undelimitedString = 0x00000004,
};

export class Parser {
    private tokenizer_ : Tokenizer;
    private mode_: TokenizerMode;
    private lookahead_ : TokenType;
    private parseErrorBeforeNextFinishedNode = false;

    constructor(tokenizer: Tokenizer, mode: TokenizerMode = TokenizerMode.tag) {
        this.tokenizer_ = tokenizer;
        this.mode_ = mode;
        this.lookahead_ = this.peek().type;
    }

    peek(jump: number = 0) {
        return this.tokenizer_.peek(jump, this.mode_);
    }
    lookahead() {
        return this.lookahead_;
    }
    next() {
        const result = this.tokenizer_.next(this.mode_);
        this.lookahead_ = this.peek().type;
        return result;
    }

    parseErrorAtCurrentPos(msg: string) : void {
        console.error("parse error at current pos; not yet impl'd");
        this.parseErrorBeforeNextFinishedNode = true;
    }

    createMissingNode<T extends NodeBase>(node: T) {
        node.flags |= NodeFlags.error | NodeFlags.missing;
        return node;
    }

    parseOptionalTerminal(type: TokenType, parseOptions: ParseOptions) : Terminal | null {
        if (this.lookahead() === type) {
            const token = this.next();
            if (parseOptions & ParseOptions.withTrivia) {
                return new Terminal(token, this.parseTrivia());
            }
            else {
                return new Terminal(token);
            }
        }
        else {
            return null;
        }
    }

    parseExpectedTerminal(type: TokenType, parseOptions: ParseOptions) : Terminal {
        const maybeTerminal = this.parseExpectedTerminal(type, parseOptions);
        if (maybeTerminal) {
            return maybeTerminal;
        }
        else {
            this.parseErrorAtCurrentPos("Expected a <??> here");
            const phonyToken : Token = new Token(type, SourceRange.Nil());
            return this.createMissingNode(new Terminal(phonyToken));
        }
    }

    parseTrivia() : NodeList<NodeBase> {
        return new NodeList<NodeBase>();
    }

    parseCfStartTag() {
        const tagStart = this.parseExpectedTerminal(TokenType.CF_START_TAG_START, ParseOptions.noTrivia);
        const tagName = this.parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
        const canonicalName = this.tokenizer_.getTokenText(tagName.token).toLowerCase();

        if (canonicalName === "if" || canonicalName === "elseif") {
            const expr = this.parseLogicalExpression();
            const rightAngle = this.parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
            return new CfTag.ScriptLike("start", tagStart, tagName, null, rightAngle, canonicalName, expr);
        }
        else if (canonicalName === "set") {
            const expr = this.parseAssignmentOrLower();
            const rightAngle = this.parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
            return new CfTag.ScriptLike("start", tagStart, tagName, null, rightAngle, canonicalName, expr);
        }
        else {
            const tagAttrs = this.parseTagAttributes();
            const maybeVoidSlash = this.parseOptionalTerminal(TokenType.FORWARD_SLASH, ParseOptions.withTrivia);
            const rightAngle = this.parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.withTrivia);
            return new CfTag.Common("start", tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, tagAttrs);
        }
    }

    parseCfEndTag() {
        const tagStart = this.parseExpectedTerminal(TokenType.CF_END_TAG_START, ParseOptions.noTrivia);
        const tagName = this.parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
        const rightAngle = this.parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);

        let canonicalName = "";
        if (!(tagName.flags & NodeFlags.missing)) {
            canonicalName = this.tokenizer_.getTokenText(tagName.token).toLowerCase();
        }
        return new CfTag.Common("end", tagStart, tagName, null, rightAngle, canonicalName);
    }

    parseTagAttributes() : NodeList<TagAttribute> {
        const result = new NodeList<TagAttribute>();

        while (this.lookahead() === TokenType.LEXEME) {
            const attrName = this.parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
            if (this.lookahead() === TokenType.EQUAL) {
                const equal = this.parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                let value : NodeBase;
                if (this.lookahead() === TokenType.LEXEME) {
                    value = this.parseStringLiteral(ParseOptions.undelimitedString);
                }
                else {
                    value = this.parseLogicalExpression();
                }
                result.list.push(new TagAttribute(attrName, equal, value));
            }
            else {
                result.list.push(new TagAttribute(attrName));
            }
        }

        return result;
    }

    parseTags() : NodeList<CfTag.TagBase> {
        const saveMode = this.mode_;
        this.mode_ = TokenizerMode.tag;

        const result = new NodeList<CfTag.TagBase>();
        let tagTextRange = SourceRange.Nil();

        const startOrContinueTagTextRange = () => {
            if (tagTextRange.isNil()) {
                const index = this.tokenizer_.getIndex();
                tagTextRange = new SourceRange(index, index+1);
            }
        }
        const finishTagTextRange = () => {
            if (tagTextRange.isNil()) {
                return;
            }
            tagTextRange.toExclusive = this.tokenizer_.getIndex() + 1;
            result.list.push(new CfTag.Text(tagTextRange))
            tagTextRange = SourceRange.Nil();
        }

        while (this.lookahead_ != TokenType.EOF) {
            switch (this.lookahead_) {
                case TokenType.CF_START_TAG_START: {
                    finishTagTextRange();
                    const tag = this.parseCfStartTag();
                    if (tag.canonicalName === "script") {
                        // parse cfscript ...
                    }
                    else {
                        result.list.push(tag);
                    }
                }
                case TokenType.CF_END_TAG_START: {
                    finishTagTextRange();
                    result.list.push(this.parseCfEndTag());
                }
                case TokenType.CF_TAG_COMMENT_START: {
                    finishTagTextRange();
                }
                default: {
                    startOrContinueTagTextRange();
                    this.next();
                }
            }
        }

        this.mode_ = saveMode;
        return result;
    }

    parseTextWithPossibleInterpolations(quoteDelimiter?: TokenType.QUOTE_SINGLE | TokenType.QUOTE_DOUBLE) {
        const result = new NodeList<NodeBase>();
        let textSourceRange = SourceRange.Nil();

        const startOrContinueTextRange = () => {
            if (textSourceRange.isNil()) {
                const index = this.tokenizer_.getIndex();
                textSourceRange = new SourceRange(index, index+1);
            }
            // continuing is a no-op; when we finish the text range, we'll update the "toExclusive"
            // with the tokenizer's current index
        }
        const finishTextRange = () => {
            // if we hadn't started a range yet, we're done
            if (textSourceRange.isNil()) {
                return;
            }
            textSourceRange.toExclusive = this.tokenizer_.getIndex() + 1;
            result.list.push(new TextSpan(textSourceRange));
            textSourceRange = SourceRange.Nil();
        }

        while (this.lookahead() != TokenType.EOF) {
            switch (this.lookahead()) {
                case quoteDelimiter: { // doubled-up delimiter; meaning it is an escaped quote
                    if (this.peek(1).type === quoteDelimiter) {
                        startOrContinueTextRange();
                        this.next(), this.next();
                        continue;
                    }
                    else { // single delimiter; we're done, don't eat it because the caller will need it
                        finishTextRange();
                        return result;
                    }
                }
                case TokenType.HASH: {
                    if (this.peek(1).type === TokenType.HASH) { // doubled up hash, meaning it is an escaped hash symbol
                        startOrContinueTextRange();
                        this.next(), this.next();
                        continue;
                    }
                    else { // single hash, meaning this is an interpolated string element
                        finishTextRange();
                        const leftHash = this.parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
                        const expr = this.parseLogicalExpression();
                        const rightHash = this.parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
                        result.list.push(new HashWrappedNode(leftHash, expr, rightHash));
                        continue;
                    }
                }
                default: {
                    startOrContinueTextRange();
                    this.next();
                }
            }
        }
        
        finishTextRange();
        return result;
    }

    parseAssignmentOrLower() : NodeBase {
        
    }

    parseLogicalExpression() : NodeBase {
        return new BinaryOperator();
    }

    parseStringLiteral(parseOptions: ParseOptions) : StringLiteral {
        if (parseOptions & ParseOptions.undelimitedString) {
            const unquotedString = this.parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
            return new StringLiteral(unquotedString);
        }
        else {
            const quoteType = this.lookahead();
            if (quoteType !== TokenType.QUOTE_SINGLE && quoteType !== TokenType.QUOTE_DOUBLE) {
                // will a lookahead or speculate ever trigger this ... ?
                throw "AssertionFailure: parseStringLiteral called on input without valid string delimiter";
            }

            const leftQuote = this.parseExpectedTerminal(quoteType, ParseOptions.noTrivia);
            const stringElements = this.parseTextWithPossibleInterpolations();
            const rightQuote = this.parseExpectedTerminal(quoteType, ParseOptions.withTrivia);
            return new StringLiteral(quoteType, leftQuote, stringElements, rightQuote);
        }
    }
}
}