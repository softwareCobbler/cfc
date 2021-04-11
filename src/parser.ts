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

function mergeRanges(...nodes : (NodeBase|null)[]) : SourceRange {
    const result = SourceRange.Nil();
    if (nodes.length === 0) {
        return result;
    }

    let gotStart = false;
    for (const node of nodes) {
        if (!node) continue;

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

class HashWrappedExpr extends NodeBase {
    leftHash: Terminal;
    expr: NodeBase;
    rightHash: Terminal;
    constructor(leftHash: Terminal, expr: NodeBase, rightHash: Terminal) {
        super(mergeRanges(leftHash, expr, rightHash));
        this.leftHash = leftHash;
        this.expr = expr;
        this.rightHash = rightHash;
    }
}

class Parenthetical extends NodeBase {
    leftParen: Terminal;
    expr: NodeBase;
    rightParen: Terminal;
    constructor(leftParen: Terminal, expr: NodeBase, rightParen: Terminal) {
        super(mergeRanges(leftParen, expr, rightParen));
        this.leftParen = leftParen;
        this.expr = expr;
        this.rightParen = rightParen;
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
            tagStart: Terminal,
            body: NodeList<TagBase>,
            tagEnd: Terminal) {
            const nilTerminal = Terminal.Nil();
            super("start", tagStart, nilTerminal, nilTerminal, tagEnd, "");
            this.body = body;
        }
    }
}

class CallExpression extends NodeBase {
    identifier: NodeBase;
    leftParen: Terminal;
    args: NodeList<CallArgument>;
    rightParen: Terminal;

    constructor(identifier: NodeBase, leftParen: Terminal, args: NodeList<CallArgument>, rightParen: Terminal) {
        super(mergeRanges(identifier, rightParen));
        this.identifier = identifier;
        this.leftParen = leftParen;
        this.args = args;
        this.rightParen = rightParen;
    }
}

class CallArgument extends NodeBase {
    expr: NodeBase;
    comma: Terminal | null;
    constructor(expr: NodeBase, comma: Terminal | null) {
        super(mergeRanges(expr, comma));
        this.expr = expr;
        this.comma = comma ?? null;
    }
}

interface Assignee {
    equals: Terminal;
    value: NodeBase;
}
class Assignment extends NodeBase {
    finalModifier: Terminal | null;
    varModifier: Terminal | null;
    baseTarget: NodeBase;
    assignmentChain: Assignee[];
    constructor(finalModifier: Terminal | null, varModifier: Terminal | null, baseTarget: NodeBase, assignmentChain: Assignee[]) {
        if (assignmentChain.length == 0) throw "assignment chain must have at least one element";

        super(mergeRanges(finalModifier, varModifier, baseTarget, assignmentChain[assignmentChain.length-1].value));

        this.finalModifier = finalModifier;
        this.varModifier = varModifier;
        this.baseTarget = baseTarget;
        this.assignmentChain = assignmentChain;
    }
}

const enum UnaryOperatorPos { pre, post };
const enum UnaryOpType { inc, dec, pos, neg };
class UnaryOperator extends NodeBase {
    pos: UnaryOperatorPos;
    optype: UnaryOpType;
    operator: Terminal;
    expr: NodeBase;
    constructor(expr: NodeBase, op: Terminal);
    constructor(op: Terminal, expr: NodeBase);
    constructor(lexicallyFirst: Terminal | NodeBase, lexicallyAfter: Terminal | NodeBase) {
        super(mergeRanges(lexicallyFirst, lexicallyAfter));
        if (lexicallyFirst instanceof Terminal) {
            this.pos = UnaryOperatorPos.pre;
            this.optype = UnaryOperator.tokenTypeToOpType(lexicallyFirst.token.type);
            this.operator = lexicallyFirst;
            this.expr = lexicallyAfter;
        }
        else {
            this.pos = UnaryOperatorPos.post;
            this.optype = UnaryOperator.tokenTypeToOpType((lexicallyAfter as Terminal).token.type);
            this.operator = lexicallyAfter as Terminal;
            this.expr = lexicallyFirst;
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

class NumericLiteral extends NodeBase {
    literal: Terminal;
    constructor(literal: Terminal) {
        super(literal.range);
        this.literal = literal;
    }
}

class Identifier extends NodeBase {
    identifier: Terminal;
    canonicalName: string;
    constructor(identifier: Terminal, name: string) {
        super(identifier.range);
        this.identifier = identifier;
        this.canonicalName = name.toLowerCase();
    }
}

type AccessElement =
    | DotAccess
    | BracketAccess;

interface DotAccess {
    dot: Terminal;
    propertyName: StringLiteral;
}
interface BracketAccess {
    leftBracket: Terminal;
    expr: NodeBase;
    rightBracket: Terminal;
}

class IndexedAccess extends NodeBase {
    root: NodeBase;
    accessElements: AccessElement[] = [];
    constructor(root: NodeBase) {
        super(root.range);
        this.root = root;
    }

    pushAccessElement(dot: Terminal, propertyName: StringLiteral) : void;
    pushAccessElement(leftBracket: Terminal, expr: NodeBase, rightBracket: Terminal) : void;
    pushAccessElement(dotOrBracket: Terminal, expr: NodeBase | StringLiteral, rightBracket?: Terminal) : void {
        if (rightBracket) { // bracket access
            this.accessElements.push({
                leftBracket: dotOrBracket,
                expr: expr,
                rightBracket: rightBracket
            });
        }
        else { // dot access
            this.accessElements.push({
                dot: dotOrBracket,
                propertyName: (expr as StringLiteral)
            });
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

    tagMode() : boolean {
        return this.mode_ === TokenizerMode.tag;
    }
    scriptMode() : boolean {
        return this.mode_ === TokenizerMode.script;
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
        const maybeTerminal = this.parseOptionalTerminal(type, parseOptions);
        if (maybeTerminal) {
            return maybeTerminal;
        }
        else {
            this.parseErrorAtCurrentPos("Expected a <??> here");
            const phonyToken : Token = new Token(type, SourceRange.Nil());
            return this.createMissingNode(new Terminal(phonyToken));
        }
    }

    parseTagComment() : CfTag.Comment {
        const commentStart = this.parseExpectedTerminal(TokenType.CF_TAG_COMMENT_START, ParseOptions.noTrivia);
        const commentBody = this.parseTagTrivia();
        const commentEnd = this.parseExpectedTerminal(TokenType.CF_TAG_COMMENT_END, ParseOptions.noTrivia);

        // if no comment end, emit an error

        return new CfTag.Comment(commentStart, commentBody, commentEnd);
    }

    parseScriptSingleLineComment() : NodeBase {
        throw "nyi";
    }

    parseScriptMultiLineComment() : NodeBase {
        throw "nyi";
    }

    parseTrivia() : NodeList<NodeBase> {
        if (this.tagMode()) {
            return this.parseTagTrivia();
        }

        const result = new NodeList<NodeBase>();
        while (true) {
            switch (this.lookahead()) {
                case TokenType.DBL_FORWARD_SLASH:
                    result.list.push(this.parseScriptSingleLineComment());
                    continue;
                case TokenType.FORWARD_SLASH_STAR:
                    result.list.push(this.parseScriptMultiLineComment());
                    continue;
                case TokenType.WHITESPACE:
                    result.list.push(new TextSpan(this.next().range));
                    continue;
            }
            break;
        }
        return result;
    }

    parseTagTrivia() : NodeList<CfTag.TagBase> {
        const result = new NodeList<CfTag.TagBase>();
        while (true) {
            switch (this.lookahead()) {
                case TokenType.CF_TAG_COMMENT_START: {
                    result.list.push(this.parseTagComment());
                    continue;
                }
                case TokenType.WHITESPACE: {
                    result.list.push(new CfTag.Text(this.next().range));
                    continue;
                }
            }
            // if we didn't match tag comment start or whitespace, we're done
            break;
        }
        return result;
    }

    parseCfStartTag() {
        const tagStart = this.parseExpectedTerminal(TokenType.CF_START_TAG_START, ParseOptions.noTrivia);
        const tagName = this.parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
        const canonicalName = this.tokenizer_.getTokenText(tagName.token).toLowerCase();

        if (canonicalName === "if" || canonicalName === "elseif") {
            const expr = this.parseExpression();
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
                    value = this.parseExpression();
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
                    break;
                }
                case TokenType.CF_END_TAG_START: {
                    finishTagTextRange();
                    result.list.push(this.parseCfEndTag());
                    break;
                }
                case TokenType.CF_TAG_COMMENT_START: {
                    finishTagTextRange();
                    result.list.push(this.parseTagComment());
                    break;
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
                        const expr = this.parseExpression();
                        const rightHash = this.parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
                        result.list.push(new HashWrappedExpr(leftHash, expr, rightHash));
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

    isAssignmentTarget<T extends NodeBase>(node: T) : boolean {
        switch (node.constructor) {
            case IndexedAccess:
            case Identifier:
                return true;
            case HashWrappedExpr:
                return this.isAssignmentTarget((node as unknown as HashWrappedExpr).expr);
            default:
                return false;
        }
    }

    parseAssignmentOrLower() : NodeBase {
        const finalModifier = this.parseOptionalTerminal(TokenType.KW_FINAL, ParseOptions.withTrivia);
        const varModifier = this.parseOptionalTerminal(TokenType.KW_VAR, ParseOptions.withTrivia);
        const root = this.parseCallExpressionOrLower();

        if (this.lookahead() != TokenType.EQUAL) {
            return root;
        }
        if (!this.isAssignmentTarget(root)) {
            // if we had a final or var modifer we need to mark an error, but this is otherwise OK
		    // "left-hand side of an assignment must be an assignment target"
            return root;
        }

        const assignmentChain : Assignee[] = [];
        do {
            assignmentChain.push({
                equals: this.parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia),
                value: this.parseExpression()
            });
        } while (this.lookahead() === TokenType.EQUAL && this.isAssignmentTarget(assignmentChain[assignmentChain.length-1].value));

        return new Assignment(finalModifier, varModifier, root, assignmentChain);
    }

    parseExpression() : NodeBase {
        let root = this.parseBooleanExpression();

        while (true) {
            if (this.tagMode() && this.lookahead() === TokenType.LEFT_ANGLE || this.lookahead() === TokenType.RIGHT_ANGLE) {
                break;
            }
            switch (this.lookahead()) {
                case TokenType.DBL_EQUAL:      		// [[fallthrough]];
                case TokenType.LIT_EQ:        		// [[fallthrough]];
                case TokenType.EXCLAMATION_EQUAL:	// [[fallthrough]];
                case TokenType.LIT_NEQ:           	// [[fallthrough]];
    
                case TokenType.LEFT_ANGLE:    		// [[fallthrough]]; // invalid in tag mode, but we've already checked for it
                case TokenType.LIT_LT:              // [[fallthrough]];
                case TokenType.LEFT_ANGLE_EQUAL: 	// [[fallthrough]];
                case TokenType.LIT_LTE:				// [[fallthrough]];
                case TokenType.LIT_LE:				// [[fallthrough]];
    
                case TokenType.RIGHT_ANGLE:    		// [[fallthrough]]; // invalid in tag mode, but we've already checked for it
                case TokenType.LIT_GT:				// [[fallthrough]];
                case TokenType.RIGHT_ANGLE_EQUAL: 	// [[fallthrough]];
                case TokenType.LIT_GTE:				// [[fallthrough]];
                case TokenType.LIT_GE: {
                    const op = this.parseExpectedTerminal(this.lookahead(), ParseOptions.withTrivia);
                    const right = this.parseExpression();
                    root = new BinaryOperator(root, op, right);
                    continue;
                }
            }
            // if we didn't match any of the above tokens, we're done
            break;
        }

        return root;
    }

    parseBooleanExpression(descendIntoOr = true) : NodeBase {
        let root = this.parseAddition();

        outer:
        while (true) {
            switch (this.lookahead()) {
                case TokenType.DBL_PIPE:
                case TokenType.LIT_OR:
                case TokenType.LIT_XOR: {
                    if (!descendIntoOr) break outer;
                    const op = this.parseExpectedTerminal(this.lookahead(), ParseOptions.withTrivia);
                    const expr = this.parseAddition();
                    root = new BinaryOperator(root, op, expr);
                    continue;
                }
                case TokenType.DBL_AMPERSAND:
                case TokenType.LIT_AND: {
                    const op = this.parseExpectedTerminal(this.lookahead(), ParseOptions.withTrivia);
                    const expr = this.parseBooleanExpression(/*descendIntoOr*/ false);
                    root = new BinaryOperator(root, op, expr);
                    continue;
                }
            }
            // if we didn't match any of the above operators, we're done
            break;
        }

        return root;
    }

    parseAddition() {
        let root = this.parseMultiplication();

        while (true) {
            switch (this.lookahead()) {
                case TokenType.PLUS:
                case TokenType.MINUS:
                case TokenType.AMPERSAND: {
                    const op = this.parseExpectedTerminal(this.lookahead(), ParseOptions.withTrivia);
                    const expr = this.parseMultiplication();
                    root = new BinaryOperator(root, op, expr);
                    continue;
                }
            }
            // if we didn't match any of the above operators, we're done
            break;
        }

        return root;
    }

    parseMultiplication() : NodeBase {
        const stack : NodeBase[] = [];

        // bind "^" (exponentiation) right
        function reduceRight() : void {
            if (stack.length === 1) return;

            // the stack should always have an odd number of elements,
            // (expr (op expr (op expr ...) ...) ...)
            while (stack.length > 1 && (stack[stack.length-2] as Terminal).token.type === TokenType.CARET) {
                const reduced = new BinaryOperator(
                    stack[stack.length - 3],
                    stack[stack.length - 2] as Terminal,
                    stack[stack.length - 1]);
                stack.splice(stack.length - 3, 3);
                stack.push(reduced);
            }
        }

        stack.push(this.parseParentheticalOrUnaryPrefix());

        while (true) {
            switch (this.lookahead()) {
                case TokenType.STAR:
                case TokenType.FORWARD_SLASH: {
                    reduceRight();
                    const op = this.parseExpectedTerminal(this.lookahead(), ParseOptions.withTrivia);
                    const expr = this.parseParentheticalOrUnaryPrefix();
                    stack.push(op, expr);
                    continue;
                }
                case TokenType.CARET: {
                    const op = this.parseExpectedTerminal(this.lookahead(), ParseOptions.withTrivia);
                    const expr = this.parseParentheticalOrUnaryPrefix();
                    stack.push(op, expr);
                    continue;
                }
            }
            break;
        }

        if (stack.length % 2 !== 1) {
            throw "stack should be odd-sized";
        }

        if (stack.length === 1) {
            return stack[0];
        }
        else {
            let result = new BinaryOperator(stack[0], stack[1] as Terminal, stack[2]);
            for (let i = 3; i < stack.length; i += 2) {
                result = new BinaryOperator(result, stack[i] as Terminal, stack[i+1]);
                i += 2;
            }
            return result;
        }
    }

    parseParentheticalOrUnaryPrefix() : NodeBase {
        switch (this.lookahead()) {
            case TokenType.LEFT_PAREN: {
                const leftParen = this.parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                const expr = this.parseExpression();
                const rightParen = this.parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                return new Parenthetical(leftParen, expr, rightParen);
            }
            case TokenType.DBL_MINUS: // [[fallthrough]];
            case TokenType.DBL_PLUS: {
                const op = this.parseExpectedTerminal(this.lookahead(), ParseOptions.withTrivia);
                const expr = this.parseCallExpressionOrLower();
                return new UnaryOperator(op, expr);
            }
            default: return this.parseCallExpressionOrLower();
        }
    }

    parseCallExpressionOrLower() : NodeBase {
        switch(this.lookahead()) {
            case TokenType.NUMBER: return this.parseNumericLiteral();
            case TokenType.QUOTE_DOUBLE: // fallthrough
            case TokenType.QUOTE_SINGLE: return this.parseStringLiteral()
            default: break;
        }

        let root = this.parseIdentifier(ParseOptions.allowHashWrapped);
        if (root instanceof HashWrappedExpr) {
            return root;
        }

        while (this.lookahead() != TokenType.EOF) {
            switch(this.lookahead()) {
                case TokenType.LEFT_BRACE:
                case TokenType.DOT:
                    root = this.parseIndexedAccess(root);
                    continue;
                case TokenType.LEFT_PAREN:
                    root = this.parseCallExpression(root);
                    continue;
            }
            break;
        }

        if (root instanceof CallExpression) {
            switch (this.lookahead()) {
                case TokenType.DBL_PLUS:
                case TokenType.DBL_MINUS:
                    const unaryOp = this.parseExpectedTerminal(this.lookahead(), ParseOptions.withTrivia);
                    root = new UnaryOperator(root, unaryOp);
            }
        }

        return root;
    }

    parseIndexedAccess(root: NodeBase) : NodeBase {
        while (this.lookahead() != TokenType.EOF) {
            switch (this.lookahead()) {
                case TokenType.LEFT_BRACKET: {
                    const leftBracket = this.parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                    const expr = this.parseExpression();
                    const rightBracket = this.parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);

                    if (!(root instanceof IndexedAccess)) {
                        root = new IndexedAccess(root);
                    }
                    (root as IndexedAccess).pushAccessElement(leftBracket, expr, rightBracket);

                    continue;
                }
                case TokenType.DOT: {
                    const dot = this.parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
                    const propertyName = this.parseStringLiteral(ParseOptions.undelimitedString);
                    if (!(root instanceof IndexedAccess)) {
                        root = new IndexedAccess(root);
                    }
                    (root as IndexedAccess).pushAccessElement(dot, propertyName);
                    continue;
                }
            }
            break;
        }
        return root;
    }

    parseCallExpression(root: NodeBase) {
        const leftParen = this.parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const args = new NodeList<CallArgument>();
        while (this.lookahead() != TokenType.EOF && this.lookahead() != TokenType.RIGHT_PAREN) {
            const expr = this.parseExpression();
            const comma = this.parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            args.list.push(new CallArgument(expr, comma));
        }
        const rightParen = this.parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        return new CallExpression(root, leftParen, args, rightParen);
    }

    parseIdentifier(parseOptions : ParseOptions) : NodeBase {
        let leftHash : Terminal | null = null;
        if (parseOptions & ParseOptions.allowHashWrapped) {
            leftHash = this.parseOptionalTerminal(TokenType.HASH, ParseOptions.withTrivia);
        }

        const identifier = this.parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
        const name = this.tokenizer_.getTokenText(identifier.token);

        if (parseOptions & ParseOptions.allowHashWrapped && leftHash) {
            const rightHash = this.parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
            return new HashWrappedExpr(
                leftHash,
                new Identifier(identifier, name),
                rightHash);
        }
        else {
            return new Identifier(identifier, name);
        }
    }

    parseStringLiteral(parseOptions: ParseOptions = ParseOptions.none) : StringLiteral {
        if (parseOptions & ParseOptions.undelimitedString) { // e.g., 'foo' in `<cfwhatever attr=foo>`
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

    parseNumericLiteral() {
        return new NumericLiteral(this.parseExpectedTerminal(TokenType.NUMBER, ParseOptions.withTrivia));
    }
}
}