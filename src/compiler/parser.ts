import { SourceRange } from "./scanner";
import { Token, TokenType, TokenizerMode, Tokenizer, TokenTypeUiString } from "./tokenizer";

const enum TagFact {
    ALLOW_VOID		= 0x00000001, // tag can be void, e.g., <cfhttp> can be loose, or have a body like <cfhttp><cfhttpparam></cfhttp>
	REQUIRE_VOID    = 0x00000002, // tag is always just a loose tag, whether marked with a void-slash or not
	DISALLOW_VOID   = 0x00000004, // the body can be length 0, but it needs to have a matching end tag

	// helpful "inverse" aliases
	ALLOW_BODY = ALLOW_VOID,
	REQUIRE_BODY = DISALLOW_VOID,
	DISALLOW_BODY = REQUIRE_VOID
}
const tagFacts = {
    // the "cf" prefix is implied
    abort:          TagFact.DISALLOW_BODY,
    application:    TagFact.DISALLOW_BODY,
    argument:       TagFact.DISALLOW_BODY,
    break:          TagFact.DISALLOW_BODY,
    catch:          TagFact.DISALLOW_BODY,
    content:        TagFact.DISALLOW_BODY,
    continue:       TagFact.DISALLOW_BODY,
    cookie:         TagFact.DISALLOW_BODY,
    directory:      TagFact.DISALLOW_BODY,
    documentitem:   TagFact.DISALLOW_BODY,
    dump:           TagFact.DISALLOW_BODY,
    error:          TagFact.DISALLOW_BODY,
    exit:           TagFact.DISALLOW_BODY,
    file:           TagFact.ALLOW_BODY,
    function:       TagFact.REQUIRE_BODY,
    header:         TagFact.DISALLOW_BODY,
    http:           TagFact.ALLOW_BODY,
    httpparam:      TagFact.DISALLOW_BODY,
    include:        TagFact.DISALLOW_BODY,
    input:          TagFact.DISALLOW_BODY,
    invoke:         TagFact.ALLOW_BODY,
    invokeargument: TagFact.DISALLOW_BODY,
    location:       TagFact.DISALLOW_BODY,
    log:            TagFact.DISALLOW_BODY,
    mail:           TagFact.REQUIRE_BODY,
    mailparam:      TagFact.DISALLOW_BODY,
    output:         TagFact.REQUIRE_BODY,
    param:          TagFact.DISALLOW_BODY,
    pdf:            TagFact.ALLOW_BODY,
    pdfform:        TagFact.DISALLOW_BODY,
    pdfformparam:   TagFact.DISALLOW_BODY,
    pdfparam:       TagFact.DISALLOW_BODY,
    procparam:      TagFact.DISALLOW_BODY,
    procresult:     TagFact.DISALLOW_BODY,
    property:       TagFact.DISALLOW_BODY,
    queryparam:     TagFact.DISALLOW_BODY,
    query:          TagFact.REQUIRE_BODY,
    reportparam:    TagFact.DISALLOW_BODY,
    rethrow:        TagFact.DISALLOW_BODY,
    return:         TagFact.DISALLOW_BODY,
    set:            TagFact.DISALLOW_BODY,
    setting:        TagFact.DISALLOW_BODY,
    spreadsheet:    TagFact.DISALLOW_BODY,
    storedproc:     TagFact.ALLOW_BODY,
    throw:          TagFact.DISALLOW_BODY,
    trace:          TagFact.DISALLOW_BODY,
    transaction:    TagFact.ALLOW_BODY,
    wddx:           TagFact.ALLOW_BODY,
    zip:            TagFact.ALLOW_BODY,
    zipparam:       TagFact.DISALLOW_BODY,
} as const;

function getTagFacts(tag: CfTag.TagBase) : TagFact | null {
    if (tagFacts.hasOwnProperty(tag.canonicalName)) {
        return tagFacts[tag.canonicalName as keyof typeof tagFacts]
    }
    return null;
}

function requiresEndTag(tag: CfTag.TagBase) : boolean {
	const facts = getTagFacts(tag);
    return !!facts && !!(facts & TagFact.REQUIRE_BODY);
}

function allowTagBody(tag: CfTag.TagBase) {
    const facts = getTagFacts(tag);
    return !!facts && (
        (facts & TagFact.ALLOW_BODY) || !!(facts & TagFact.REQUIRE_BODY));
}

const enum NodeFlags {
    none    = 0,
    error   = 0x00000001,
    missing = 0x00000002
}

export class NodeBase {
    parent: NodeBase | null;
    range: SourceRange;
    tagOrigin: {
        startTag: CfTag.TagBase | null,
        endTag: CfTag.TagBase | null
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

export class NodeList<T extends NodeBase> extends NodeBase {    
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
        return new Terminal(Token.Nil());
    }
}

export class TextSpan extends NodeBase {
    constructor(sourceRange: SourceRange) {
        super(sourceRange);
    }
}

export class HashWrappedExpr extends NodeBase {
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

export class Parenthetical extends NodeBase {
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

export class TagAttribute extends NodeBase {
    name: Terminal;
    equals: Terminal | null;
    expr: NodeBase | null;

    lcName: string;

    constructor(name: Terminal, lcName: string);
    constructor(name: Terminal, lcName: string, equals: Terminal, expr: NodeBase);
    constructor(name: Terminal, lcName: string, equals?: Terminal, expr?: NodeBase | undefined) {
        if (name && equals && expr) {
            super(mergeRanges(name, expr));
        }
        else {
            super(name.range);
        }
        this.name = name;
        this.equals = equals ?? null;
        this.expr = expr ?? null;
        this.lcName = lcName;
    }
}

export namespace CfTag {
    export const enum Which { start, end };
    //
    // end tags are expected to be "common" tags; they should not have attributes or etc.
    //
    export abstract class TagBase extends NodeBase {
        which: Which;
        tagStart: Terminal;         // <cf | </cf
        tagName: Terminal;          // terminal for "script" | "if" | "param", etc.; the "cf" is implied
        voidSlash: Terminal | null; // trailing "/" in "/>", if present
        tagEnd: Terminal;           // ">"
        canonicalName: string;      // string representation of tagName

        constructor(
            which: Which,
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
        constructor(which: Which.start, tagStart: Terminal, tagName: Terminal, voidSlash: Terminal | null, tagEnd: Terminal, canonicalName: string, attrs: NodeList<TagAttribute>);
        constructor(which: Which.end, tagStart: Terminal, tagName: Terminal, voidSlash: Terminal | null, tagEnd: Terminal, canonicalName: string);
        constructor(
            which: Which,
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
            which: Which.start,
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
            tagStart: Terminal,
            tagName: Terminal,
            voidSlash: Terminal | null,
            tagEnd: Terminal,
            canonicalName: string,
            stmtList: NodeList<NodeBase>) {
            super(Which.start, tagStart, tagName, voidSlash, tagEnd, canonicalName);
            this.stmtList = stmtList;
        }
    }
    export class Text extends TagBase { // text ranges
        // just interested in the node's range and uniquely identifying it as tag-text
        constructor(range: SourceRange) {
            const nilTerminal = Terminal.Nil();
            super(Which.start, nilTerminal, nilTerminal, null, nilTerminal, "");
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
            super(Which.start, tagStart, nilTerminal, nilTerminal, tagEnd, "");
            this.body = body;
        }
    }
    export function assertIsScriptLike(tag: TagBase) : asserts tag is ScriptLike {
        if (tag instanceof ScriptLike) return;
        else throw "tag was expected to be ScriptLike";
    }
    export function assertIsCommon(tag: TagBase) : asserts tag is Common {
        if (tag instanceof Common) return;
        else throw "tag was expected to be Common";
    }
    export function assertIsScript(tag: TagBase) : asserts tag is Script {
        if (tag instanceof Script) return;
        else throw "tag was expected to be Script";
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

const enum ConditionalSubtype { if, elseif, else };
class Conditional extends NodeBase {
    ifToken     : Terminal | null;
    elseToken   : Terminal | null;
    leftParen   : Terminal | null;
    expr        : NodeBase | null;
    rightParen  : Terminal | null;
    consequent  : NodeList<NodeBase>;
    alternative : Conditional | null;

    constructor(subtype: ConditionalSubtype, fromTag: CfTag.TagBase, consequent: NodeList<NodeBase>) {
        super(fromTag.range);
        this.ifToken     = null;
        this.elseToken   = null;
        this.leftParen   = null;
        this.rightParen  = null;
        this.consequent  = consequent;
        this.alternative = null;
        this.tagOrigin.startTag = fromTag;

        if (subtype === ConditionalSubtype.if || subtype === ConditionalSubtype.elseif) {
            CfTag.assertIsScriptLike(fromTag);
            this.expr = fromTag.expr;
        }
        else /* tag is an else tag */ {
            this.expr = null;
        }
    }
}

export class Statement extends NodeBase {
    stmt : NodeBase | null;
    semicolon : Terminal | null;
    constructor(tag: CfTag.TagBase) {
        super(tag.range);
        CfTag.assertIsCommon(tag);
        this.stmt = null;
        this.tagOrigin.startTag = tag;
        this.semicolon = null;
        //
		// will probably need to determine which of the "cf-built-in" statements this is;
		// or maybe the caller will have to do that, and constructing from "any start tag" doesn't make sense
		//
    }
}

export class NamedBlock extends NodeBase {
    name: Terminal | null;
    attrs: NodeList<TagAttribute>;
    leftBrace: Terminal | null;
    stmtList: NodeList<NodeBase>;
    rightBrace: Terminal | null;

    constructor(startTag: CfTag.TagBase, endTag: CfTag.TagBase);
    constructor(startTag: CfTag.TagBase, stmtList: NodeList<NodeBase>, endTag: CfTag.TagBase);
    constructor(startTag: CfTag.TagBase, endTagOrStmtList: CfTag.TagBase | NodeList<NodeBase>, endTag?: CfTag.TagBase) {
        if (endTag) {
            super(mergeRanges(startTag, endTag));
            this.tagOrigin.startTag = startTag;
            this.tagOrigin.endTag = endTag;
            this.name = null;
            this.attrs = new NodeList<TagAttribute>();
            this.leftBrace = null;
            this.stmtList = endTagOrStmtList as NodeList<NodeBase>;
            this.rightBrace = null;
        }
        else {
            super(mergeRanges(startTag, endTagOrStmtList));
            this.tagOrigin.startTag = startTag;
            this.tagOrigin.endTag = endTagOrStmtList as CfTag.TagBase;
            this.name = null;
            this.attrs = new NodeList<TagAttribute>();
            this.leftBrace = null;
            this.stmtList = new NodeList<NodeBase>();
            this.rightBrace = null;
        }
    }
}

// BasicString
// InterpolatedString

export class StringLiteral extends NodeBase {
    //
    // it is possible to have an "unquoted" string literal, e.g.,
    // for tag attribute values like <cffunction name="x" foo=bar>
    // where bar is an unquoted string literal value
    // a null delimiter indicates this case
    //
    delimiter: TokenType.QUOTE_DOUBLE | TokenType.QUOTE_SINGLE | null;
    leftQuote: Terminal | null;
    elements: NodeList<NodeBase>; // TextSpan | HashWrappedExpr
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

export class NumericLiteral extends NodeBase {
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

class FunctionParameter extends NodeBase {
    type: null;
    requiredTerminal: Terminal | null;
    identifier: Identifier | null;
    equals: Terminal | null;
    defaultValue: NodeBase | null;
    comma: Terminal | null;

    name: string | null;
    required: boolean;

    constructor(tag: CfTag.Common, paramName: string | null, required: boolean) {
        super(tag.range);
        this.tagOrigin.startTag = tag;

        this.type = null;
        this.requiredTerminal = null;
        this.identifier = null;
        this.equals = null;

        this.defaultValue = null; // findAttr(tag.attrs, "default") ?? null;
        this.comma = null;

        this.name = paramName;
        this.required = required;
    }
}

class FunctionDefinition extends NodeBase {
    attrs: TagAttribute[];
    params: FunctionParameter[];

    name: string;

    constructor(startTag: CfTag.Common, params: FunctionParameter[], body: NodeList<NodeBase>, endTag: CfTag.Common, name: string) {
        super(mergeRanges(startTag, endTag));
        this.tagOrigin.startTag = startTag;
        this.tagOrigin.endTag = endTag;

        this.attrs = startTag.attrs.list;
        this.params = params;
        this.name = name;
    }
}

const enum ParseOptions {
    none     = 0,
    noTrivia = 0,
    withTrivia        = 0x00000001,
    allowHashWrapped  = 0x00000002,
    undelimitedString = 0x00000004,
};

function TagContext() {
    return {
        depth: {
            output: 0,
            mail: 0,
            query: 0
        },
        inTextInterpolationContext() {
            return this.depth.output > 0
                || this.depth.mail > 0
                || this.depth.query > 0;
        }
    }
}
type TagContext = ReturnType<typeof TagContext>;
interface TokenizerState {
    mode : TokenizerMode;
    index: number;
    artificialEndLimit: number | undefined;
}

export interface Diagnostic {
    fileName: string;
    fromInclusive: number;
    toExclusive: number;
    msg: string;
}

export function Parser(tokenizer_: Tokenizer, mode_: TokenizerMode = TokenizerMode.tag) {
    let tokenizer : Tokenizer = tokenizer_;
    let mode: TokenizerMode = mode_;
    let lookahead_ : TokenType = peek().type;
    let globalDiagnosticEmitter : (() => void) | null = null;

    const diagnostics : Diagnostic[] = [];
    let parseErrorBeforeNextFinishedNode = false;
    parseErrorBeforeNextFinishedNode;

    function peek(jump: number = 0) {
        return tokenizer.peek(jump, mode);
    }

    function lookahead() {
        return lookahead_;
    }

    function next() {
        const result = tokenizer.next(mode);
        lookahead_ = peek().type;
        return result;
    }

    function getTokenizerState() : TokenizerState {
        return {
            index: tokenizer.getIndex(),
            mode: mode,
            artificialEndLimit: tokenizer.getArtificalEndLimit()
        }
    }

    function restoreTokenizerState(state: TokenizerState) {
        tokenizer.restoreIndex(state.index);
        mode = state.mode;
        if (state.artificialEndLimit) {
            tokenizer.setArtificialEndLimit(state.artificialEndLimit);
        }
        else {
            tokenizer.clearArtificalEndLimit();
        }
        lookahead_ = peek().type;
    }

    function tagMode() : boolean {
        return mode === TokenizerMode.tag;
    }
    /*function scriptMode() : boolean {
        return mode === TokenizerMode.script;
    }*/


    function parseErrorAtRange(range: SourceRange, msg: string) : void;
    function parseErrorAtRange(fromInclusive: number, toExclusive: number, msg: string) : void;
    function parseErrorAtRange(fromInclusiveOrRange: number | SourceRange, toExclusiveOrMsg: number | string, msg?: string) : void {
        parseErrorBeforeNextFinishedNode = true;
        diagnostics.push({
            fileName: "<NYI>",
            fromInclusive: typeof fromInclusiveOrRange === "number"
                ? fromInclusiveOrRange
                : fromInclusiveOrRange.fromInclusive,
            toExclusive: typeof toExclusiveOrMsg === "number"
                ? toExclusiveOrMsg
                : (fromInclusiveOrRange as SourceRange).toExclusive,
            msg: msg ?? (toExclusiveOrMsg as string)
        });
    }

    function parseErrorAtCurrentToken(msg: string) : void {
        const sourceRange = peek().range;
        parseErrorAtRange(sourceRange.fromInclusive, sourceRange.toExclusive, msg);
    }

    function createMissingNode<T extends NodeBase>(node: T) {
        node.flags |= NodeFlags.error | NodeFlags.missing;
        return node;
    }

    function parseOptionalTerminal(type: TokenType, parseOptions: ParseOptions) : Terminal | null {
        if (lookahead() === type) {
            const token = next();
            if (parseOptions & ParseOptions.withTrivia) {
                return new Terminal(token, parseTrivia());
            }
            else {
                return new Terminal(token);
            }
        }
        else {
            return null;
        }
    }

    function parseExpectedTerminal(type: TokenType, parseOptions: ParseOptions, localDiagnosticEmitter?: () => void) : Terminal {
        const maybeTerminal = parseOptionalTerminal(type, parseOptions);
        if (maybeTerminal) {
            return maybeTerminal;
        }
        else {
            if (localDiagnosticEmitter) {
                localDiagnosticEmitter();
            }
            else if (globalDiagnosticEmitter) {
                globalDiagnosticEmitter();
            }
            else {
                parseErrorAtCurrentToken("Expected a " + TokenTypeUiString[type] + " here");
            }
            const phonyToken : Token = new Token(type, "", SourceRange.Nil());
            return createMissingNode(new Terminal(phonyToken));
        }
    }

    function parseTagComment() : CfTag.Comment {
        const commentStart = parseExpectedTerminal(TokenType.CF_TAG_COMMENT_START, ParseOptions.noTrivia);
        const commentBody = parseTagTrivia();
        const commentEnd = parseExpectedTerminal(TokenType.CF_TAG_COMMENT_END, ParseOptions.noTrivia);

        // if no comment end, emit an error

        return new CfTag.Comment(commentStart, commentBody, commentEnd);
    }

    function parseScriptSingleLineComment() : NodeBase {
        throw "nyi";
    }

    function parseScriptMultiLineComment() : NodeBase {
        throw "nyi";
    }

    function parseTrivia() : NodeList<NodeBase> {
        if (tagMode()) {
            return parseTagTrivia();
        }

        const result = new NodeList<NodeBase>();
        while (true) {
            switch (lookahead()) {
                case TokenType.DBL_FORWARD_SLASH:
                    result.list.push(parseScriptSingleLineComment());
                    continue;
                case TokenType.FORWARD_SLASH_STAR:
                    result.list.push(parseScriptMultiLineComment());
                    continue;
                case TokenType.WHITESPACE:
                    result.list.push(new TextSpan(next().range)); // not really any need to store the whitespace
                    continue;
            }
            break;
        }
        return result;
    }

    function parseTagTrivia() : NodeList<CfTag.TagBase> {
        const result = new NodeList<CfTag.TagBase>();
        while (true) {
            switch (lookahead()) {
                case TokenType.CF_TAG_COMMENT_START: {
                    result.list.push(parseTagComment());
                    continue;
                }
                case TokenType.WHITESPACE: {
                    result.list.push(new CfTag.Text(next().range));
                    continue;
                }
            }
            // if we didn't match tag comment start or whitespace, we're done
            break;
        }
        return result;
    }

    function parseCfStartTag() {
        const tagStart = parseExpectedTerminal(TokenType.CF_START_TAG_START, ParseOptions.noTrivia);
        const tagName = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
        const canonicalName = tokenizer.getTokenText(tagName.token).toLowerCase();

        if (canonicalName === "if" || canonicalName === "elseif") {
            const expr = parseExpression();
            const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
            return new CfTag.ScriptLike(CfTag.Which.start, tagStart, tagName, null, rightAngle, canonicalName, expr);
        }
        else if (canonicalName === "set") {
            const expr = parseAssignmentOrLower();
            const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
            return new CfTag.ScriptLike(CfTag.Which.start, tagStart, tagName, null, rightAngle, canonicalName, expr);
        }
        else {
            const tagAttrs = parseTagAttributes();
            const maybeVoidSlash = parseOptionalTerminal(TokenType.FORWARD_SLASH, ParseOptions.withTrivia);
            const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
            return new CfTag.Common(CfTag.Which.start, tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, tagAttrs);
        }
    }

    function parseCfEndTag() {
        const tagStart = parseExpectedTerminal(TokenType.CF_END_TAG_START, ParseOptions.noTrivia);
        const tagName = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
        const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);

        let canonicalName = "";
        if (!(tagName.flags & NodeFlags.missing)) {
            canonicalName = tokenizer.getTokenText(tagName.token).toLowerCase();
        }
        return new CfTag.Common(CfTag.Which.end, tagStart, tagName, null, rightAngle, canonicalName);
    }

    function parseTagAttributes() : NodeList<TagAttribute> {
        const result = new NodeList<TagAttribute>();

        while (lookahead() === TokenType.LEXEME) {
            const attrName = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
            if (lookahead() === TokenType.EQUAL) {
                const equal = parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                let value : NodeBase;
                if (lookahead() === TokenType.LEXEME) {
                    value = parseStringLiteral(ParseOptions.undelimitedString);
                }
                else {
                    value = parseExpression();
                }
                result.list.push(new TagAttribute(attrName, tokenizer_.getTokenText(attrName.token).toLowerCase(), equal, value));
            }
            else {
                result.list.push(new TagAttribute(attrName, tokenizer_.getTokenText(attrName.token).toLowerCase()));
            }
        }

        return result;
    }

    function parseTags() : NodeList<NodeBase> {
        //
        // tag treeifier
        // after parsing tags, we end up with just a list of tags
        // we need to go through a second time and turn them into a tree
        // we convert text spans into interpolated text spans here if necessary
        // as well as convert all tags into either blocks or statements;
        // a tag with children becomes a block, a single tag (like a <cfset ...> or just a loose <cffile ...> ) becomes a statement
        // this also gives us the opportunity to emit diagnostics for unbalanced start or end tags
        //
        function treeifyTagList(tagList: NodeList<CfTag.TagBase>) : NodeList<NodeBase> {
            const tagContext = TagContext();
            let index = 0;

            const enum ReductionScheme { default, return };
            interface ReductionInstruction {
                onHitWhich: CfTag.Which,
                onHitName: string,
                reduction: ReductionScheme;
            }
            const reductionInstructions = {
                cfif: <readonly ReductionInstruction[]>[
                    { onHitWhich: CfTag.Which.start, onHitName: "elseif", reduction: ReductionScheme.return },
                    { onHitWhich: CfTag.Which.start, onHitName: "else",   reduction: ReductionScheme.return },
                    { onHitWhich: CfTag.Which.end,   onHitName: "if",     reduction: ReductionScheme.return }
                ],
                cfelseif: <readonly ReductionInstruction[]>[
                    { onHitWhich: CfTag.Which.start, onHitName: "elseif", reduction: ReductionScheme.return },
                    { onHitWhich: CfTag.Which.start, onHitName: "else",   reduction: ReductionScheme.return },
                    { onHitWhich: CfTag.Which.end,   onHitName: "if",     reduction: ReductionScheme.return }
                ],
                cfelse: <readonly ReductionInstruction[]>[
                    {onHitWhich: CfTag.Which.end, onHitName: "if", reduction: ReductionScheme.return }
                ],
                default: <readonly ReductionInstruction[]>[]
            };

            function updateTagContext(tag: CfTag.TagBase) {
                let bumpDir = tag.which === CfTag.Which.start ? 1 : -1;
                switch (tag.canonicalName) {
                    case "output":
                    case "mail":
                    case "query": {
                        tagContext.depth[tag.canonicalName] += bumpDir;
                    }
                }
            }

            function stopAt(which: CfTag.Which, name: string, reduction: ReductionScheme) : ReductionInstruction[] {
                return [{
                    onHitWhich: which,
                    onHitName: name,
                    reduction: reduction
                }]
            }

            function getReductionScheme(tag: CfTag.TagBase, instructions: readonly ReductionInstruction[]) : ReductionScheme {
                for (const instr of instructions) {
                    if (instr.onHitWhich === tag.which && instr.onHitName === tag.canonicalName) {
                        return instr.reduction;
                    }
                }
                return ReductionScheme.default;
            }

            function hasNextTag() : boolean {
                return index < tagList.list.length;
            }
            function nextTag() {
                return tagList.list[index++];
            }
            function peekTag() {
                return hasNextTag() ? tagList.list[index] : null;
            }

            function parseOptionalTag(which: CfTag.Which, canonicalName: string) : CfTag.TagBase | null {
                if (hasNextTag() && tagList.list[index].which === which && tagList.list[index].canonicalName === canonicalName) {
                    return nextTag();
                }
                else {
                    return null;
                }
            }

            function parseExpectedTag(which: CfTag.Which, canonicalName: string, diagnosticEmitter?: () => void) : CfTag.TagBase {
                const tag = parseOptionalTag(which, canonicalName);
                if (tag) {
                    return tag;
                }
                else {
                    if (diagnosticEmitter) {
                        diagnosticEmitter();
                    }
                    else {
                        const msg = `Expected a <${which === CfTag.Which.end ? "/" : ""}${canonicalName}> tag here`;
                        let range = hasNextTag() ? peekTag()!.range : tagList.list[tagList.list.length-1].range;
                        parseErrorAtRange(range, msg);
                    }

                    // create fake placeholder tag
                    let missingTag : CfTag.Common;
                    if (which === CfTag.Which.start) {
                        missingTag = new CfTag.Common(which, Terminal.Nil(), Terminal.Nil(), null, Terminal.Nil(), canonicalName, new NodeList<TagAttribute>())
                    }
                    else {
                        missingTag = new CfTag.Common(which, Terminal.Nil(), Terminal.Nil(), null, Terminal.Nil(), canonicalName)
                    }
                    createMissingNode(missingTag);
                    return missingTag;
                }
            }

            function treeifyConditionalTag() {
                const ifTag = parseExpectedTag(CfTag.Which.start, "if");
                const rootConsequent = treeifyTags(reductionInstructions.cfif);
                let root = new Conditional(ConditionalSubtype.if, ifTag, rootConsequent);

                let working = root;

                while (true) {
                    const elseIfTag = parseOptionalTag(CfTag.Which.start, "elseif");
                    if (elseIfTag) {
                        const consequent = treeifyTags(reductionInstructions.cfelseif);
                        working.alternative = new Conditional(ConditionalSubtype.elseif, elseIfTag, consequent);
                        working = root.alternative!;
                        continue;
                    }
                    const elseTag = parseOptionalTag(CfTag.Which.start, "else");
                    if (elseTag) {
                        const consequent = treeifyTags(reductionInstructions.cfelse);
                        working.alternative = new Conditional(ConditionalSubtype.else, elseTag, consequent);
                    }
                    break;
                }

                if (hasNextTag()) {
                    const nextTag = peekTag()!;
                    if (nextTag.canonicalName === "if" && nextTag.which === CfTag.Which.end) {
                        root.tagOrigin.endTag = parseExpectedTag(CfTag.Which.end, "if");
                        return root;
                    }
                }

                parseErrorAtRange(root.range, "Missing </cfif> tag");
                return root;
            }

            /*function parseTagFunctionParameter() {
                const tag = parseExpectedTag(CfTag.Which.start, "argument") as CfTag.Common; // <cfargument ...> defines the function's parameters
                const name = findAttr(tag.attrs, "name");
                if (!name) {
                    parseErrorAtRange(tag.range, "<cfargument> requires a 'name' attribute");
                }
                //
                // check that the name is a string, after stripping off parens and hashes
                // if (isStaticallyComputableStringValue(name)) ... or something like that
                // then (isValidIdentifier(name)) !!
                //
                const required = findAttr(tag.attrs, "required");
                required;
                //
                // need something like "isStaticallyComputableBoolean(value)" and then get the value
                //
                //

                const validatedName = "<statically-computable|null>";
                const validatedRequired = false;

                return new FunctionParameter(tag, validatedName, validatedRequired);
            }*/

            /*function parseTagFunction() : NodeBase {
                const startTag = parseExpectedTag(CfTag.Which.start, "function") as CfTag.Common;
                const params : FunctionParameter[] = [];

                while (true) {
                    if (peekTag()?.canonicalName === "argument") {
                        params.push(parseTagFunctionParameter());
                    }
                    else {
                        break;
                    }
                }

                const body = treeifyTags(stopAt(CfTag.Which.end, "function", ReductionScheme.return));
                const endTag = parseExpectedTag(CfTag.Which.end, "function", () => parseErrorAtRange(startTag.range, "Missing </cffunction> tag")) as CfTag.Common;

                //
                // if !isStaticallyVerifiable(name)....
                //
                const name = findAttr(startTag.attrs, "name");
                if (!name) {
                    parseErrorAtRange(startTag.range, "<cffunction> requires a 'name' attribute")
                }
                const validatedName = "<some-name|null>"
                return new FunctionDefinition(startTag, params, body, endTag, validatedName);
            }*/

            function treeifyTags(reductionInstructions: readonly ReductionInstruction[]) : NodeList<NodeBase> {
                const result = new NodeList<NodeBase>();

                // any CfTags left in the result, which were in there to act as anchors that end tags could match against,
                // get turned into statements
                // all text and comments by now should have been turned into TextSpans
                // is returning an r-value reasonable? goal is to use this as the return statement, like `return finalize();`
                //
                // probably want to leave the tags ... then we can match on them
                function finalize(from = 0) {
                    /*for (let i = from; i < result.list.length; i++) {
                        if (result.list[i] instanceof CfTag.TagBase) {
                            result.list[i] = new Statement(result.list[i] as CfTag.TagBase);
                        }
                    }*/
                    return result;
                }

                function localStackFindMatchingStartTag(tag: CfTag.TagBase) : number | null {
                    for (let i = result.list.length - 1; i >= 0; i--) {
                        if (result.list[i] instanceof CfTag.TagBase) {
                            const stackTag = result.list[i] as CfTag.TagBase;
                            if (stackTag.which === CfTag.Which.end && stackTag.canonicalName === tag.canonicalName) {
                                return i;
                            }
                        }
                    }
                    return null;
                }

                while (hasNextTag()) {
                    const tag = peekTag()!;
                    updateTagContext(tag);

                    if (tag instanceof CfTag.Text) {
                        parseTextInTagContext(tag.range, tagContext);
                        nextTag()
                        continue;
                    }
                    else if (tag.which === CfTag.Which.start) {
                        const reductionScheme = getReductionScheme(tag, reductionInstructions);
                        switch (reductionScheme) {
                            case ReductionScheme.return:
                                return finalize();
                            case ReductionScheme.default:
                                // ok, no-op: the default reduction scheme for a start tag is to do nothing
                                break;
                        }

                        switch (tag.canonicalName) {
                            case "if": {
                                result.list.push(treeifyConditionalTag());
                                continue;
                            }
                            case "set": {
                                CfTag.assertIsScriptLike(tag);
                                const expr = tag.expr;
                                expr.tagOrigin.startTag = tag;
                                result.list.push(expr);
                                nextTag();
                                continue;
                            }
                            case "script": {
                                // same as handling set, except there is an end tag to consume
                                CfTag.assertIsScript(tag);
                                const stmtList = tag.stmtList;
                                stmtList.tagOrigin.startTag = tag;
                                nextTag();
                                stmtList.tagOrigin.endTag = parseExpectedTag(CfTag.Which.end, "script");

                                // so our result now has at least one list in it, looking something like (node | node[])[]
                                result.list.push(stmtList);
                                continue;
                            }
                            default: {
                                if (requiresEndTag(tag)) {
                                    const startTag = tag;
                                    nextTag();
                                    const blockChildren = treeifyTags(stopAt(CfTag.Which.end, startTag.canonicalName, ReductionScheme.return));
                                    if (!hasNextTag()) {
                                        parseErrorAtRange(tag.range, "no matching end tag for cf" + tag.canonicalName);
                                    }
                                    else {
                                        const endTag = parseExpectedTag(CfTag.Which.end, startTag.canonicalName);
                                        updateTagContext(endTag);
                                        result.list.push(new NamedBlock(startTag, blockChildren, endTag));
                                    }
                                }
                                else {
                                    // a single loose tag
                                    // it will become a statement
                                    // e.g., <cfhttp args /> is essentially a call to a fictitious "cfhttp(args)" function, except it is a statement instead of a value producing expression
                                    // but first we push it into the result as a tag, so that any possible matching end tags can walk up the local results list
                                    // and find it 
                                    result.list.push(tag);
                                    nextTag();
                                }
                            }
                        }
                    }
                    else if (tag.which === CfTag.Which.end) {
                        const reductionScheme = getReductionScheme(tag, reductionInstructions);
                        if (reductionScheme === ReductionScheme.return) {
                                return finalize();
                        }

                        // if we can't find the target tag in our local result, this tag has no matching start tag
                        // if this tag isn't allowed to have a block body, it's an error, and gets discarded
                        if (!allowTagBody(tag)) {
                            // parse error - end tag for tag type that does not support tag bodys (like cfargument)
                            nextTag();
                            continue;
                        }

                        const maybeMatchingStartTagIndex = localStackFindMatchingStartTag(tag);
                        if (maybeMatchingStartTagIndex) {
                            const matchingStartTagIndex = maybeMatchingStartTagIndex;
                            const matchingStartTag = result.list[matchingStartTagIndex] as CfTag.TagBase;

                            //
                            // finalize so any remaining loose tag bodies are statement-ized
                            // move (matching_tag + 0) to be the head of a new block
                            // move everything from (matching_tag+1) into the new block as children
                            // use current tag as end of tag block
                            // once everything has been moved into their new homes, drop the original references
                            // from our local results list
                            //
					        
                            finalize(matchingStartTagIndex + 1);
                            const blockChildren = new NodeList<NodeBase>(result.list.slice(matchingStartTagIndex + 1));
                            result.list.splice(matchingStartTagIndex)
                            result.list.push(new NamedBlock(matchingStartTag, blockChildren, tag));
                        }
                        else {
                            parseErrorAtRange(tag.range, "End tag without a matching start tag (cf" + tag.canonicalName + ")")
                        }
                        nextTag();
                    }
                }

                return finalize();
            }

            return treeifyTags(reductionInstructions.default);
        }

        const saveMode = mode;
        mode = TokenizerMode.tag;

        const result = new NodeList<CfTag.TagBase>();
        let tagTextRange = SourceRange.Nil();

        function startOrContinueTagTextRange() {
            if (tagTextRange.isNil()) {
                const index = tokenizer.getIndex();
                tagTextRange = new SourceRange(index, index+1);
            }
        }
        function finishTagTextRange() {
            if (tagTextRange.isNil()) {
                return;
            }
            tagTextRange.toExclusive = tokenizer.getIndex(); // does not include current; so, no "+1"
            result.list.push(new CfTag.Text(tagTextRange))
            tagTextRange = SourceRange.Nil();
        }

        while (lookahead_ != TokenType.EOF) {
            switch (lookahead_) {
                case TokenType.CF_START_TAG_START: {
                    finishTagTextRange();
                    const tag = parseCfStartTag();
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
                    result.list.push(parseCfEndTag());
                    break;
                }
                case TokenType.CF_TAG_COMMENT_START: {
                    finishTagTextRange();
                    result.list.push(parseTagComment());
                    break;
                }
                default: {
                    startOrContinueTagTextRange();
                    next();
                }
            }
        }

        mode = saveMode;

        return treeifyTagList(result);
    }

    function parseTextInTagContext(range: SourceRange, tagContext: TagContext) {
        if (!tagContext.inTextInterpolationContext()) {
            return new TextSpan(range);
        }
        const saveTokenizerState = getTokenizerState();
        restoreTokenizerState({
            index: range.fromInclusive,
            mode: TokenizerMode.tag,
            artificialEndLimit: range.toExclusive});
        
        const result = parseTextWithPossibleInterpolations();
        restoreTokenizerState(saveTokenizerState);
        return result;
    }

    function parseTextWithPossibleInterpolations(quoteDelimiter?: TokenType.QUOTE_SINGLE | TokenType.QUOTE_DOUBLE) {
        const result = new NodeList<NodeBase>();
        let textSourceRange = SourceRange.Nil();

        function startOrContinueTextRange() {
            if (textSourceRange.isNil()) {
                const index = tokenizer.getIndex();
                textSourceRange = new SourceRange(index, index+1);
            }
            // continuing is a no-op; when we finish the text range, we'll update the "toExclusive"
            // with the tokenizer's current index
        }
        function finishTextRange() {
            // if we hadn't started a range yet, we're done
            if (textSourceRange.isNil()) {
                return;
            }
            textSourceRange.toExclusive = tokenizer.getIndex(); // current index is NOT included, so, no '+1'
            result.list.push(new TextSpan(textSourceRange));
            textSourceRange = SourceRange.Nil();
        }

        //
        // there is no anchor to stop at other than EOF if the delimiter undefined
        // we rely on the caller setting the tokenizer to have an artifical range limit, such that we
        // eat only some pre-determined range of what we consider to be valid text
        //
        while (lookahead() != TokenType.EOF) {
            switch (lookahead()) {
                case quoteDelimiter: { // doubled-up delimiter; meaning it is an escaped quote
                    if (peek(1).type === quoteDelimiter) {
                        startOrContinueTextRange();
                        next(), next();
                        continue;
                    }
                    else { // single delimiter; we're done, don't eat it because the caller will need it
                        finishTextRange();
                        return result;
                    }
                }
                case TokenType.HASH: {
                    if (peek(1).type === TokenType.HASH) { // doubled up hash, meaning it is an escaped hash symbol
                        startOrContinueTextRange();
                        next(), next();
                        continue;
                    }
                    else { // single hash, meaning this is an interpolated string element
                        finishTextRange();
                        const leftHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
                        const expr = parseExpression();
                        const rightHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
                        result.list.push(new HashWrappedExpr(leftHash, expr, rightHash));
                        continue;
                    }
                }
                default: {
                    startOrContinueTextRange();
                    next();
                }
            }
        }
        
        finishTextRange();
        return result;
    }

    function isAssignmentTarget<T extends NodeBase>(node: T) : boolean {
        switch (node.constructor) {
            case IndexedAccess:
            case Identifier:
                return true;
            case HashWrappedExpr:
                return isAssignmentTarget((node as unknown as HashWrappedExpr).expr);
            default:
                return false;
        }
    }

    function parseAssignmentOrLower() : NodeBase {
        const finalModifier = parseOptionalTerminal(TokenType.KW_FINAL, ParseOptions.withTrivia);
        const varModifier = parseOptionalTerminal(TokenType.KW_VAR, ParseOptions.withTrivia);
        const root = parseCallExpressionOrLower();

        if (lookahead() != TokenType.EQUAL) {
            return root;
        }
        if (!isAssignmentTarget(root)) {
            // if we had a final or var modifer we need to mark an error, but this is otherwise OK
		    // "left-hand side of an assignment must be an assignment target"
            return root;
        }

        const assignmentChain : Assignee[] = [];
        do {
            assignmentChain.push({
                equals: parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia),
                value: parseExpression()
            });
        } while (lookahead() === TokenType.EQUAL && isAssignmentTarget(assignmentChain[assignmentChain.length-1].value));

        return new Assignment(finalModifier, varModifier, root, assignmentChain);
    }

    function parseExpression() : NodeBase {
        const saveDiagnosticEmitter = globalDiagnosticEmitter;
        const currentPos = tokenizer_.getIndex();
        globalDiagnosticEmitter = () => parseErrorAtRange(currentPos, tokenizer_.getIndex(), "Expected an expression");

        let root = parseBooleanExpression();

        while (true) {
            if (tagMode() && lookahead() === TokenType.LEFT_ANGLE || lookahead() === TokenType.RIGHT_ANGLE) {
                break;
            }
            switch (lookahead()) {
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
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const right = parseExpression();
                    root = new BinaryOperator(root, op, right);
                    continue;
                }
            }
            // if we didn't match any of the above tokens, we're done
            break;
        }

        globalDiagnosticEmitter = saveDiagnosticEmitter;

        return root;
    }

    function parseBooleanExpression(descendIntoOr = true) : NodeBase {
        let root = parseAddition();

        outer:
        while (true) {
            switch (lookahead()) {
                case TokenType.DBL_PIPE:
                case TokenType.LIT_OR:
                case TokenType.LIT_XOR: {
                    if (!descendIntoOr) break outer;
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseAddition();
                    root = new BinaryOperator(root, op, expr);
                    continue;
                }
                case TokenType.DBL_AMPERSAND:
                case TokenType.LIT_AND: {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseBooleanExpression(/*descendIntoOr*/ false);
                    root = new BinaryOperator(root, op, expr);
                    continue;
                }
            }
            // if we didn't match any of the above operators, we're done
            break;
        }

        return root;
    }

    function parseAddition() {
        let root = parseMultiplication();

        while (true) {
            switch (lookahead()) {
                case TokenType.PLUS:
                case TokenType.MINUS:
                case TokenType.AMPERSAND: {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseMultiplication();
                    root = new BinaryOperator(root, op, expr);
                    continue;
                }
            }
            // if we didn't match any of the above operators, we're done
            break;
        }

        return root;
    }

    function parseMultiplication() : NodeBase {
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

        stack.push(parseParentheticalOrUnaryPrefix());

        while (true) {
            switch (lookahead()) {
                case TokenType.STAR:
                case TokenType.FORWARD_SLASH: {
                    reduceRight();
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseParentheticalOrUnaryPrefix();
                    stack.push(op, expr);
                    continue;
                }
                case TokenType.CARET: {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseParentheticalOrUnaryPrefix();
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

    function parseParentheticalOrUnaryPrefix() : NodeBase {
        switch (lookahead()) {
            case TokenType.LEFT_PAREN: {
                const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                const expr = parseExpression();
                const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                return new Parenthetical(leftParen, expr, rightParen);
            }
            case TokenType.DBL_MINUS: // [[fallthrough]];
            case TokenType.DBL_PLUS: {
                const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                const expr = parseCallExpressionOrLower();
                return new UnaryOperator(op, expr);
            }
            default: return parseCallExpressionOrLower();
        }
    }

    function parseCallExpressionOrLower() : NodeBase {
        switch(lookahead()) {
            case TokenType.NUMBER: return parseNumericLiteral();
            case TokenType.QUOTE_DOUBLE: // fallthrough
            case TokenType.QUOTE_SINGLE: return parseStringLiteral()
            default: break;
        }

        let root = parseIdentifier(ParseOptions.allowHashWrapped);
        if (root instanceof HashWrappedExpr) {
            return root;
        }

        while (lookahead() != TokenType.EOF) {
            switch(lookahead()) {
                case TokenType.LEFT_BRACE:
                case TokenType.DOT:
                    root = parseIndexedAccess(root);
                    continue;
                case TokenType.LEFT_PAREN:
                    root = parseCallExpression(root);
                    continue;
            }
            break;
        }

        if (root instanceof CallExpression) {
            switch (lookahead()) {
                case TokenType.DBL_PLUS:
                case TokenType.DBL_MINUS:
                    const unaryOp = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    root = new UnaryOperator(root, unaryOp);
            }
        }

        return root;
    }

    function parseIndexedAccess(root: NodeBase) : NodeBase {
        while (lookahead() != TokenType.EOF) {
            switch (lookahead()) {
                case TokenType.LEFT_BRACKET: {
                    const leftBracket = parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                    const expr = parseExpression();
                    const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);

                    if (!(root instanceof IndexedAccess)) {
                        root = new IndexedAccess(root);
                    }
                    (root as IndexedAccess).pushAccessElement(leftBracket, expr, rightBracket);

                    continue;
                }
                case TokenType.DOT: {
                    const dot = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
                    const propertyName = parseStringLiteral(ParseOptions.undelimitedString);
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

    function parseCallExpression(root: NodeBase) {
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const args = new NodeList<CallArgument>();
        while (lookahead() != TokenType.EOF && lookahead() != TokenType.RIGHT_PAREN) {
            const expr = parseExpression();
            const comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            args.list.push(new CallArgument(expr, comma));
        }
        const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        return new CallExpression(root, leftParen, args, rightParen);
    }

    function parseIdentifier(parseOptions : ParseOptions) : NodeBase {
        let leftHash : Terminal | null = null;
        if (parseOptions & ParseOptions.allowHashWrapped) {
            leftHash = parseOptionalTerminal(TokenType.HASH, ParseOptions.withTrivia);
        }

        const identifier = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
        const name = tokenizer.getTokenText(identifier.token);

        if (parseOptions & ParseOptions.allowHashWrapped && leftHash) {
            const rightHash = parseExpectedTerminal(
                TokenType.HASH,
                ParseOptions.withTrivia,
                () => parseErrorAtRange(
                    leftHash!.range.fromInclusive,
                    tokenizer_.getIndex(),
                    "Unterminated hash wrapped expression"));

            return new HashWrappedExpr(
                leftHash,
                new Identifier(identifier, name),
                rightHash);
        }
        else {
            return new Identifier(identifier, name);
        }
    }

    function parseStringLiteral(parseOptions: ParseOptions = ParseOptions.none) : StringLiteral {
        if (parseOptions & ParseOptions.undelimitedString) { // e.g., 'foo' in `<cfwhatever attr=foo>`
            const unquotedString = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
            return new StringLiteral(unquotedString);
        }
        else {
            const quoteType = lookahead();
            if (quoteType !== TokenType.QUOTE_SINGLE && quoteType !== TokenType.QUOTE_DOUBLE) {
                // will a lookahead or speculate ever trigger this ... ?
                throw "AssertionFailure: parseStringLiteral called on input without valid string delimiter";
            }

            const leftQuote = parseExpectedTerminal(quoteType, ParseOptions.noTrivia);
            const stringElements = parseTextWithPossibleInterpolations(quoteType);
            const rightQuote = parseExpectedTerminal(quoteType, ParseOptions.withTrivia);
            return new StringLiteral(quoteType, leftQuote, stringElements, rightQuote);
        }
    }

    function parseNumericLiteral() {
        return new NumericLiteral(parseExpectedTerminal(TokenType.NUMBER, ParseOptions.withTrivia));
    }

    function getDiagnostics() : readonly Diagnostic[] {
        return diagnostics;
    }

    return {
        parseTags,
        getDiagnostics
    }
}
