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
    else:           TagFact.REQUIRE_BODY,
    elseif:         TagFact.REQUIRE_BODY,
    error:          TagFact.DISALLOW_BODY,
    exit:           TagFact.DISALLOW_BODY,
    file:           TagFact.ALLOW_BODY,
    function:       TagFact.REQUIRE_BODY,
    header:         TagFact.DISALLOW_BODY,
    http:           TagFact.ALLOW_BODY,
    httpparam:      TagFact.DISALLOW_BODY,
    if:             TagFact.REQUIRE_BODY,
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

function getTagFacts(tag: CfTag) : TagFact | null {
    if (tagFacts.hasOwnProperty(tag.canonicalName)) {
        return tagFacts[tag.canonicalName as keyof typeof tagFacts]
    }
    return null;
}

function requiresEndTag(tag: CfTag) : boolean {
	const facts = getTagFacts(tag);
    return !!facts && !!(facts & TagFact.REQUIRE_BODY);
}

function allowTagBody(tag: CfTag) {
    const facts = getTagFacts(tag);
    return !!facts && (
        (facts & TagFact.ALLOW_BODY) || !!(facts & TagFact.REQUIRE_BODY));
}

const enum NodeFlags {
    none    = 0,
    error   = 0x00000001,
    missing = 0x00000002
}

const enum NodeType {
    terminal, list, textSpan, comment, hashWrappedExpr, parenthetical, tagAttribute,
    tag, callExpression, callArgument, assignment, unaryOperator, binaryOperator,
    conditional, statement, namedBlock, simpleStringLiteral, interpolatedStringLiteral, numericLiteral, booleanLiteral,
    identifier, indexedAccess, functionParameter, functionDefinition
}

type Node =
    | Terminal
    | Comment
    | TextSpan
    | HashWrappedExpr
    | Parenthetical
    | TagAttribute
    | CfTag
    | CallExpression
    | CallArgument
    | Assignment
    | UnaryOperator
    | BinaryOperator
    | Conditional
    | Statement
    | NamedBlock
    | SimpleStringLiteral
    | InterpolatedStringLiteral
    | NumericLiteral
    | BooleanLiteral
    | Identifier
    | IndexedAccess
    | FunctionParameter
    | FunctionDefinition

interface NodeBase {
    type: NodeType;
    parent: Node | null,
    range: SourceRange,
    tagOrigin: {
        startTag: CfTag | null,
        endTag: CfTag | null,
    }
    flags: NodeFlags,
}

export function NodeBase<T extends NodeBase>(type: T["type"], range: SourceRange = SourceRange.Nil()) : T {
    const result : Partial<T> = {};
    result.type = type;
    result.parent = null;
    result.range = range ?? null;
    result.tagOrigin = {
        startTag: null,
        endTag: null,
    }
    result.flags = NodeFlags.none;
    return result as T;
}

function mergeRanges(...nodes : (SourceRange | Node | Node[] | undefined | null )[]) : SourceRange {
    const result = SourceRange.Nil();
    if (nodes.length === 0) {
        return result;
    }

    let gotStart = false;
    for (const node of nodes) {
        if (!node) continue;

        let thisRange : SourceRange;
        if (Array.isArray(node)) {
            if (node.length === 0) continue;
            thisRange = mergeRanges(...node);
        }
        else if (node instanceof SourceRange) {
            thisRange = node;
        }
        else {
            thisRange = node.range;
        }

        if (!gotStart) {
            result.fromInclusive = thisRange.fromInclusive;
            result.toExclusive = thisRange.toExclusive;
            gotStart = true;
        }
        else {
            result.toExclusive = thisRange.toExclusive;
        }
    }

    return result;
}

export interface Terminal extends NodeBase {
    type: NodeType.terminal;
    token: Token;
    trivia: Node[];
}

export function Terminal(token: Token, trivia: Node[] = []) : Terminal {
    const terminal = NodeBase<Terminal>(NodeType.terminal, mergeRanges(token.range, trivia));
    terminal.token = token;
    terminal.trivia = trivia;
    return terminal;
}

const NilTerminal : Readonly<Terminal> = (() => {
    const terminal = NodeBase<Terminal>(NodeType.terminal, SourceRange.Nil());
    terminal.token = Token.Nil();
    terminal.trivia = [];
    return terminal;
})();


export const enum CommentType { tag, scriptSingleLine, scriptMultiLine };

export interface Comment extends NodeBase {
    type: NodeType.comment;
    commentType: CommentType;
}

export function Comment(commentType: CommentType, range: SourceRange) {
    const comment = NodeBase<Comment>(NodeType.comment, range);
    comment.commentType = commentType;
    return comment;
}

export interface TextSpan extends NodeBase {
    type: NodeType.textSpan;
    text: string;
}
export function TextSpan(sourceRange: SourceRange, text: string) : TextSpan {
    const textSpan = NodeBase<TextSpan>(NodeType.textSpan, sourceRange);
    textSpan.text = text;
    return textSpan;
}

export interface HashWrappedExpr extends NodeBase {
    type: NodeType.hashWrappedExpr;
    leftHash: Terminal;
    expr: Node;
    rightHash: Terminal;
}
export function HashWrappedExpr(leftHash: Terminal, expr: Node, rightHash: Terminal) : HashWrappedExpr {
    const hashWrappedExpr = NodeBase<HashWrappedExpr>(NodeType.hashWrappedExpr, mergeRanges(leftHash, rightHash));
    hashWrappedExpr.leftHash = leftHash;
    hashWrappedExpr.expr = expr;
    hashWrappedExpr.rightHash = rightHash;
    return hashWrappedExpr;
}

export interface Parenthetical extends NodeBase {
    type: NodeType.parenthetical;
    leftParen: Terminal;
    expr: Node;
    rightParen: Terminal;
}
export function Parenthetical(leftParen: Terminal, expr: Node, rightParen: Terminal) : Parenthetical {
    const parentWrappedExpr = NodeBase<Parenthetical>(NodeType.parenthetical, mergeRanges(leftParen, rightParen));
    parentWrappedExpr.leftParen = leftParen;
    parentWrappedExpr.expr = expr;
    parentWrappedExpr.rightParen = rightParen;
    return parentWrappedExpr;
}

export interface TagAttribute extends NodeBase {
    type: NodeType.tagAttribute;
    name: Terminal;
    equals: Terminal | null;
    expr: Node | null;

    lcName: string; // mapify so we don't need this
}
export function TagAttribute(name: Terminal, lcName: string) : TagAttribute;
export function TagAttribute(name: Terminal, lcName: string, equals: Terminal, expr: Node) : TagAttribute;
export function TagAttribute(name: Terminal, lcName: string, equals?: Terminal, expr?: Node | undefined) : TagAttribute {
    let tagAttr : TagAttribute;
    if (name && equals && expr) {
        tagAttr = NodeBase<TagAttribute>(NodeType.tagAttribute, mergeRanges(name, expr));
    }
    else {
        tagAttr = NodeBase<TagAttribute>(NodeType.tagAttribute, name.range);
    }

    tagAttr.name = name;
    tagAttr.equals = equals ?? null;
    tagAttr.expr = expr ?? null;
    tagAttr.lcName = lcName;
    return tagAttr;
}

export type CfTag =
    | CfTag.Common
    | CfTag.ScriptLike
    | CfTag.Script
    | CfTag.Text
    | CfTag.Comment;

export namespace CfTag {
    export const enum Which { start, end };
    export const enum TagType { common, scriptLike, script, text, comment };
    //
    // end tags are expected to be "common" tags; they should not have attributes or etc.
    //
    export interface TagBase extends NodeBase {
        type: NodeType.tag;
        which: Which;
        tagType: TagType;
        tagStart: Terminal;         // <cf | </cf
        tagName: Terminal;          // terminal for "script" | "if" | "param", etc.; the "cf" is implied
        voidSlash: Terminal | null; // trailing "/" in "/>", if present
        tagEnd: Terminal;           // ">"
        canonicalName: string;      // string representation of tagName
    }
    function TagBase<T extends TagBase>(
        which: Which,
        tagType: TagType,
        tagStart: Terminal,
        tagName: Terminal,
        voidSlash: Terminal | null,
        tagEnd: Terminal,
        canonicalName: string) : T {

        const tag = NodeBase<TagBase>(NodeType.tag, mergeRanges(tagStart, tagEnd));
        tag.which = which;
        tag.tagType = tagType;
        tag.tagStart = tagStart;
        tag.tagName = tagName;
        tag.voidSlash = voidSlash;
        tag.tagEnd = tagEnd;
        tag.canonicalName = canonicalName;
        return tag as T;
    }

    export interface Common extends TagBase {
        tagType: TagType.common;
        attrs: TagAttribute[];
    }
    export function Common(which: Which.start, tagStart: Terminal, tagName: Terminal, voidSlash: Terminal | null, tagEnd: Terminal, canonicalName: string, attrs: TagAttribute[]) : Common;
    export function Common(which: Which.end, tagStart: Terminal, tagName: Terminal, voidSlash: Terminal | null, tagEnd: Terminal, canonicalName: string) : Common;
    export function Common(
        which: Which,
        tagStart: Terminal,
        tagName: Terminal,
        voidSlash: Terminal | null,
        tagEnd: Terminal,
        canonicalName: string,
        attrs?: TagAttribute[]) : Common {
        const tag = TagBase<Common>(which, TagType.common, tagStart, tagName, voidSlash, tagEnd, canonicalName);
        tag.attrs = attrs ?? []
        return tag;
    }

    export interface ScriptLike extends TagBase {
        tagType: TagType.scriptLike;
        expr: Node;
    }
    export function ScriptLike(
        which: Which.start,
        tagStart: Terminal,
        tagName: Terminal,
        voidSlash: Terminal | null,
        tagEnd: Terminal,
        canonicalName: string,
        expr: Node) : ScriptLike {
        const v = TagBase<ScriptLike>(which, TagType.scriptLike, tagStart, tagName, voidSlash, tagEnd, canonicalName);
        v.expr = expr;
        return v;
    }

    export interface Script extends TagBase {
        tagType: TagType.script;
        stmtList: Node[];
    }
    export function Script(
        tagStart: Terminal,
        tagName: Terminal,
        voidSlash: Terminal | null,
        tagEnd: Terminal,
        canonicalName: string,
        stmtList: Node[]) : Script {
        const v = TagBase<Script>(Which.start, TagType.script, tagStart, tagName, voidSlash, tagEnd, canonicalName);
        v.stmtList = stmtList;
        return v;
    }

    export interface Text extends TagBase {
        tagType: TagType.text;
    }
    export function Text(range: SourceRange) : Text {
        const nilTerminal = NilTerminal;
        const v = TagBase<Text>(Which.start, TagType.text, nilTerminal, nilTerminal, null, nilTerminal, "");
        v.range = range;
        return v;
    }

    export interface Comment extends TagBase {
        tagType: TagType.comment;
        body: TagBase[];
    }
    export function Comment(
        tagStart: Terminal,
        body: TagBase[],
        tagEnd: Terminal) : Comment {
        const nilTerminal = NilTerminal;
        const v = TagBase<Comment>(Which.start, TagType.comment, tagStart, nilTerminal, nilTerminal, tagEnd, "");
        v.body = body;
        return v;
    }
}

export interface CallExpression extends NodeBase {
    type: NodeType.callExpression;
    identifier: Node;
    leftParen: Terminal;
    args: CallArgument[];
    rightParen: Terminal;

}
export function CallExpression(identifier: Node, leftParen: Terminal, args: CallArgument[], rightParen: Terminal) {
    const v = NodeBase<CallExpression>(NodeType.callExpression, mergeRanges(identifier, rightParen));
    v.identifier = identifier;
    v.leftParen = leftParen;
    v.args = args;
    v.rightParen = rightParen;
    return v;
}

export interface CallArgument extends NodeBase {
    type: NodeType.callArgument;
    expr: Node;
    comma: Terminal | null;
}

export function CallArgument(expr: Node, comma: Terminal | null) : CallArgument {
    const v = NodeBase<CallArgument>(NodeType.callArgument, mergeRanges(expr, comma));
    v.expr = expr;
    v.comma = comma;
    return v;
}

export interface Assignee {
    equals: Terminal;
    value: Node;
}
export interface Assignment extends NodeBase {
    type: NodeType.assignment;
    finalModifier: Terminal | null;
    varModifier: Terminal | null;
    baseTarget: Node;
    assignmentChain: Assignee[];
}

export function Assignment(finalModifier: Terminal | null, varModifier: Terminal | null, baseTarget: Node, assignmentChain: Assignee[]) : Assignment {
    if (assignmentChain.length == 0) throw "assignment chain must have at least one element";
    const v = NodeBase<Assignment>(NodeType.assignment, mergeRanges(finalModifier, varModifier, baseTarget, assignmentChain[assignmentChain.length-1].value));
    v.finalModifier = finalModifier;
    v.varModifier = varModifier;
    v.baseTarget = baseTarget;
    v.assignmentChain = assignmentChain;
    return v;
}

export const enum UnaryOperatorPos { pre, post };
export const enum UnaryOpType { inc, dec, pos, neg };
export interface UnaryOperator extends NodeBase {
    type: NodeType.unaryOperator;
    pos: UnaryOperatorPos;
    optype: UnaryOpType;
    operator: Terminal;
    expr: Node;
}

export function UnaryOperator(expr: Node, op: Terminal) : UnaryOperator;
export function UnaryOperator(op: Terminal, expr: Node) : UnaryOperator;
export function UnaryOperator(lexicallyFirst: Node, lexicallyAfter: Node) {
    const v = NodeBase<UnaryOperator>(NodeType.unaryOperator, mergeRanges(lexicallyFirst, lexicallyAfter));
    if (lexicallyFirst.type === NodeType.terminal) {
        v.pos = UnaryOperatorPos.pre;
        v.optype = tokenTypeToUnaryOpType(lexicallyFirst.token.type);
        v.operator = lexicallyFirst;
        v.expr = lexicallyAfter;
    }
    else {
        v.pos = UnaryOperatorPos.post;
        v.optype = tokenTypeToUnaryOpType((lexicallyAfter as Terminal).token.type);
        v.operator = lexicallyAfter as Terminal;
        v.expr = lexicallyFirst;
    }

    return v;
}

export function tokenTypeToUnaryOpType(tokenType: TokenType) {
    switch (tokenType) {
        case TokenType.DBL_MINUS: return UnaryOpType.dec;
        case TokenType.DBL_PLUS:  return UnaryOpType.inc;
        case TokenType.MINUS:     return UnaryOpType.neg;
        case TokenType.PLUS:      return UnaryOpType.pos;
        default: break;
    }
    throw "bad unary op type transform";
}

export const enum BinaryOpType {
    add, sub, mul, div, mod, exp, cat, eq, neq, lt, lte, gt, gte, 
}

export interface BinaryOperator extends NodeBase {
    type: NodeType.binaryOperator;
    optype: BinaryOpType;
    left: Node;
    operator: Terminal;
    right: Node;
}

export function BinaryOperator(left: Node, operator: Terminal, right: Node) : BinaryOperator {
    const v = NodeBase<BinaryOperator>(NodeType.binaryOperator, mergeRanges(left, right));
    v.left = left;
    v.operator = operator;
    v.right = right;
    v.optype = tokenTypeToBinaryOpType(operator.token.type);
    return v;
}

export function tokenTypeToBinaryOpType(tokenType: TokenType) {
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
        case TokenType.LIT_IS:              return BinaryOpType.eq;
        case TokenType.EXCLAMATION_EQUAL:	return BinaryOpType.neq;
        case TokenType.LIT_NEQ:				return BinaryOpType.neq;
        case TokenType.LIT_IS_NOT:          return BinaryOpType.neq;

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

export const enum ConditionalSubtype { if, elseif, else };
export interface Conditional extends NodeBase {
    type: NodeType.conditional;
    ifToken     : Terminal | null;
    elseToken   : Terminal | null;
    leftParen   : Terminal | null;
    expr        : Node | null;
    rightParen  : Terminal | null;
    consequent  : Node[];
    alternative : Conditional | null;
}

export function Conditional(subtype: ConditionalSubtype, fromTag: CfTag, consequent: Node[]) : Conditional {
    const v = NodeBase<Conditional>(NodeType.conditional, fromTag.range);
    v.ifToken     = null;
    v.elseToken   = null;
    v.leftParen   = null;
    v.rightParen  = null;
    v.consequent  = consequent;
    v.alternative = null;
    v.tagOrigin.startTag = fromTag;

    if (subtype === ConditionalSubtype.if || subtype === ConditionalSubtype.elseif) {
        if (fromTag.tagType !== CfTag.TagType.scriptLike) throw "conditional created from tag must be created from a script-like tag";
        v.expr = fromTag.expr;
    }
    else /* tag is an else tag */ {
        v.expr = null;
    }

    return v;
}

export interface Statement extends NodeBase {
    type: NodeType.statement;
    stmt: Node | null;
    semicolon : Terminal | null;
}

export function Statement(tag: CfTag) : Statement {
    const stmt = NodeBase<Statement>(NodeType.statement, tag.range);
    stmt.stmt = null;
    stmt.tagOrigin.startTag = tag;
    stmt.semicolon = null;
    return stmt;
    //
    // will probably need to determine which of the "cf-built-in" statements this is;
    // or maybe the caller will have to do that, and constructing from "any start tag" doesn't make sense
    //
}

export interface NamedBlock extends NodeBase {
    type: NodeType.namedBlock;
    name: Terminal | null;
    attrs: TagAttribute[];
    leftBrace: Terminal | null;
    stmtList: Node[];
    rightBrace: Terminal | null;
}

export function NamedBlock(startTag: CfTag, endTag: CfTag) : NamedBlock;
export function NamedBlock(startTag: CfTag, stmtList: Node[], endTag: CfTag) : NamedBlock;
export function NamedBlock(startTag: CfTag, endTagOrStmtList: CfTag | Node[], endTag?: CfTag) {
    if (endTag) {
        const v = NodeBase<NamedBlock>(NodeType.namedBlock, mergeRanges(startTag, endTag));
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag = endTag;
        v.name = null;
        v.attrs = [];
        v.leftBrace = null;
        v.stmtList = [];
        v.rightBrace = null;
        return v;
    }
    else {
        const v = NodeBase<NamedBlock>(NodeType.namedBlock, mergeRanges(startTag, endTagOrStmtList));
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag = endTagOrStmtList as CfTag;
        v.name = null;
        v.attrs = [];
        v.leftBrace = null;
        v.stmtList = [];
        v.rightBrace = null;
        return v;
    }
}

export interface SimpleStringLiteral extends NodeBase {
    type: NodeType.simpleStringLiteral;
    leftQuote : Terminal;
    textSpan : TextSpan;
    rightQuote: Terminal;
}

export function SimpleStringLiteral(
    quoteType: TokenType.QUOTE_SINGLE | TokenType.QUOTE_DOUBLE,
    leftQuote: Terminal,
    textSpan: TextSpan,
    rightQuote: Terminal) : SimpleStringLiteral {
    const v = NodeBase<SimpleStringLiteral>(NodeType.simpleStringLiteral, mergeRanges(leftQuote, rightQuote));
    v.leftQuote = leftQuote;
    v.textSpan = textSpan;
    v.rightQuote = rightQuote;
    return v;
}

export interface InterpolatedStringLiteral extends NodeBase {
    type: NodeType.interpolatedStringLiteral;
    delimiter: TokenType.QUOTE_DOUBLE | TokenType.QUOTE_SINGLE;
    leftQuote: Terminal;
    elements: (TextSpan | HashWrappedExpr)[];
    rightQuote: Terminal;
}

export function InterpolatedStringLiteral(
    quoteType: TokenType.QUOTE_DOUBLE | TokenType.QUOTE_SINGLE,
    leftQuote: Terminal,
    elements: (TextSpan | HashWrappedExpr)[],
    rightQuote: Terminal) : InterpolatedStringLiteral {

    const v = NodeBase<InterpolatedStringLiteral>(NodeType.interpolatedStringLiteral, mergeRanges(leftQuote, rightQuote));
    v.delimiter = quoteType;
    v.leftQuote = leftQuote;
    v.elements = elements;
    v.rightQuote = rightQuote;
    return v;
}

export interface NumericLiteral extends NodeBase {
    type: NodeType.numericLiteral;
    literal: Terminal;
}

export function NumericLiteral(literal: Terminal) : NumericLiteral {
    const v = NodeBase<NumericLiteral>(NodeType.numericLiteral, literal.range);
    v.literal = literal;
    return v;
}

export interface BooleanLiteral extends NodeBase {
    type: NodeType.booleanLiteral;
    literal: Terminal;
}

export function BooleanLiteral(literal: Terminal) {
    const v = NodeBase<BooleanLiteral>(NodeType.booleanLiteral, literal.range);
    v.literal = literal;
    return v;
}

export interface Identifier extends NodeBase {
    type: NodeType.identifier;
    identifier: Terminal;
    canonicalName: string;
}

export function Identifier(identifier: Terminal, name: string) {
    const v = NodeBase<Identifier>(NodeType.identifier, identifier.range);
    v.identifier = identifier;
    v.canonicalName = name.toLowerCase();
    return v;
}

export type AccessElement =
    | DotAccess
    | BracketAccess;

export interface DotAccess {
    dot: Terminal;
    propertyName: InterpolatedStringLiteral;
}

export interface BracketAccess {
    leftBracket: Terminal;
    expr: Node;
    rightBracket: Terminal;
}

export interface IndexedAccess extends NodeBase {
    type: NodeType.indexedAccess;
    root: Node;
    accessElements: AccessElement[];
}

export function IndexedAccess(root: Node) : IndexedAccess {
    const v = NodeBase<IndexedAccess>(NodeType.indexedAccess, root.range);
    v.root = root;
    return v;
}

export function pushAccessElement(base: IndexedAccess, dot: Terminal, propertyName: Terminal) : void;
export function pushAccessElement(base: IndexedAccess, leftBracket: Terminal, expr: Node, rightBracket: Terminal) : void;
export function pushAccessElement(base: IndexedAccess, dotOrBracket: Terminal, expr: Node | Terminal, rightBracket?: Terminal) : void {
    if (rightBracket) { // bracket access
        (<BracketAccess[]>base.accessElements).push({
            leftBracket: dotOrBracket,
            expr: expr,
            rightBracket: rightBracket
        });
    }
    else { // dot access
        (<DotAccess[]>base.accessElements).push({
            dot: dotOrBracket,
            propertyName: (expr as InterpolatedStringLiteral)
        });
    }
}

export interface FunctionParameter extends NodeBase {
    type: NodeType.functionParameter;
    returnType: null;
    requiredTerminal: Terminal | null;
    identifier: Identifier | null;
    equals: Terminal | null;
    defaultValue: Node | null;
    comma: Terminal | null;
    name: string | null;
    required: boolean;
}

export function FunctionParameter(tag: CfTag.Common, paramName: string | null, required: boolean) : FunctionParameter {
    const v = NodeBase<FunctionParameter>(NodeType.functionParameter, tag.range);
    v.tagOrigin.startTag = tag;
    v.returnType = null;
    v.requiredTerminal = null;
    v.identifier = null;
    v.equals = null;
    v.defaultValue = null; // findAttr(tag.attrs, "default") ?? null;
    v.comma = null;
    v.name = paramName;
    v.required = required;
    return v;
}

export interface FunctionDefinition extends NodeBase {
    type: NodeType.functionDefinition;
    attrs: TagAttribute[];
    params: FunctionParameter[];
    name: string;
}

export function FunctionDefinition(startTag: CfTag.Common, params: FunctionParameter[], body: Node[], endTag: CfTag.Common, name: string) : FunctionDefinition {
    const v = NodeBase<FunctionDefinition>(NodeType.functionDefinition, mergeRanges(startTag, endTag));
    v.tagOrigin.startTag = startTag;
    v.tagOrigin.endTag = endTag;
    v.attrs = startTag.attrs;
    v.params = params;
    v.name = name;
    return v;
}

const enum ParseOptions {
    none     = 0,
    noTrivia = 0,
    withTrivia        = 0x00000001,
    allowHashWrapped  = 0x00000002,
};

const enum ParseFlags {
    none = 0,
    inHashWrappedExpr = 0x00000001
}

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

/**
 * a string is trivially computable if, possibly stripping outer hash-wrapper and
 * any number of parentheses, we arrive at:
 *      a string with 1 element, which is a TextSpan | Terminal (InterpolatedStringLiteral | NumericLiteral)
 *      an integer numeric literal
 */
 function getTriviallyComputableString(node: Node | undefined | null) : string | null {
    if (!node) return null;

    if (node.type === NodeType.simpleStringLiteral) {
        return node.textSpan.text;
    }
    else if (node.type === NodeType.numericLiteral) {
        return node.literal.token.text;
    }
    else if (node.type === NodeType.hashWrappedExpr || node.type === NodeType.parenthetical) {
        return getTriviallyComputableString(node.expr);
    }

    return null;
}

function getTriviallyComputableBoolean(node: Node | undefined | null) : boolean | null {
    if (!node) return null

    if (node.type === NodeType.simpleStringLiteral) {
        const textVal = node.textSpan.text;
        if (textVal.length <= 3) {
            const lowerCase = textVal.toLowerCase();
            if (lowerCase === "yes") return true;
            else if (lowerCase === "no") return false;

            // there are cases where a negative value is simply unaccepted
            // by the cf engine in certain positions, like <cfargument name="foo" required="-1">
            const maybeNumeric = parseFloat(lowerCase);
            return isNaN(maybeNumeric)
                ? null
                : maybeNumeric !== 0;
        }
    }
    else if (node.type === NodeType.booleanLiteral) {
        return node.literal.token.type === TokenType.KW_TRUE;
    }
    else if (node.type === NodeType.numericLiteral) {
        return parseFloat(node.literal.token.text) !== 0;
    }
    else if (node.type === NodeType.hashWrappedExpr || node.type === NodeType.parenthetical) {
        return getTriviallyComputableBoolean(node.expr);
    }

    return null;
}

/**
 * get the value for some attribute
 * returns:
 *      the attributes value if found,
 *      undefined if the attribute exists but there is no expression associated with the attribute,
 *      null if not found
 */
 function getAttributeValue(attrs: TagAttribute[], name: string) : Node | undefined | null {
    for (const attr of attrs) {
        if (attr.lcName === name) {
            return attr.expr
                ? attr.expr
                : undefined;
        }
    }
    return null;
}

function isFromTag(node: Node) {
    return node.tagOrigin.startTag !== null;
}
isFromTag;

export function Parser(tokenizer_: Tokenizer, mode_: TokenizerMode = TokenizerMode.tag) {
    let tokenizer : Tokenizer = tokenizer_;
    let mode: TokenizerMode = mode_;
    let parseFlags : ParseFlags = ParseFlags.none;
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
    function inHashWrappedExpr() : boolean {
        return !!(parseFlags & ParseFlags.inHashWrappedExpr);
    }


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
                return Terminal(token, parseTrivia());
            }
            else {
                return Terminal(token);
            }
        }
        else {
            return null;
        }
    }

    function parseExpectedTerminal(type: TokenType, parseOptions: ParseOptions, localDiagnosticEmitter?: (() => void) | string) : Terminal {
        const maybeTerminal = parseOptionalTerminal(type, parseOptions);
        if (maybeTerminal) {
            return maybeTerminal;
        }
        else {
            if (localDiagnosticEmitter) {
                if (typeof localDiagnosticEmitter === "string") {
                    parseErrorAtCurrentToken(localDiagnosticEmitter);
                }
                else {
                    localDiagnosticEmitter();
                }
            }
            else if (globalDiagnosticEmitter) {
                globalDiagnosticEmitter();
            }
            else {
                parseErrorAtCurrentToken("Expected a " + TokenTypeUiString[type] + " here");
            }

            const phonyToken : Token = new Token(type, "", SourceRange.Nil());
            return createMissingNode(Terminal(phonyToken));
        }
    }

    function parseTagComment() : CfTag.Comment {
        const commentStart = parseExpectedTerminal(TokenType.CF_TAG_COMMENT_START, ParseOptions.noTrivia);
        const commentBody = parseTagTrivia();
        const commentEnd = parseExpectedTerminal(TokenType.CF_TAG_COMMENT_END, ParseOptions.noTrivia);

        // if no comment end, emit an error

        return CfTag.Comment(commentStart, commentBody, commentEnd);
    }

    function parseScriptSingleLineComment() : Comment {
        throw "nyi";
    }

    function parseScriptMultiLineComment() : Comment {
        throw "nyi";
    }

    function parseTrivia() : Node[] {
        if (tagMode()) {
            return parseTagTrivia();
        }

        const result : Node[] = [];
        while (true) {
            switch (lookahead()) {
                case TokenType.DBL_FORWARD_SLASH:
                    result.push(parseScriptSingleLineComment());
                    continue;
                case TokenType.FORWARD_SLASH_STAR:
                    result.push(parseScriptMultiLineComment());
                    continue;
                case TokenType.WHITESPACE:
                    result.push(TextSpan(next().range, "")); // not really any need to store the whitespace
                    continue;
            }
            break;
        }
        return result;
    }

    function parseTagTrivia() : CfTag[] {
        const result : CfTag[] = [];
        while (true) {
            switch (lookahead()) {
                case TokenType.CF_TAG_COMMENT_START: {
                    result.push(parseTagComment());
                    continue;
                }
                case TokenType.WHITESPACE: {
                    result.push(CfTag.Text(next().range));
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
            return CfTag.ScriptLike(CfTag.Which.start, tagStart, tagName, null, rightAngle, canonicalName, expr);
        }
        else if (canonicalName === "set") {
            const expr = parseAssignmentOrLower();
            const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
            return CfTag.ScriptLike(CfTag.Which.start, tagStart, tagName, null, rightAngle, canonicalName, expr);
        }
        else {
            const tagAttrs = parseTagAttributes();
            const maybeVoidSlash = parseOptionalTerminal(TokenType.FORWARD_SLASH, ParseOptions.withTrivia);
            const rightAngle = parseExpectedTerminal(TokenType.RIGHT_ANGLE, ParseOptions.noTrivia);
            return CfTag.Common(CfTag.Which.start, tagStart, tagName, maybeVoidSlash, rightAngle, canonicalName, tagAttrs);
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
        return CfTag.Common(CfTag.Which.end, tagStart, tagName, null, rightAngle, canonicalName);
    }

    function parseTagAttributes() : TagAttribute[] {
        const result : TagAttribute[] = [];

        while (lookahead() === TokenType.LEXEME) {
            const attrName = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
            if (lookahead() === TokenType.EQUAL) {
                const equal = parseExpectedTerminal(TokenType.EQUAL, ParseOptions.withTrivia);
                let value : Node;
                if (lookahead() === TokenType.LEXEME) {
                    value = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
                }
                else {
                    value = parseExpression();
                }
                result.push(TagAttribute(attrName, tokenizer_.getTokenText(attrName.token).toLowerCase(), equal, value));
            }
            else {
                result.push(TagAttribute(attrName, tokenizer_.getTokenText(attrName.token).toLowerCase()));
            }
        }

        return result;
    }

    function parseTags() : Node[] {
        //
        // tag treeifier
        // after parsing tags, we end up with just a list of tags
        // we need to go through a second time and turn them into a tree
        // we convert text spans into interpolated text spans here if necessary
        // as well as convert all tags into either blocks or statements;
        // a tag with children becomes a block, a single tag (like a <cfset ...> or just a loose <cffile ...> ) becomes a statement
        // this also gives us the opportunity to emit diagnostics for unbalanced start or end tags
        //
        function treeifyTagList(tagList: CfTag[]) : Node[] {
            const tagContext = TagContext();
            const openTagStack : string[] = [];
            let index = 0;

            function openTagStackFindMatchingStartTag(tagCanonicalName: string) : number | null {
                for (let i = openTagStack.length-1; i >= 0; i--) {
                    if (openTagStack[i] === tagCanonicalName) return i;
                }
                return null;
            }

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

            function updateTagContext(tag: CfTag) {
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

            function getReductionScheme(tag: CfTag, instructions: readonly ReductionInstruction[]) : ReductionScheme {
                for (const instr of instructions) {
                    if (instr.onHitWhich === tag.which && instr.onHitName === tag.canonicalName) {
                        return instr.reduction;
                    }
                }
                return ReductionScheme.default;
            }

            function hasNextTag() : boolean {
                return index < tagList.length;
            }
            function nextTag() {
                return tagList[index++];
            }
            function peekTag() {
                return hasNextTag() ? tagList[index] : null;
            }

            function parseOptionalTag(which: CfTag.Which, canonicalName: string) : CfTag | null {
                if (hasNextTag() && tagList[index].which === which && tagList[index].canonicalName === canonicalName) {
                    return nextTag();
                }
                else {
                    return null;
                }
            }

            function parseExpectedTag(which: CfTag.Which, canonicalName: string, diagnosticEmitter?: () => void) : CfTag {
                const tag = parseOptionalTag(which, canonicalName);
                if (tag) {
                    return tag;
                }
                else {
                    if (diagnosticEmitter) {
                        diagnosticEmitter();
                    }
                    else {
                        const msg = `Expected a <${which === CfTag.Which.end ? "/" : ""}cf${canonicalName}> tag here`;
                        let range = hasNextTag() ? peekTag()!.range : tagList[tagList.length-1].range;
                        parseErrorAtRange(range, msg);
                    }

                    // create fake placeholder tag
                    let missingTag : CfTag.Common;
                    if (which === CfTag.Which.start) {
                        missingTag = CfTag.Common(which, NilTerminal, NilTerminal, null, NilTerminal, canonicalName, [])
                    }
                    else {
                        missingTag = CfTag.Common(which, NilTerminal, NilTerminal, null, NilTerminal, canonicalName)
                    }
                    createMissingNode(missingTag);
                    return missingTag;
                }
            }

            function treeifyConditionalTag() {
                const ifTag = parseExpectedTag(CfTag.Which.start, "if");
                openTagStack.push("if");
                const rootConsequent = treeifyTags(reductionInstructions.cfif);
                let root = Conditional(ConditionalSubtype.if, ifTag, rootConsequent);

                let working = root;


                while (true) {
                    const elseIfTag = parseOptionalTag(CfTag.Which.start, "elseif");
                    if (elseIfTag) {
                        const consequent = treeifyTags(reductionInstructions.cfelseif);
                        working.alternative = Conditional(ConditionalSubtype.elseif, elseIfTag, consequent);
                        working = root.alternative!;
                        continue;
                    }
                    const elseTag = parseOptionalTag(CfTag.Which.start, "else");
                    if (elseTag) {
                        const consequent = treeifyTags(reductionInstructions.cfelse);
                        working.alternative = Conditional(ConditionalSubtype.else, elseTag, consequent);
                    }
                    break;
                }

                openTagStack.pop();

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

            function treeifyTagFunction(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) {
                function parseParam(tag: CfTag.Common) : FunctionParameter {
                    const nameAttr = getAttributeValue(tag.attrs, "name");
                    let name = "";
                    if (!nameAttr) {
                        parseErrorAtRange(tag.range, "<cfargument> requires a 'name' attribute");
                    }
                    else {
                        const nameVal = getTriviallyComputableString(nameAttr);
                        if (!nameVal) {
                            //parseErrorAtRange(tag.attrs.name.range, "<cfargument> 'name' attribute must be a non-dynamic string value")
                        }
                        else {
                            name = nameVal;
                        }
                    }

                    const requiredAttr = getAttributeValue(tag.attrs, "required");
                    let isRequired = false;
                    if (requiredAttr) {
                        const boolVal = getTriviallyComputableBoolean(requiredAttr);
                        if (boolVal === null) {
                            //parseErrorAtRange(tag.attrs.required.range, "<cfargument> 'required' attribute must be a non-dynamic boolean value");
                        }
                        isRequired = boolVal ?? false;
                    }

                    return FunctionParameter(tag, name, isRequired);
                }

                let functionName = "";
                const functionNameExpr = getAttributeValue(startTag.attrs, "name");
                if (!functionNameExpr) {
                    parseErrorAtRange(startTag.range, "<cffunction> requires a name attribute")
                }
                else {
                    let functionName = getTriviallyComputableString(functionNameExpr);
                    if (!functionName) {
                        parseErrorAtRange(functionNameExpr.range, "<cffunction> name attribute must be a constant")
                    }
                }

                const params : FunctionParameter[] = [];
                let i = 0;
                for (; i < body.length; i++) {
                    const node = body[i];
                    if (node.type === NodeType.textSpan || node.type === NodeType.comment) {
                        continue;
                    }
                    if (node.type === NodeType.tag && node.canonicalName === "argument") {
                        params.push(parseParam(node as CfTag.Common));
                        continue;
                    }
                    break;
                }

                body = body.splice(i); // drop all the params and whitespace that we consumed as FunctionParameters
                return FunctionDefinition(startTag, params, body, endTag, functionName);
            }

            function treeifyTags(reductionInstructions: readonly ReductionInstruction[]) : Node[] {
                const result : Node[] = [];

                function localStackFindMatchingStartTag(tag: CfTag) : number | null {
                    for (let i = result.length - 1; i >= 0; i--) {
                        if (result[i].type === NodeType.tag) {
                            const stackTag = result[i] as CfTag;
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

                    if (tag.tagType === CfTag.TagType.text) {
                        const text = parseTextInTagContext(tag.range, tagContext);

                        if (Array.isArray(text)) {
                            result.push(...text);
                        }
                        else {
                            result.push(text);
                        }

                        nextTag()
                        continue;
                    }
                    else if (tag.tagType === CfTag.TagType.comment) {
                        result.push(Comment(CommentType.tag, tag.range));
                        nextTag();
                        continue;
                    }
                    else if (tag.which === CfTag.Which.start) {
                        const reductionScheme = getReductionScheme(tag, reductionInstructions);
                        switch (reductionScheme) {
                            case ReductionScheme.return:
                                return result;
                            case ReductionScheme.default:
                                // ok, no-op: the default reduction scheme for a start tag is to do nothing
                                break;
                        }

                        switch (tag.canonicalName) {
                            case "if": {
                                result.push(treeifyConditionalTag());
                                continue;
                            }
                            case "set": {
                                const expr = (<CfTag.ScriptLike>tag).expr;
                                expr.tagOrigin.startTag = tag;
                                result.push(expr);
                                nextTag();
                                continue;
                            }
                            case "function": {
                                openTagStack.push("function");
                                const startTag = parseExpectedTag(CfTag.Which.start, "function");
                                const body = treeifyTags(stopAt(CfTag.Which.end, "function", ReductionScheme.return));
                                const endTag = parseExpectedTag(CfTag.Which.end, "function", () => parseErrorAtRange(startTag.range, "Missing </cffunction> tag"))
                                openTagStack.pop();
                                result.push(treeifyTagFunction(startTag as CfTag.Common, body, endTag as CfTag.Common));
                                continue;
                            }
                            case "script": {
                                // same as handling set, except there is an end tag to consume
                                const stmtList = (<CfTag.Script>tag).stmtList;
                                nextTag();
                                const endTag = parseExpectedTag(CfTag.Which.end, "script");
                                result.push(NamedBlock(tag, stmtList, endTag));
                                continue;
                            }
                            default: {
                                if (requiresEndTag(tag)) {
                                    openTagStack.push(tag.canonicalName);

                                    const startTag = tag;
                                    nextTag();
                                    const blockChildren = treeifyTags(stopAt(CfTag.Which.end, startTag.canonicalName, ReductionScheme.return));

                                    openTagStack.pop();

                                    const endTag = parseExpectedTag(CfTag.Which.end, startTag.canonicalName, () => parseErrorAtRange(startTag.range, "Missing </cf" + startTag.canonicalName + ">"));
                                    updateTagContext(endTag);
                                    result.push(NamedBlock(startTag, blockChildren, endTag));
                                }
                                else {
                                    // a single loose tag
                                    // it will become a statement
                                    // e.g., <cfhttp args /> is essentially a call to a fictitious "cfhttp(args)" function, except it is a statement instead of a value producing expression
                                    // but first we push it into the result as a tag, so that any possible matching end tags can walk up the local results list
                                    // and find it 
                                    result.push(tag);
                                    nextTag();
                                }
                            }
                        }
                    }
                    else if (tag.which === CfTag.Which.end) {
                        const reductionScheme = getReductionScheme(tag, reductionInstructions);
                        if (reductionScheme === ReductionScheme.return) {
                                return result;
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
                            const matchingStartTag = result[matchingStartTagIndex] as CfTag;

                            //
                            // finalize so any remaining loose tag bodies are statement-ized
                            // move (matching_tag + 0) to be the head of a new block
                            // move everything from (matching_tag+1) into the new block as children
                            // use current tag as end of tag block
                            // once everything has been moved into their new homes, drop the original references
                            // from our local results list
                            //
					        
                            const blockChildren = result.slice(matchingStartTagIndex + 1);
                            result.splice(matchingStartTagIndex)
                            result.push(NamedBlock(matchingStartTag, blockChildren, tag));
                        }
                        else {
                            // this tag might be a mismatched tag,
                            // in which case we return the current results, but do not consume the current tag
                            // this will naturally result in an "unmatched tag" error in the caller
                            const matchingOpenTagIndex = openTagStackFindMatchingStartTag(tag.canonicalName);
                            if (matchingOpenTagIndex !== null) {
                                return result;
                            }
                            else {
                                parseErrorAtRange(tag.range, "End tag without a matching start tag (cf" + tag.canonicalName + ")")
                            }
                        }

                        nextTag();
                    }
                }

                return result;
            }

            return treeifyTags(reductionInstructions.default);
        }

        const saveMode = mode;
        mode = TokenizerMode.tag;

        const result : CfTag[] = [];
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
            result.push(CfTag.Text(tagTextRange))
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
                        result.push(tag);
                    }
                    break;
                }
                case TokenType.CF_END_TAG_START: {
                    finishTagTextRange();
                    result.push(parseCfEndTag());
                    break;
                }
                case TokenType.CF_TAG_COMMENT_START: {
                    finishTagTextRange();
                    result.push(parseTagComment());
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
            return TextSpan(range, tokenizer_.getTextSlice(range));
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

    function parseTextWithPossibleInterpolations(quoteDelimiter?: TokenType.QUOTE_SINGLE | TokenType.QUOTE_DOUBLE) : (TextSpan | HashWrappedExpr)[] {
        const result : (TextSpan | HashWrappedExpr)[] = [];
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
            result.push(TextSpan(textSourceRange, tokenizer_.getTextSlice(textSourceRange)));
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
                        result.push(parseHashWrappedExpression());
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

    function isAssignmentTarget(node: Node) : boolean {
        switch (node.type) {
            case NodeType.indexedAccess:
            case NodeType.identifier:
                return true;
            case NodeType.hashWrappedExpr:
                return isAssignmentTarget(node.expr);
            default:
                return false;
        }
    }

    function parseAssignmentOrLower() : Node {
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

        return Assignment(finalModifier, varModifier, root, assignmentChain);
    }

    function parseExpression() : Node {
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
                case TokenType.LIT_IS:              // [[fallthrough]];
                case TokenType.EXCLAMATION_EQUAL:	// [[fallthrough]];
                case TokenType.LIT_IS_NOT:          // [[fallthrough]];
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
                    root = BinaryOperator(root, op, right);
                    continue;
                }
            }
            // if we didn't match any of the above tokens, we're done
            break;
        }

        globalDiagnosticEmitter = saveDiagnosticEmitter;

        return root;
    }

    function parseBooleanExpression(descendIntoOr = true) : Node { // i think this binds the &&'s correctly
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
                    root = BinaryOperator(root, op, expr);
                    continue;
                }
                case TokenType.DBL_AMPERSAND:
                case TokenType.LIT_AND: {
                    const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                    const expr = parseBooleanExpression(/*descendIntoOr*/ false);
                    root = BinaryOperator(root, op, expr);
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
                    root = BinaryOperator(root, op, expr);
                    continue;
                }
            }
            // if we didn't match any of the above operators, we're done
            break;
        }

        return root;
    }

    function parseMultiplication() : Node {
        const stack : Node[] = [];

        // bind "^" (exponentiation) right
        function reduceRight() : void {
            if (stack.length === 1) return;

            // the stack should always have an odd number of elements,
            // (expr (op expr (op expr ...) ...) ...)
            while (stack.length > 1 && (stack[stack.length-2] as Terminal).token.type === TokenType.CARET) {
                const reduced = BinaryOperator(
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
            let result = BinaryOperator(stack[0], stack[1] as Terminal, stack[2]);
            for (let i = 3; i < stack.length; i += 2) {
                result = BinaryOperator(result, stack[i] as Terminal, stack[i+1]);
                i += 2;
            }
            return result;
        }
    }

    function parseParentheticalOrUnaryPrefix() : Node {
        switch (lookahead()) {
            case TokenType.LEFT_PAREN: {
                const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
                const expr = parseExpression();
                const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
                return Parenthetical(leftParen, expr, rightParen);
            }
            case TokenType.DBL_MINUS: // [[fallthrough]];
            case TokenType.DBL_PLUS: {
                const op = parseExpectedTerminal(lookahead(), ParseOptions.withTrivia);
                const expr = parseCallExpressionOrLower();
                return UnaryOperator(op, expr);
            }
            default: return parseCallExpressionOrLower();
        }
    }

    function parseHashWrappedExpression() {
        if (inHashWrappedExpr()) throw "parseHashWrappedExpr cannot be nested";

        parseFlags |= ParseFlags.inHashWrappedExpr;

        const leftHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia);
        const expr = parseExpression();
        const rightHash = parseExpectedTerminal(TokenType.HASH, ParseOptions.withTrivia, "Unterminated hash-wrapped expression");

        parseFlags &= ~ParseFlags.inHashWrappedExpr;

        return HashWrappedExpr(leftHash, expr, rightHash);
    }

    function parseCallExpressionOrLower() : Node {
        switch(lookahead()) {
            case TokenType.NUMBER:
                return parseNumericLiteral();
            case TokenType.QUOTE_DOUBLE: // [[fallthrough]];
            case TokenType.QUOTE_SINGLE:
                return parseStringLiteral();
            case TokenType.KW_TRUE:
            case TokenType.KW_FALSE:
                return BooleanLiteral(parseExpectedTerminal(lookahead(), ParseOptions.withTrivia));
            case TokenType.HASH:
                if (!inHashWrappedExpr()) {
                    return parseHashWrappedExpression();
                }
                // [[fallthrough]];
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
                    root = UnaryOperator(root, unaryOp);
            }
        }

        return root;
    }

    function parseIndexedAccess(root: Node) : Node {
        while (lookahead() != TokenType.EOF) {
            switch (lookahead()) {
                case TokenType.LEFT_BRACKET: {
                    const leftBracket = parseExpectedTerminal(TokenType.LEFT_BRACKET, ParseOptions.withTrivia);
                    const expr = parseExpression();
                    const rightBracket = parseExpectedTerminal(TokenType.RIGHT_BRACKET, ParseOptions.withTrivia);

                    if (root.type !== NodeType.indexedAccess) {
                        root = IndexedAccess(root);
                    }
                    pushAccessElement(root, leftBracket, expr, rightBracket);

                    continue;
                }
                case TokenType.DOT: {
                    const dot = parseExpectedTerminal(TokenType.DOT, ParseOptions.withTrivia);
                    const propertyName = parseExpectedTerminal(TokenType.LEXEME, ParseOptions.withTrivia);
                    if (root.type !== NodeType.indexedAccess) {
                        root = IndexedAccess(root);
                    }
                    pushAccessElement(root, dot, propertyName);
                    continue;
                }
            }
            break;
        }
        return root;
    }

    function parseCallExpression(root: Node) {
        const leftParen = parseExpectedTerminal(TokenType.LEFT_PAREN, ParseOptions.withTrivia);
        const args : CallArgument[] = [];
        while (lookahead() != TokenType.EOF && lookahead() != TokenType.RIGHT_PAREN) {
            const expr = parseExpression();
            const comma = parseOptionalTerminal(TokenType.COMMA, ParseOptions.withTrivia);
            args.push(CallArgument(expr, comma));
        }
        const rightParen = parseExpectedTerminal(TokenType.RIGHT_PAREN, ParseOptions.withTrivia);
        return CallExpression(root, leftParen, args, rightParen);
    }

    function parseIdentifier(parseOptions : ParseOptions) : Node {
        let leftHash : Terminal | null = null;
        if (parseOptions & ParseOptions.allowHashWrapped && !inHashWrappedExpr()) {
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

            return HashWrappedExpr(
                leftHash,
                Identifier(identifier, name),
                rightHash);
        }
        else {
            return Identifier(identifier, name);
        }
    }

    function parseStringLiteral() : Node {
        const quoteType = lookahead();
        if (quoteType !== TokenType.QUOTE_SINGLE && quoteType !== TokenType.QUOTE_DOUBLE) {
            // will a lookahead or speculate ever trigger this ... ?
            throw "AssertionFailure: parseStringLiteral called on input without valid string delimiter";
        }

        const leftQuote = parseExpectedTerminal(quoteType, ParseOptions.noTrivia);
        const stringElements = parseTextWithPossibleInterpolations(quoteType);
        const rightQuote = parseExpectedTerminal(quoteType, ParseOptions.withTrivia);

        if (stringElements.length === 1) {
            const onlyElement = stringElements[0];
            if (onlyElement.type === NodeType.textSpan) {
                return SimpleStringLiteral(quoteType, leftQuote, onlyElement, rightQuote);
            }
        }

        return InterpolatedStringLiteral(quoteType, leftQuote, stringElements, rightQuote);
    }

    function parseNumericLiteral() {
        return NumericLiteral(parseExpectedTerminal(TokenType.NUMBER, ParseOptions.withTrivia));
    }

    function getDiagnostics() : readonly Diagnostic[] {
        return diagnostics;
    }

    return {
        parseTags,
        getDiagnostics
    }
}
