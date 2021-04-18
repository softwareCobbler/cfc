import { TokenType, Token, NilToken } from "./tokenizer";
import { SourceRange } from "./scanner";

export const enum NodeFlags {
    none    = 0,
    error   = 0x00000001,
    missing = 0x00000002
}

export const enum NodeType {
    terminal, textSpan, comment, hashWrappedExpr, parenthetical, tagAttribute,
    tag, callExpression, callArgument, assignment, unaryOperator, binaryOperator,
    conditional, statement, namedBlock, simpleStringLiteral, interpolatedStringLiteral, numericLiteral, booleanLiteral,
    identifier, indexedAccess,
    functionDefinition, arrowFunctionDefinition, functionParameter,
    dottedPath
}

export type Node =
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
    | Block
    | SimpleStringLiteral
    | InterpolatedStringLiteral
    | NumericLiteral
    | BooleanLiteral
    | Identifier
    | IndexedAccess
    | FunctionParameter
    | FunctionDefinition
    | ArrowFunctionDefinition
    | DottedPath<any>

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

export const NilTerminal : Readonly<Terminal> = Terminal(NilToken);

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

export function Statement(node: Node | null, semicolon: Terminal | null) : Statement {
    const v = NodeBase<Statement>(NodeType.statement, mergeRanges(node, semicolon));
    v.stmt = node;
    v.semicolon = semicolon;
    return v;
}

export namespace FromTag {
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
} // namespace FromTag

export interface Block extends NodeBase {
    type: NodeType.namedBlock;
    name: Terminal | null; // cf blocks may be named, e.g, `savecontent variable='foo' { ... }` the name is 'savecontent'
    attrs: TagAttribute[]; // cf blocks may have attributes just like their tag counterparts, e.g, `variable='foo'` in the above
    leftBrace: Terminal | null;
    stmtList: Node[];
    rightBrace: Terminal | null;
}

export function Block(leftBrace: Terminal, stmtList: Node[], rightBrace: Terminal) : Block {
    const v = NodeBase<Block>(NodeType.namedBlock, mergeRanges(leftBrace, rightBrace));
    v.name = null;
    v.attrs = [];
    v.leftBrace = leftBrace;
    v.stmtList = stmtList;
    v.rightBrace = rightBrace;
    return v;
}

export namespace FromTag {
export function Block(startTag: CfTag, endTag: CfTag) : Block;
export function Block(startTag: CfTag, stmtList: Node[], endTag: CfTag) : Block;
export function Block(startTag: CfTag, endTagOrStmtList: CfTag | Node[], endTag?: CfTag) {
    if (endTag) {
        const v = NodeBase<Block>(NodeType.namedBlock, mergeRanges(startTag, endTag));
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
        const v = NodeBase<Block>(NodeType.namedBlock, mergeRanges(startTag, endTagOrStmtList));
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
} // namespace FromTag

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
    lexeme: Terminal;
    canonicalName: string;
}

export function Identifier(identifier: Terminal, name: string) {
    const v = NodeBase<Identifier>(NodeType.identifier, identifier.range);
    v.lexeme = identifier;
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
    requiredTerminal: Terminal | null;
    javaLikeTypename: DottedPath<Terminal> | null;
    identifier: Identifier | null;
    equals: Terminal | null;
    defaultValue: Node | null;
    comma: Terminal | null;
    canonicalName: string | null;
    required: boolean;
}

export function FunctionParameter(
    requiredTerminal : Terminal | null,
    javaLikeTypename: DottedPath<Terminal> | null,
    identifier: Identifier,
    equals: Terminal | null,
    defaultValue: Node | null,
    comma: Terminal | null) : FunctionParameter {
    const v = NodeBase<FunctionParameter>(NodeType.functionParameter, mergeRanges(requiredTerminal, javaLikeTypename, identifier, defaultValue, comma));
    v.requiredTerminal = requiredTerminal;
    v.javaLikeTypename = javaLikeTypename;
    v.identifier = identifier;
    v.equals = equals;
    v.defaultValue = defaultValue;
    v.comma = comma;
    v.canonicalName = identifier.canonicalName;
    v.required = !!requiredTerminal;
    return v;
}

export namespace FromTag {
export function FunctionParameter(tag: CfTag.Common, canonicalName: string | null, required: boolean, defaultValue: Node | null) : FunctionParameter {
    const v = NodeBase<FunctionParameter>(NodeType.functionParameter, tag.range);
    v.tagOrigin.startTag = tag;
    v.requiredTerminal = null;
    v.javaLikeTypename = null;
    v.identifier = null;
    v.equals = null;
    v.defaultValue = defaultValue;
    v.comma = null;
    v.canonicalName = canonicalName;
    v.required = required;
    return v;
}
} // namespace FromTag

export interface FunctionDefinition extends NodeBase {
    type: NodeType.functionDefinition;
    // return type
    // access modifier ? 
    attrs: TagAttribute[];
    // left paren
    params: FunctionParameter[];
    // right paren
    name: string;
    // body block
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

export interface ArrowFunctionDefinition extends NodeBase {
    type: NodeType.arrowFunctionDefinition;
    parens: {left: Terminal, right: Terminal} | null,
    params: FunctionParameter[];
    fatArrow: Terminal,
    body: Statement;
}

export function ArrowFunctionDefinition(leftParen: Terminal | null, params: FunctionParameter[], rightParen: Terminal | null, fatArrow: Terminal, body: Statement) : ArrowFunctionDefinition {
    const v = NodeBase<ArrowFunctionDefinition>(NodeType.arrowFunctionDefinition);
    v.range = mergeRanges(leftParen, body);
    v.parens = (leftParen && rightParen) ? {left: leftParen, right: rightParen} : null,
    v.params = params;
    v.fatArrow = fatArrow;
    v.body = body;
    return v;
};

export interface DottedPath<T extends NodeBase> extends NodeBase {
    type: NodeType.dottedPath;
    leadingName: T;
    rest: {dot: Terminal, name: T}[]
}
export function DottedPath<T extends NodeBase>(leadingName: T) : DottedPath<T> {
    const v = NodeBase<DottedPath<T>>(NodeType.dottedPath, leadingName.range);
    v.rest = [];
    return v;
}