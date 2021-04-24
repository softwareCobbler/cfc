import { SourceRange, TokenType, Token, NilToken, TokenTypeUiString } from "./scanner";

let debug = false;

export function setDebug(isDebug: boolean) {
    debug = isDebug;
}

export const enum NodeFlags {
    none    = 0,
    error   = 0x00000001,
    missing = 0x00000002
}

export const enum NodeType {
    terminal, textSpan, comment, hashWrappedExpr, parenthetical, tagAttribute,
    tag, callExpression, callArgument, unaryOperator, binaryOperator,
    conditional, variableDeclaration, statement, namedBlock, simpleStringLiteral, interpolatedStringLiteral, numericLiteral, booleanLiteral,
    identifier, indexedAccess,
    functionDefinition, arrowFunctionDefinition, functionParameter,
    dottedPath, switch, switchCase, do, while, ternary, for, structLiteral, arrayLiteral,
    structLiteralInitializerMember, arrayLiteralInitializerMember, returnStatement, try, catch, finally,
    breakStatement, continueStatement
}

const NodeTypeUiString : Record<NodeType, string> = {
    [NodeType.terminal]: "terminal",
    [NodeType.textSpan]: "textSpan",
    [NodeType.comment]: "comment",
    [NodeType.hashWrappedExpr]: "hashWrappedExpr",
    [NodeType.parenthetical]: "parenthetical",
    [NodeType.tagAttribute]: "tagAttribute",
    [NodeType.tag]: "tag",
    [NodeType.callExpression]: "callExpression",
    [NodeType.callArgument]: "callArgument",
    [NodeType.unaryOperator]: "unaryOperator",
    [NodeType.binaryOperator]: "binaryOperator",
    [NodeType.conditional]: "conditional",
    [NodeType.variableDeclaration]: "declaration",
    [NodeType.statement]: "statement",
    [NodeType.namedBlock]: "namedBlock",
    [NodeType.simpleStringLiteral]: "simpleStringLiteral",
    [NodeType.interpolatedStringLiteral]: "interpolatedStringLiteral",
    [NodeType.numericLiteral]: "numericLiteral",
    [NodeType.booleanLiteral]: "booleanLiteral",
    [NodeType.identifier]: "identifier",
    [NodeType.indexedAccess]: "indexedAccess",
    [NodeType.functionDefinition]: "functionDefinition",
    [NodeType.arrowFunctionDefinition]: "arrowFunctionDefinition",
    [NodeType.functionParameter]: "functionParameter",
    [NodeType.dottedPath]: "dottedPath",
    [NodeType.switch]: "switch",
    [NodeType.switchCase]: "switchCase",
    [NodeType.do]: "do",
    [NodeType.while]: "while",
    [NodeType.ternary]: "ternary",
    [NodeType.for]: "for",
    [NodeType.structLiteral]: "structLiteral",
    [NodeType.arrayLiteral]: "arrayLiteral",
    [NodeType.structLiteralInitializerMember]: "structLiteralInitializerMember",
    [NodeType.arrayLiteralInitializerMember]: "arrayLiteralInitializerMember",
    [NodeType.returnStatement]: "returnStatement",
    [NodeType.try]: "try",
    [NodeType.catch]: "catch",
    [NodeType.finally]: "finally",
    [NodeType.breakStatement]: "break",
    [NodeType.continueStatement]: "continue"
};

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
    | UnaryOperator
    | BinaryOperator
    | Conditional
    | VariableDeclaration
    | Statement
    | ReturnStatement
    | BreakStatement
    | ContinueStatement
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
    | DottedPath<any> // `"x"."x"` in a struct literal, `x.x` in a function parameter declaration, maybe others 
    | Switch
    | SwitchCase
    | Do
    | While
    | Ternary
    | For
    | StructLiteral
    | StructLiteralInitializerMember
    | ArrayLiteral
    | ArrayLiteralInitializerMember
    | Try
    | Catch
    | Finally

interface NodeBase {
    type: NodeType;
    parent: Node | null,
    range: SourceRange,
    tagOrigin: {
        startTag: CfTag | null,
        endTag: CfTag | null,
    }
    flags: NodeFlags,
    __debug_type?: string;
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

    if (debug) {
        result.__debug_type = NodeTypeUiString[type];
    }

    return result as T;
}

export function mergeRanges(...nodes : (SourceRange | Node | Node[] | undefined | null )[]) : SourceRange {
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
    rangeWithTrivia: SourceRange;
    token: Token;
    trivia: Node[];
    __debug_tokenType?: string;
}

export function Terminal(token: Token, trivia: Node[] = []) : Terminal {
    const v = NodeBase<Terminal>(NodeType.terminal, token.range);
    v.rangeWithTrivia = mergeRanges(token.range, trivia);
    v.token = token;
    v.trivia = trivia;

    if (debug) {
        v.__debug_tokenType = TokenTypeUiString[token.type];
    }

    return v;
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
        expr: Node | null; // a return can have no expression
    }
    export function ScriptLike(
        which: Which.start,
        tagStart: Terminal,
        tagName: Terminal,
        voidSlash: Terminal | null,
        tagEnd: Terminal,
        canonicalName: string,
        expr: Node | null) : ScriptLike {
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
    left: Node;
    leftParen: Terminal;
    args: CallArgument[];
    rightParen: Terminal;

}
export function CallExpression(left: Node, leftParen: Terminal, args: CallArgument[], rightParen: Terminal) {
    const v = NodeBase<CallExpression>(NodeType.callExpression, mergeRanges(left, rightParen));
    v.left = left;
    v.leftParen = leftParen;
    v.args = args;
    v.rightParen = rightParen;
    return v;
}

export interface CallArgument extends NodeBase {
    type: NodeType.callArgument;
    name: Identifier | null;
    equals: Terminal | null;
    expr: Node;
    comma: Terminal | null;
}

export function CallArgument(name: Identifier | null, equals: Terminal | null, expr: Node, comma: Terminal | null) : CallArgument {
    const v = NodeBase<CallArgument>(NodeType.callArgument, mergeRanges(name, expr, comma));
    v.name = name;
    v.equals = equals;
    v.expr = expr;
    v.comma = comma;
    return v;
}

export const enum UnaryOperatorPos { pre, post };
export const enum UnaryOpType { inc, dec, pos, neg, not };
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
        case TokenType.EXCLAMATION: return UnaryOpType.not;
        case TokenType.LIT_NOT:     return UnaryOpType.not;
        case TokenType.DBL_MINUS:   return UnaryOpType.dec;
        case TokenType.DBL_PLUS:    return UnaryOpType.inc;
        case TokenType.MINUS:       return UnaryOpType.neg;
        case TokenType.PLUS:        return UnaryOpType.pos;
        default: break;
    }
    throw "bad unary op type transform";
}

export const enum BinaryOpType {
    add, sub, mul, div, mod, exp, cat, eq, neq, lt, lte, gt, gte, nullCoalesce,
    and, or, xor, assign, assign_add, assign_sub, assign_mul, assign_div, assign_cat
}
const BinaryOpTypeUiString : Record<BinaryOpType, string> = {
    [BinaryOpType.add]:          "+",
    [BinaryOpType.sub]:          "-",
    [BinaryOpType.mul]:          "*",
    [BinaryOpType.div]:          "/",
    [BinaryOpType.mod]:          "%",
    [BinaryOpType.exp]:          "exp", // in cf, "^", but that is easily confusable with the xor symbol from other langs so we spell it out
    [BinaryOpType.cat]:          "&",
    [BinaryOpType.eq]:           "==",
    [BinaryOpType.neq]:          "!=",
    [BinaryOpType.lt]:           "<",
    [BinaryOpType.lte]:          "<=",
    [BinaryOpType.gt]:           ">",
    [BinaryOpType.gte]:          ">=",
    [BinaryOpType.nullCoalesce]: "?:",
    [BinaryOpType.and]:          "&&",
    [BinaryOpType.or]:           "||",
    [BinaryOpType.xor]:          "xor", // no cf operator symbol exists for this
    [BinaryOpType.assign]:       "=",
    [BinaryOpType.assign_add]:   "+=",
    [BinaryOpType.assign_cat]:   "&=",
    [BinaryOpType.assign_div]:   "/=",
    [BinaryOpType.assign_sub]:   "-=",
    [BinaryOpType.assign_mul]:   "*=",
};

export interface BinaryOperator extends NodeBase {
    type: NodeType.binaryOperator;
    optype: BinaryOpType;
    left: Node;
    operator: Terminal;
    right: Node;
    __debug_opType?: string;
}

export function BinaryOperator(left: Node, operator: Terminal, right: Node) : BinaryOperator {
    const v = NodeBase<BinaryOperator>(NodeType.binaryOperator, mergeRanges(left, right));
    v.left = left;
    v.operator = operator;
    v.right = right;
    v.optype = tokenTypeToBinaryOpType(operator.token.type);

    if (debug) {
        v.__debug_opType = BinaryOpTypeUiString[v.optype];
    }
    
    return v;
}

export function tokenTypeToBinaryOpType(tokenType: TokenType) {
    switch (tokenType) {
        case TokenType.PLUS:          		return BinaryOpType.add;
        case TokenType.MINUS:         		return BinaryOpType.sub;
        case TokenType.STAR:          		return BinaryOpType.mul;
        case TokenType.FORWARD_SLASH: 		return BinaryOpType.div;
        case TokenType.PERCENT:       		return BinaryOpType.mod;
        case TokenType.LIT_MOD:       		return BinaryOpType.mod;
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

        case TokenType.DBL_PIPE:            return BinaryOpType.or;
        case TokenType.LIT_OR:              return BinaryOpType.or;
        case TokenType.DBL_AMPERSAND:       return BinaryOpType.and;
        case TokenType.LIT_AND:             return BinaryOpType.and;
        case TokenType.LIT_XOR:             return BinaryOpType.xor;

        case TokenType.EQUAL:               return BinaryOpType.assign;
        case TokenType.AMPERSAND_EQUAL:     return BinaryOpType.assign_cat;
        case TokenType.PLUS_EQUAL:          return BinaryOpType.assign_add;
        case TokenType.MINUS_EQUAL:         return BinaryOpType.assign_sub;
        case TokenType.STAR_EQUAL:          return BinaryOpType.assign_mul;
        case TokenType.FORWARD_SLASH_EQUAL: return BinaryOpType.assign_div;

        case TokenType.QUESTION_MARK_COLON: return BinaryOpType.nullCoalesce;
        default: break;
    }
    throw "bad binary op type transform";
}

export const enum ConditionalSubtype { if, elseif, else };
export interface Conditional extends NodeBase {
    type: NodeType.conditional;
    elseToken   : Terminal | null;
    ifToken     : Terminal | null;
    leftParen   : Terminal | null;
    expr        : Node | null;
    rightParen  : Terminal | null;
    consequent  : Node;
    alternative : Conditional | null;
}

export namespace Conditional {
    export function If(ifToken: Terminal, leftParen: Terminal, expr: Node, rightParen: Terminal, consequent: Node) : Conditional {
        const v       = NodeBase<Conditional>(NodeType.conditional, mergeRanges(ifToken, consequent));
        v.elseToken   = null;
        v.ifToken     = ifToken;
        v.leftParen   = leftParen;
        v.expr        = expr;
        v.rightParen  = rightParen;
        v.consequent  = consequent;
        v.alternative = null;
        return v;
    }
    export function ElseIf(elseToken: Terminal, ifToken: Terminal, leftParen: Terminal, expr: Node, rightParen: Terminal, consequent: Node) : Conditional {
        const v       = NodeBase<Conditional>(NodeType.conditional, mergeRanges(ifToken, consequent));
        v.elseToken   = elseToken;
        v.ifToken     = ifToken;
        v.leftParen   = leftParen;
        v.expr        = expr;
        v.rightParen  = rightParen;
        v.consequent  = consequent;
        v.alternative = null;
        return v;
    }
    export function Else(elseToken: Terminal, consequent: Node) : Conditional {
        const v       = NodeBase<Conditional>(NodeType.conditional, mergeRanges(elseToken, consequent));
        v.elseToken   = elseToken;
        v.ifToken     = null;
        v.leftParen   = null;
        v.expr        = null;
        v.rightParen  = null;
        v.consequent  = consequent;
        v.alternative = null;
        return v;
    }
}

export namespace FromTag {
    export function Conditional(subtype: ConditionalSubtype, fromTag: CfTag, consequent: Node) : Conditional {
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
}

export interface VariableDeclaration extends NodeBase {
    type: NodeType.variableDeclaration,
    finalModifier: Terminal | null,
    varModifier: Terminal | null,
    identifier: Node, // can be Identifier | HashWrappedExpr | SimpleStringLiteral | InterpolatedStringLiteral | IndexedAccess, but that's tough to constrain ergonomically
    expr: Node
}

export function VariableDeclaration(
    finalModifier: Terminal | null,
    varModifier: Terminal | null,
    identifier: Node,
    expr: Node
) : VariableDeclaration {
    const v = NodeBase<VariableDeclaration>(NodeType.variableDeclaration, mergeRanges(finalModifier, varModifier, expr));
    v.finalModifier = finalModifier;
    v.varModifier = varModifier;
    v.identifier = identifier;
    v.expr = expr;
    return v;
}

export interface Statement extends NodeBase {
    type: NodeType.statement;
    expr: Node | null;              // null if from tag (originating node will be in tagOrigin.startTag)
    semicolon : Terminal | null;    // null if from tag
}

export function Statement(node: Node | null, semicolon: Terminal | null) : Statement {
    const v = NodeBase<Statement>(NodeType.statement, mergeRanges(node, semicolon));
    v.expr = node;
    v.semicolon = semicolon;
    return v;
}

export namespace FromTag {
    export function Statement(tag: CfTag.ScriptLike) : Statement {
        const stmt = NodeBase<Statement>(NodeType.statement, tag.range);
        stmt.expr = tag.expr;
        stmt.tagOrigin.startTag = tag;
        stmt.semicolon = null;
        return stmt;
    }
}

export interface ReturnStatement extends Omit<Statement, "type"> {
    type: NodeType.returnStatement;
    returnToken: Terminal | null; // null if from tag
}
export function ReturnStatement(returnToken: Terminal, expr: Node | null, semicolon: Terminal | null) : ReturnStatement {
    const v = NodeBase<ReturnStatement>(NodeType.returnStatement, mergeRanges(returnToken, expr, semicolon))
    v.returnToken = returnToken;
    v.expr = expr;
    v.semicolon;
    return v;
}
export namespace FromTag {
    export function ReturnStatement(tag: CfTag.ScriptLike) : ReturnStatement {
        const v = NodeBase<ReturnStatement>(NodeType.returnStatement, tag.range);
        v.tagOrigin.startTag = tag;
        v.returnToken = null;
        v.expr = tag.expr;
        v.semicolon = null;
        return v;
    }
}

export interface BreakStatement extends NodeBase {
    type: NodeType.breakStatement,
    breakToken: Terminal | null,
    semicolon: Terminal | null
}
export function BreakStatement(
    breakToken: Terminal,
    semicolon: Terminal | null
) : BreakStatement {
    const v = NodeBase<BreakStatement>(NodeType.breakStatement, mergeRanges(breakToken, semicolon));
    v.breakToken = breakToken;
    v.semicolon = semicolon;
    return v;
}
export namespace FromTag {
    export function BreakStatement(tag: CfTag.Common) : BreakStatement {
        const v = NodeBase<BreakStatement>(NodeType.breakStatement, tag.range);
        v.tagOrigin.startTag = tag;
        v.breakToken = null;
        v.semicolon = null;
        return v;
    }
}

export interface ContinueStatement extends NodeBase {
    type: NodeType.continueStatement,
    continueToken: Terminal | null,
    semicolon: Terminal | null
}
export function ContinueStatement(
    continueToken: Terminal,
    semicolon: Terminal | null
) : ContinueStatement {
    const v = NodeBase<ContinueStatement>(NodeType.continueStatement, mergeRanges(continueToken, semicolon));
    v.continueToken = continueToken;
    v.semicolon = semicolon;
    return v;
}
export namespace FromTag {
    export function ContinueStatement(tag: CfTag.Common) : ContinueStatement {
        const v = NodeBase<ContinueStatement>(NodeType.continueStatement, tag.range);
        v.tagOrigin.startTag = tag;
        v.continueToken = null;
        v.semicolon = null;
        return v;
    }
}

// we're going to abuse Block for at least <cftransaction>, maybe others:
// <cftransaction>...</cftransaction> is a named block, with the name "transaction"
// <cftransaction action="rollback"> is effectively a statement, but we will store it as a named block,
//     with no body, and no end tag; so the moral script equivalent would be like `transaction action="rollback";`, no braced-body
//     when we parse named blocks in script statement mode we have to account for some named blocks being allowed to have no body
export interface Block extends NodeBase {
    type: NodeType.namedBlock;
    name: Terminal | null; // null if from tag (it's in tagOrigin.startTag), a terminal if from script
    attrs: TagAttribute[]; // cf blocks may have attributes just like their tag counterparts, e.g, `variable='foo'` in the above
    leftBrace: Terminal | null; // tag-origin blocks will have no braces
    stmtList: Node[];
    rightBrace: Terminal | null;
}

export function NamedBlockFromBlock(name: Terminal, attrs: TagAttribute[], block: Block) : Block {
    block.name = name;
    block.attrs = attrs;
    return block;
}

export function Block(leftBrace: Terminal | null, stmtList: Node[], rightBrace: Terminal | null) : Block {
    const v      = NodeBase<Block>(NodeType.namedBlock, mergeRanges(leftBrace, rightBrace));
    v.name       = null;
    v.attrs      = [];
    v.leftBrace  = leftBrace;
    v.stmtList   = stmtList;
    v.rightBrace = rightBrace;
    return v;
}

export namespace FromTag {
export function Block(startTag: CfTag, endTag: CfTag) : Block;                                      // overload 1
export function Block(startTag: CfTag, stmtList: Node[], endTag: CfTag | null) : Block;             // overload 2
export function Block(startTag: CfTag, endTagOrStmtList: CfTag | Node[], endTag?: CfTag | null) {
    if (endTag === undefined) { // overload 1
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
    else { // overload 2
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
}
} // namespace FromTag

export interface SimpleStringLiteral extends NodeBase {
    type: NodeType.simpleStringLiteral;
    leftQuote : Terminal;
    textSpan : TextSpan;
    rightQuote: Terminal;
}

export function SimpleStringLiteral(
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

export const enum IndexedAccessType { dot, bracket };
export interface DotAccess {
    accessType: IndexedAccessType.dot;
    dot: Terminal;
    propertyName: Terminal;
}

export interface BracketAccess {
    accessType: IndexedAccessType.bracket;
    leftBracket: Terminal;
    expr: Node;
    rightBracket: Terminal;
}

export interface IndexedAccess extends NodeBase {
    type: NodeType.indexedAccess;
    root: Node;
    accessElements: AccessElement[];
    last: () => AccessElement | undefined;
}

export function IndexedAccess(root: Node) : IndexedAccess {
    const v = NodeBase<IndexedAccess>(NodeType.indexedAccess, root.range);
    v.root = root;
    v.accessElements = [];
    v.last = function() { return this.accessElements[this.accessElements.length-1]; }
    return v;
}

export function pushAccessElement(base: IndexedAccess, dot: Terminal, propertyName: Terminal) : void;
export function pushAccessElement(base: IndexedAccess, leftBracket: Terminal, expr: Node, rightBracket: Terminal) : void;
export function pushAccessElement(base: IndexedAccess, dotOrBracket: Terminal, expr: Node | Terminal, rightBracket?: Terminal) : void {
    if (rightBracket) { // bracket access
        (<BracketAccess[]>base.accessElements).push({
            accessType: IndexedAccessType.bracket,
            leftBracket: dotOrBracket,
            expr: expr,
            rightBracket: rightBracket
        });
        base.range.toExclusive = rightBracket.range.toExclusive;
    }
    else { // dot access
        (<DotAccess[]>base.accessElements).push({
            accessType: IndexedAccessType.dot,
            dot: dotOrBracket,
            propertyName: (expr as Terminal)
        });
        base.range.toExclusive = expr.range.toExclusive;
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
}

export interface FunctionDefinition extends NodeBase {
    type: NodeType.functionDefinition;
    accessModifier: Terminal | null;
    returnType    : DottedPath<Terminal> | null;
    functionToken : Terminal | null;
    nameToken     : Terminal | null;
    leftParen     : Terminal | null;
    params        : FunctionParameter[];
    rightParen    : Terminal | null;
    attrs         : TagAttribute[];
    body          : Block;
    canonicalName : string | null;
}

export function FunctionDefinition(
    accessModifier: Terminal | null,
    returnType    : DottedPath<Terminal> | null,
    functionToken : Terminal,
    nameToken     : Terminal | null,
    leftParen     : Terminal,
    params        : FunctionParameter[],
    rightParen    : Terminal,
    attrs         : TagAttribute[],
    body          : Block
) : FunctionDefinition {
    const v = NodeBase<FunctionDefinition>(NodeType.functionDefinition, mergeRanges(accessModifier, returnType, functionToken, body));
    v.accessModifier = accessModifier;
    v.returnType     = returnType;
    v.functionToken  = functionToken;
    v.nameToken      = nameToken;
    v.leftParen      = leftParen;
    v.params         = params;
    v.rightParen     = rightParen;
    v.attrs          = attrs;
    v.body           = body;
    v.canonicalName  = nameToken?.token.text.toLowerCase() ?? null;
    return v;
}

export namespace FromTag {
    export function FunctionDefinition(startTag: CfTag.Common, params: FunctionParameter[], body: Block, endTag: CfTag.Common, name: string) : FunctionDefinition {
        const v = NodeBase<FunctionDefinition>(NodeType.functionDefinition, mergeRanges(startTag, endTag));
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag   = endTag;
        v.accessModifier     = null;
        v.returnType         = null;
        v.functionToken      = null;
        v.nameToken          = null;
        v.leftParen          = null;
        v.params             = params;
        v.rightParen         = null;
        v.attrs              = startTag.attrs;
        v.body               = body;
        v.canonicalName      = name;
        return v;
    }
}

export interface ArrowFunctionDefinition extends NodeBase {
    type: NodeType.arrowFunctionDefinition;
    parens: {left: Terminal, right: Terminal} | null,
    params: FunctionParameter[];
    fatArrow: Terminal,
    body: Node;
}

export function ArrowFunctionDefinition(leftParen: Terminal | null, params: FunctionParameter[], rightParen: Terminal | null, fatArrow: Terminal, body: Node) : ArrowFunctionDefinition {
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
    headKey: T;
    rest: {dot: Terminal, key: T}[]
}
export function DottedPath<T extends NodeBase>(headKey: T) : DottedPath<T> {
    const v = NodeBase<DottedPath<T>>(NodeType.dottedPath, headKey.range);
    v.headKey = headKey;
    v.rest = [];
    return v;
}

export interface Switch extends NodeBase {
    type: NodeType.switch;
    switchToken: Terminal;
    leftParen: Terminal;
    expr: Node;
    rightParen: Terminal;
    leftBrace: Terminal;
    cases: SwitchCase[];
    rightBrace: Terminal;
}

export function Switch(
    switchToken: Terminal,
    leftParen: Terminal,
    expr: Node,
    rightParen: Terminal,
    leftBrace: Terminal,
    cases: SwitchCase[],
    rightBrace: Terminal) : Switch {
    const v = NodeBase<Switch>(NodeType.switch, mergeRanges(switchToken, rightBrace));
    v.switchToken = switchToken;
    v.leftParen = leftParen;
    v.expr = expr;
    v.rightParen = rightParen;
    v.leftBrace = leftBrace;
    v.cases = cases;
    v.rightBrace = rightBrace;
    return v;
}

export const enum SwitchCaseType { case, default };
export interface SwitchCase extends NodeBase {
    type: NodeType.switchCase;
    caseType: SwitchCaseType;
    caseOrDefaultToken: Terminal;
    expr: Node | null;
    colon: Terminal;
    statements: Node[];
}

export namespace SwitchCase {
    export function Case(caseToken: Terminal, expr: Node, colon: Terminal, statements: Node[]) : SwitchCase {
        const v = NodeBase<SwitchCase>(NodeType.switchCase, mergeRanges(caseToken, statements));
        v.caseType = SwitchCaseType.case
        v.caseOrDefaultToken = caseToken;
        v.expr = expr;
        v.colon = colon;
        v.statements = statements;
        return v;
    }
    export function Default(defaultToken: Terminal, colon: Terminal, statements: Node[]) : SwitchCase {
        const v = NodeBase<SwitchCase>(NodeType.switchCase, mergeRanges(defaultToken, statements));
        v.caseType = SwitchCaseType.default;
        v.caseOrDefaultToken = defaultToken;
        v.expr = null;
        v.colon = colon;
        v.statements = statements;
        return v;
    }
}

export interface Do extends NodeBase {
    type: NodeType.do;
    doToken: Terminal;
    body: Node;
    whileToken: Terminal;
    leftParen: Terminal;
    expr: Node;
    rightParen: Terminal;
    // a trailing semicolon is optional, and should be consumed as part of a null-statement
}

export function Do(
    doToken: Terminal,
    body: Node,
    whileToken: Terminal,
    leftParen: Terminal,
    expr: Node,
    rightParen: Terminal) : Do
{
    const v = NodeBase<Do>(NodeType.do, mergeRanges(doToken, rightParen));
    v.doToken    = doToken;
    v.body       = body;
    v.whileToken = whileToken;
    v.leftParen  = leftParen;
    v.expr       = expr;
    v.rightParen = rightParen;
    return v;
}

export interface While extends NodeBase {
    type: NodeType.while;
    whileToken: Terminal;
    leftParen: Terminal;
    expr: Node;
    rightParen: Terminal;
    body: Node;
}

export function While(
    whileToken: Terminal,
    leftParen: Terminal,
    expr: Node,
    rightParen: Terminal,
    body: Node) : While
{
    const v = NodeBase<While>(NodeType.while, mergeRanges(whileToken, body));
    v.whileToken = whileToken;
    v.leftParen  = leftParen;
    v.expr       = expr;
    v.rightParen = rightParen;
    v.body       = body;
    return v;
}

export interface Ternary extends NodeBase {
    type: NodeType.ternary;
    expr: Node;
    questionMark: Terminal;
    ifTrue: Node;
    colon: Terminal;
    ifFalse: Node;
}

export function Ternary(
    expr: Node,
    questionMark: Terminal,
    ifTrue: Node,
    colon: Terminal,
    ifFalse: Node,
) {
    const v = NodeBase<Ternary>(NodeType.ternary, mergeRanges(expr, ifFalse));
    v.questionMark = questionMark;
    v.ifTrue = ifTrue;
    v.colon = colon;
    v.ifFalse = ifFalse;
    return v;
}

export const enum ForSubType { for, forIn}
export interface For extends NodeBase {
    type: NodeType.for;
    subType: ForSubType;
    forToken: Terminal;
    leftParen: Terminal;
    for?: {
        initExpr: Node | null;
        semi1: Terminal;
        conditionExpr: Node | null;
        semi2: Terminal;
        incrementExpr: Node | null;
    }
    forIn?: {
        init: Node;
        inToken: Terminal;
        expr: Node;
    }
    rightParen: Terminal;
    body: Node;
}

export namespace For {
    export function For(
        forToken: Terminal,
        leftParen: Terminal,
        initExpr: Node | null,
        semi1: Terminal,
        conditionExpr: Node | null,
        semi2: Terminal,
        incrementExpr: Node | null,
        rightParen: Terminal,
        body: Node) : For {
        const v = NodeBase<For>(NodeType.for, mergeRanges(forToken, body));
        v.forToken = forToken;
        v.leftParen = leftParen;
        v.for = {
            initExpr,
            semi1,
            conditionExpr,
            semi2,
            incrementExpr,
        }
        v.rightParen = rightParen;
        v.body = body;
        return v;
    }
    export function ForIn(
        forToken: Terminal,
        leftParen: Terminal,
        init: Node,
        inToken: Terminal,
        expr: Node,
        rightParen: Terminal,
        body: Node) : For {
        const v = NodeBase<For>(NodeType.for, mergeRanges(forToken, body));
        v.forToken = forToken;
        v.leftParen = leftParen;
        v.forIn = {
            init,
            inToken,
            expr,
        }
        v.rightParen = rightParen;
        v.body = body;
        return v;
    }
}

export interface StructLiteral extends NodeBase {
    type: NodeType.structLiteral,
    ordered: boolean,
    leftDelimiter: Terminal,
    members: StructLiteralInitializerMember[],
    emptyOrderedStructColon?: Terminal,
    rightDelimiter: Terminal,
}

export function StructLiteral(
    leftBrace: Terminal,
    
    members: StructLiteralInitializerMember[],
    rightBrace: Terminal,
) : StructLiteral {
    const v = NodeBase<StructLiteral>(NodeType.structLiteral, mergeRanges(leftBrace, rightBrace));
    v.ordered = false;
    v.leftDelimiter = leftBrace;
    v.members = members;
    v.rightDelimiter = rightBrace;
    return v;
}

export function OrderedStructLiteral(
    leftBracket: Terminal,
    members: StructLiteralInitializerMember[],
    rightBracket: Terminal,
) : StructLiteral {
    const v = NodeBase<StructLiteral>(NodeType.structLiteral, mergeRanges(leftBracket, rightBracket));
    v.ordered = true;
    v.leftDelimiter = leftBracket;
    v.members = members;
    v.rightDelimiter = rightBracket;
    return v;
}

export function EmptyOrderedStructLiteral(
    leftBracket: Terminal,
    colon: Terminal,
    rightBracket: Terminal,
) : StructLiteral {
    const v = NodeBase<StructLiteral>(NodeType.structLiteral, mergeRanges(leftBracket, rightBracket));
    v.ordered = true;
    v.leftDelimiter = leftBracket;
    v.members = [];
    v.emptyOrderedStructColon = colon;
    v.rightDelimiter = rightBracket;
    return v;
}

export interface StructLiteralInitializerMember extends NodeBase {
    type: NodeType.structLiteralInitializerMember;
    key: Node,
    colon: Terminal,
    expr: Node,
    comma: Node | null
}

export function StructLiteralInitializerMember(
    key: Node,
    colon: Terminal,
    expr: Node,
    comma: Node | null) : StructLiteralInitializerMember {
    const v = NodeBase<StructLiteralInitializerMember>(NodeType.structLiteralInitializerMember, mergeRanges(key, expr, comma));
    v.key = key;
    v.colon = colon;
    v.expr = expr;
    v.comma = comma;
    return v;
}

export interface ArrayLiteral extends NodeBase {
    type: NodeType.arrayLiteral,
    leftBracket: Terminal;
    members: ArrayLiteralInitializerMember[];
    rightBracket: Terminal;
}

export function ArrayLiteral(
    leftBracket: Terminal,
    members: ArrayLiteralInitializerMember[],
    rightBracket: Terminal
) : ArrayLiteral {
    const v = NodeBase<ArrayLiteral>(NodeType.arrayLiteral, mergeRanges(leftBracket, rightBracket));
    v.leftBracket = leftBracket
    v.members = members;
    v.rightBracket = rightBracket;
    return v;
}

export interface ArrayLiteralInitializerMember extends NodeBase {
    type: NodeType.arrayLiteralInitializerMember,
    expr: Node,
    comma: Terminal | null
}

export function ArrayLiteralInitializerMember(
    expr: Node,
    comma: Terminal | null
) : ArrayLiteralInitializerMember {
    const v = NodeBase<ArrayLiteralInitializerMember>(NodeType.arrayLiteralInitializerMember, mergeRanges(expr, comma));
    v.expr = expr;
    v.comma = comma;
    return v;
}

export interface Try extends NodeBase {
    type: NodeType.try,
    tryToken: Terminal,
    leftBrace: Terminal,
    body: Node[],
    rightBrace: Terminal,
    catchBlocks: Catch[],
    finallyBlock: Finally | null
}

export function Try(
    tryToken: Terminal,
    leftBrace: Terminal,
    body: Node[],
    rightBrace: Terminal,
    catchBlocks: Catch[],
    finallyBlock: Finally | null
) : Try {
    const v = NodeBase<Try>(NodeType.try, mergeRanges(tryToken, finallyBlock));
    v.tryToken = tryToken;
    v.leftBrace = leftBrace;
    v.body = body;
    v.rightBrace = rightBrace;
    v.catchBlocks = catchBlocks;
    v.finallyBlock = finallyBlock;
    return v;
}

export interface Catch extends NodeBase {
    type: NodeType.catch,
    catchToken: Terminal,
    leftParen: Terminal,
    exceptionType: DottedPath<Terminal>,
    exceptionBinding: Identifier,
    rightParen: Terminal,
    leftBrace: Terminal,
    body: Node[],
    rightBrace: Terminal
}

export function Catch(
    catchToken: Terminal,
    leftParen: Terminal,
    exceptionType: DottedPath<Terminal>,
    exceptionBinding: Identifier,
    rightParen: Terminal,
    leftBrace: Terminal,
    body: Node[],
    rightBrace: Terminal
) : Catch {
    const v = NodeBase<Catch>(NodeType.catch, mergeRanges(leftParen, rightBrace));
    v.catchToken = catchToken;
    v.leftParen = leftParen;
    v.exceptionType = exceptionType;
    v.exceptionBinding = exceptionBinding;
    v.rightParen = rightParen;
    v.leftBrace = leftBrace;
    v.body = body;
    v.rightBrace = rightBrace;
    return v;
}

export interface Finally extends NodeBase {
    type: NodeType.finally,
    finallyToken: Terminal,
    leftBrace: Terminal,
    body: Node[],
    rightBrace: Terminal
}

export function Finally(
    finallyToken: Terminal,
    leftBrace: Terminal,
    body: Node[],
    rightBrace: Terminal
) : Finally {
    const v = NodeBase<Finally>(NodeType.finally, mergeRanges(finallyToken, rightBrace));
    v.finallyToken = finallyToken;
    v.leftBrace = leftBrace;
    v.body = body;
    v.rightBrace = rightBrace;
    return v;
}

