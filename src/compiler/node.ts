import { SourceRange, TokenType, Token, NilToken, TokenTypeUiString, CfFileType } from "./scanner";
import { getAttributeValue, getTriviallyComputableBoolean, getTriviallyComputableString } from "./utils";
import { Type as Type } from "./types";

let debug = false;
let nextNodeId : NodeId = 0;

export function setDebug(isDebug: boolean) {
    debug = isDebug;
}

export const enum NodeFlags {
    none    = 0,
    error   = 0x00000001,
    missing = 0x00000002,
    checkerError = 0x00000004,
}

export const enum NodeType {
    sourceFile,
    terminal, textSpan, comment, hashWrappedExpr, parenthetical, tagAttribute,
    tag, callExpression, callArgument, unaryOperator, binaryOperator,
    conditional, variableDeclaration, statement, block, simpleStringLiteral, interpolatedStringLiteral, numericLiteral, booleanLiteral,
    identifier,
    indexedAccess, indexedAccessChainElement, sliceExpression,
    functionDefinition, arrowFunctionDefinition, functionParameter,
    dottedPath, switch, switchCase, do, while, ternary, for, structLiteral, arrayLiteral,
    structLiteralInitializerMember, arrayLiteralInitializerMember, try, catch, finally,
    breakStatement, continueStatement, returnStatement, importStatement,
    new, type, typeAttribute,
}

const NodeTypeUiString : Record<NodeType, string> = {
    [NodeType.sourceFile]: "program",
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
    [NodeType.block]: "block",
    [NodeType.simpleStringLiteral]: "simpleStringLiteral",
    [NodeType.interpolatedStringLiteral]: "interpolatedStringLiteral",
    [NodeType.numericLiteral]: "numericLiteral",
    [NodeType.booleanLiteral]: "booleanLiteral",
    [NodeType.identifier]: "identifier",
    [NodeType.indexedAccess]: "indexedAccess",
    [NodeType.indexedAccessChainElement]: "indexedAccessChainElement",
    [NodeType.sliceExpression]: "sliceExpression",
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
    [NodeType.continueStatement]: "continue",
    [NodeType.importStatement]: "import",
    [NodeType.new]: "new",
    [NodeType.type]: "type",
    [NodeType.typeAttribute]: "type-attribute",
};

export type Node =
    | SourceFile
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
    | SliceExpression
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
    | ImportStatement
    | New
    | DotAccess
    | BracketAccess
    | OptionalDotAccess
    | OptionalBracketAccess
    | OptionalCall
    | Type

export interface Term {
    type: Type,
    name: string,
    final: boolean,
    var: boolean,
    target: Node | undefined,
}

export interface SymTabEntry {
    uiName: string,
    canonicalName: string,
    firstBinding: Node | null,
    userType: Type | null,
    inferredType: Type | null,
    type: Type,
}

export type SymTab = Map<string, SymTabEntry>;

export type ScopeDisplay = {
    container: Node | null, // rename to parentContainer
    typedefs: Map<string, Type>,
} & {[name in StaticallyKnownScopeName]?: Map<string, SymTabEntry>}

const staticallyKnownScopeName = [
    "application",
    "arguments",
    "attributes",
    "caller",
    "cgi",
    "client",
    "cookie",
    "file",
    "form",
    "local",
    "query",
    "request",
    "server",
    "session",
    "this",
    "thisTag",
    "thread",
    "threadLocal",
    "url",
    "variables",
    "global", // fake scope where we stick things like `encodeForHTML` or etc.
] as const;

export type StaticallyKnownScopeName = (typeof staticallyKnownScopeName)[number];

export const isStaticallyKnownScopeName = (() => {
    const scopeNames = new Set<string>(staticallyKnownScopeName);
    return (name: string) : name is StaticallyKnownScopeName => scopeNames.has(name);
})();

export type NodeId = number;
export type TypeId = number;
export type FlowId = number;
export type IdentifierId = number;
export type NodeWithScope<N extends Node = Node, T extends (StaticallyKnownScopeName | never) = never> = N & {containedScope: ScopeDisplay & {[k in T]: SymTab}};

export const enum FlowType {
    default,
    assignment,
    postReturn
}

export interface Flow {
    flowId: FlowId,
    flowType: FlowType,
    predecessor: Flow[],
    successor: Flow | null,
    node: Node | null // this effectively be null while a FlowNode is waiting on a node to attach to; but by the time we get to the checker it will have been populated or the entire flownode discarded
}

export type ReachableFlow = Flow & {node: Node};

export interface NodeBase {
    kind: NodeType,
    nodeId: NodeId,
    parent: Node | null,
    range: SourceRange,

    typeAnnotation: Type | null,

    fromTag?: boolean,
    tagOrigin: { // todo: make this only present on particular tags, and flatten it
        startTag: CfTag | null,
        endTag: CfTag | null,
    }
    flags: NodeFlags,

    containedScope?: ScopeDisplay,
    links?: {
        symTabEntry?: SymTabEntry
    }
    flow: Flow | null,

    __debug_type?: string;
}

export function NodeBase<T extends NodeBase>(type: T["kind"], range: SourceRange = SourceRange.Nil()) : T {
    const result : Partial<T> = {};
    result.nodeId = nextNodeId++;
    result.kind = type;
    result.parent = null;
    result.range = range ?? null;
    result.typeAnnotation = null;
    result.tagOrigin = {
        startTag: null,
        endTag: null,
    }
    result.flags = NodeFlags.none;
    result.flow = null;

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

export interface SourceFile extends NodeBase {
    kind: NodeType.sourceFile,
    absPath: string,
    cfFileType: CfFileType,
    source: string | Buffer,
    content: Node[]
    libRefs: SourceFile[],
}

export function SourceFile(absPath: string, cfFileType: CfFileType, sourceText: string | Buffer) : SourceFile {
    const sourceFile = NodeBase<SourceFile>(NodeType.sourceFile);
    sourceFile.absPath = absPath;
    sourceFile.cfFileType = cfFileType;
    sourceFile.source = sourceText;
    sourceFile.content = [];
    sourceFile.libRefs = [];
    return sourceFile;
}

export const NilCfm = (text: string) => SourceFile("nil!", CfFileType.cfm, text);
export const NilCfc = (text: string) => SourceFile("nil!", CfFileType.cfc, text);
export const NilDCfm = (text: string) => SourceFile("nil!", CfFileType.dCfm, text);

export interface Terminal extends NodeBase {
    kind: NodeType.terminal;
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

export const NilTerminal = (pos: number) => Terminal(NilToken(pos));

export const freshFlow = (function() {
    let flowId = 0;
    return (predecessor: Flow | Flow[], flowType: FlowType, node: Node | null = null) : Flow => ({
        flowId: flowId++,
        flowType: flowType,
        predecessor: Array.isArray(predecessor) ? predecessor : [predecessor],
        successor: null,
        node});
})();

export const enum CommentType { tag, scriptSingleLine, scriptMultiLine };
export interface Comment extends NodeBase {
    kind: NodeType.comment;
    commentType: CommentType;
    typedefs?: Type[],
}

export function Comment(tagOrigin: CfTag.Comment) : Comment;
export function Comment(commentType: CommentType, range: SourceRange, typedefs?: Type[]) : Comment;
export function Comment(commentType: CfTag.Comment | CommentType, range?: SourceRange, typedefs?: Type[]) {
    if (typeof commentType === "number") { // overload 2
        const comment = NodeBase<Comment>(NodeType.comment, range);
        comment.commentType = commentType;
        if (typedefs) comment.typedefs = typedefs;
        return comment;
    }
    else { // overload 1
        const tagOrigin = commentType as CfTag.Comment;
        const comment = NodeBase<Comment>(NodeType.comment, tagOrigin.range);
        comment.commentType = CommentType.tag;
        if (tagOrigin.typedefs) {
            comment.typedefs = tagOrigin.typedefs;
        }
        return comment;
    }
}

export interface TextSpan extends NodeBase {
    kind: NodeType.textSpan;
    text: string;
}
export function TextSpan(sourceRange: SourceRange, text: string) : TextSpan {
    const textSpan = NodeBase<TextSpan>(NodeType.textSpan, sourceRange);
    textSpan.text = text;
    return textSpan;
}

export interface HashWrappedExpr extends NodeBase {
    kind: NodeType.hashWrappedExpr;
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
    kind: NodeType.parenthetical;
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
    kind: NodeType.tagAttribute;
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
        kind: NodeType.tag;
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
        scriptRange: SourceRange,
    }
    export function Script(
        tagStart: Terminal,
        tagName: Terminal,
        voidSlash: Terminal | null,
        tagEnd: Terminal,
        canonicalName: string,
        stmtList: Node[],
        scriptBodyRange: SourceRange) : Script {
        const v = TagBase<Script>(Which.start, TagType.script, tagStart, tagName, voidSlash, tagEnd, canonicalName);
        v.stmtList = stmtList;
        v.scriptRange = scriptBodyRange;
        return v;
    }

    export interface Text extends TagBase {
        tagType: TagType.text;
    }
    export function Text(range: SourceRange) : Text {
        const nilTerminal = NilTerminal(-1);
        const v = TagBase<Text>(Which.start, TagType.text, nilTerminal, nilTerminal, null, nilTerminal, "");
        v.range = range;
        return v;
    }

    export interface Comment extends TagBase {
        tagType: TagType.comment,
        body: TagBase[],
        typedefs?: Type[],
    }
    export function Comment(
        tagStart: Terminal,
        body: TagBase[],
        tagEnd: Terminal) : Comment {
        const nilTerminal = NilTerminal(-1);
        const v = TagBase<Comment>(Which.start, TagType.comment, tagStart, nilTerminal, nilTerminal, tagEnd, "");
        v.body = body;
        return v;
    }
}

export interface CallExpression extends NodeBase {
    kind: NodeType.callExpression;
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
    kind: NodeType.callArgument;
    name: Identifier | null;
    equals: Terminal | null;
    dotDotDot: Terminal | null;
    expr: Node;
    comma: Terminal | null;
}

export function CallArgument(name: Identifier | null, equals: Terminal | null, dotDotDot: Terminal | null, expr: Node, comma: Terminal | null) : CallArgument {
    const v = NodeBase<CallArgument>(NodeType.callArgument, mergeRanges(name, expr, comma));
    v.name = name;
    v.equals = equals;
    v.dotDotDot = dotDotDot;
    v.expr = expr;
    v.comma = comma;
    return v;
}

export const enum UnaryOperatorPos { pre, post };
export const enum UnaryOpType { inc, dec, pos, neg, not };
export interface UnaryOperator extends NodeBase {
    kind: NodeType.unaryOperator;
    pos: UnaryOperatorPos;
    optype: UnaryOpType;
    operator: Terminal;
    expr: Node;
}

export function UnaryOperator(expr: Node, op: Terminal) : UnaryOperator;
export function UnaryOperator(op: Terminal, expr: Node) : UnaryOperator;
export function UnaryOperator(lexicallyFirst: Node, lexicallyAfter: Node) {
    const v = NodeBase<UnaryOperator>(NodeType.unaryOperator, mergeRanges(lexicallyFirst, lexicallyAfter));
    if (lexicallyFirst.kind === NodeType.terminal) {
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
    and, or, xor,
    assign, assign_add, assign_sub, assign_mul, assign_div, assign_mod, assign_cat,
    contains, does_not_contain, strict_eq, strict_neq,
    equivalent, implies
}
export const BinaryOpTypeUiString : Record<BinaryOpType, string> = {
    [BinaryOpType.add]:              "+",
    [BinaryOpType.sub]:              "-",
    [BinaryOpType.mul]:              "*",
    [BinaryOpType.div]:              "/",
    [BinaryOpType.mod]:              "%",
    [BinaryOpType.exp]:              "exp", // in cf, "^", but that is easily confusable with the xor symbol from other langs so we spell it out
    [BinaryOpType.cat]:              "&",
    [BinaryOpType.eq]:               "==",
    [BinaryOpType.neq]:              "!=",
    [BinaryOpType.lt]:               "<",
    [BinaryOpType.lte]:              "<=",
    [BinaryOpType.gt]:               ">",
    [BinaryOpType.gte]:              ">=",
    [BinaryOpType.nullCoalesce]:     "?:",
    [BinaryOpType.and]:              "&&",
    [BinaryOpType.or]:               "||",
    [BinaryOpType.xor]:              "xor", // no cf operator symbol exists for this
    [BinaryOpType.assign]:           "=",
    [BinaryOpType.assign_add]:       "+=",
    [BinaryOpType.assign_cat]:       "&=",
    [BinaryOpType.assign_div]:       "/=",
    [BinaryOpType.assign_sub]:       "-=",
    [BinaryOpType.assign_mul]:       "*=",
    [BinaryOpType.assign_mod]:       "%=",
    [BinaryOpType.contains]:         "contains",
    [BinaryOpType.does_not_contain]: "does_not_contain",
    [BinaryOpType.strict_eq]:        "strict_eq",
    [BinaryOpType.strict_neq]:       "strict_neq",
    [BinaryOpType.equivalent]:       "eqv",
    [BinaryOpType.implies]:          "imp",
};

export interface BinaryOperator extends NodeBase {
    kind: NodeType.binaryOperator;
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

export function tokenTypeToBinaryOpType(tokenType: TokenType) : BinaryOpType {
    switch (tokenType) {
        case TokenType.PLUS:          		  return BinaryOpType.add;
        case TokenType.MINUS:         		  return BinaryOpType.sub;
        case TokenType.STAR:          		  return BinaryOpType.mul;
        case TokenType.FORWARD_SLASH: 		  return BinaryOpType.div;
        case TokenType.PERCENT:       		  return BinaryOpType.mod;
        case TokenType.LIT_MOD:       		  return BinaryOpType.mod;
        case TokenType.CARET:         		  return BinaryOpType.exp;
        case TokenType.AMPERSAND:     		  return BinaryOpType.cat;

        case TokenType.DBL_EQUAL:       	  return BinaryOpType.eq;
        case TokenType.LIT_EQ:        		  return BinaryOpType.eq;
        case TokenType.LIT_IS:                return BinaryOpType.eq;
        case TokenType.EXCLAMATION_EQUAL:	  return BinaryOpType.neq;
        case TokenType.LIT_NEQ:				  return BinaryOpType.neq;
        case TokenType.LIT_IS_NOT:            return BinaryOpType.neq;

        case TokenType.LEFT_ANGLE:    		  return BinaryOpType.lt;
        case TokenType.LIT_LT:                return BinaryOpType.lt;
        case TokenType.LEFT_ANGLE_EQUAL: 	  return BinaryOpType.lte;
        case TokenType.LIT_LTE:				  return BinaryOpType.lte;
        case TokenType.LIT_LE:				  return BinaryOpType.lte;

        case TokenType.RIGHT_ANGLE:    	      return BinaryOpType.gt;
        case TokenType.LIT_GT:				  return BinaryOpType.gt;
        case TokenType.RIGHT_ANGLE_EQUAL:     return BinaryOpType.gte;
        case TokenType.LIT_GTE:				  return BinaryOpType.gte;
        case TokenType.LIT_GE:				  return BinaryOpType.gte;

        case TokenType.DBL_PIPE:              return BinaryOpType.or;
        case TokenType.LIT_OR:                return BinaryOpType.or;
        case TokenType.DBL_AMPERSAND:         return BinaryOpType.and;
        case TokenType.LIT_AND:               return BinaryOpType.and;
        case TokenType.LIT_XOR:               return BinaryOpType.xor;

        case TokenType.EQUAL:                 return BinaryOpType.assign;
        case TokenType.AMPERSAND_EQUAL:       return BinaryOpType.assign_cat;
        case TokenType.PLUS_EQUAL:            return BinaryOpType.assign_add;
        case TokenType.MINUS_EQUAL:           return BinaryOpType.assign_sub;
        case TokenType.STAR_EQUAL:            return BinaryOpType.assign_mul;
        case TokenType.FORWARD_SLASH_EQUAL:   return BinaryOpType.assign_div;
        case TokenType.PERCENT_EQUAL:         return BinaryOpType.assign_mod;

        case TokenType.QUESTION_MARK_COLON:   return BinaryOpType.nullCoalesce;

        case TokenType.LIT_CONTAINS:          return BinaryOpType.contains;
        case TokenType.LIT_DOES_NOT_CONTAIN:  return BinaryOpType.does_not_contain;

        case TokenType.LIT_EQV:               return BinaryOpType.equivalent;
        case TokenType.LIT_IMP:               return BinaryOpType.implies;
        case TokenType.TRIPLE_EQUAL:          return BinaryOpType.strict_eq;
        case TokenType.EXCLAMATION_DBL_EQUAL: return BinaryOpType.strict_neq;
        default: break;
    }
    throw "bad binary op type transform";
}

export const enum ConditionalSubtype { if, elseif, else };

interface ConditionalBase extends NodeBase {
    kind: NodeType.conditional,
    subType: ConditionalSubtype,
    fromTag: boolean,
    consequent: Node,
    alternative: ConditionalBase | null,
}

export type Conditional = Script.Conditional | Tag.Conditional;


export namespace Script {
    export interface Conditional extends ConditionalBase {
        kind: NodeType.conditional,
        fromTag     : false,
        elseToken   : Terminal | null,
        ifToken     : Terminal | null,
        leftParen   : Terminal | null,
        expr        : Node | null,
        rightParen  : Terminal | null,
        consequent  : Node,
        alternative : Conditional | null,
    }

    export function If(ifToken: Terminal, leftParen: Terminal, expr: Node, rightParen: Terminal, consequent: Node) : Conditional {
        const v       = NodeBase<Conditional>(NodeType.conditional, mergeRanges(ifToken, consequent));
        v.fromTag     = false;
        v.subType     = ConditionalSubtype.if;
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
        v.fromTag     = false;
        v.subType     = ConditionalSubtype.elseif;
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
        v.fromTag     = false;
        v.subType     = ConditionalSubtype.else;
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

export namespace Tag {
    export interface Conditional extends ConditionalBase {
        kind: NodeType.conditional,
        fromTag: true,
        tagOrigin: {startTag: CfTag.ScriptLike | CfTag.Common, endTag: CfTag.Common | null},
        consequent: Node,
        alternative: Conditional | null,
    }

    export function If(startTag: CfTag.ScriptLike, consequent: Block, alternative: Conditional | null, endTag: CfTag.Common) : Conditional {
        const v = NodeBase<Conditional>(NodeType.conditional, mergeRanges(startTag, endTag));
        v.fromTag            = true;
        v.subType            = ConditionalSubtype.if;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag   = endTag;
        v.consequent         = consequent;
        v.alternative        = alternative;
        return v;
    }

    export function ElseIf(startTag: CfTag.ScriptLike, block: Block, alternative: Conditional | null) : Conditional {
        const v = NodeBase<Conditional>(NodeType.conditional, mergeRanges(startTag, block));
        v.fromTag            = true;
        v.subType            = ConditionalSubtype.elseif;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag   = null;
        v.consequent         = block;
        v.alternative        = alternative;
        return v;
    }

    export function Else(startTag: CfTag.Common, block: Block) : Conditional {
        const v = NodeBase<Conditional>(NodeType.conditional, mergeRanges(startTag, block));
        v.fromTag            = true;
        v.subType            = ConditionalSubtype.else;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag   = null;
        v.consequent         = block;
        v.alternative        = null;
        return v;
    }
}

export interface VariableDeclaration extends NodeBase {
    kind: NodeType.variableDeclaration,
    finalModifier: Terminal | null,
    varModifier: Terminal | null,
    expr: Node,
}

export function VariableDeclaration(
    finalModifier: Terminal | null,
    varModifier: Terminal | null,
    expr: Node
) : VariableDeclaration {
    const v = NodeBase<VariableDeclaration>(NodeType.variableDeclaration, mergeRanges(finalModifier, varModifier, expr));
    v.finalModifier = finalModifier;
    v.varModifier = varModifier;
    v.expr = expr;
    return v;
}

export const enum StatementType {
    fromTag,                       // any loose, no-bodied tag like `<cfhttpparam ...>`
    expressionWrapper,             // any expr in a statement context, mostly so that we can bind a semicolon to it
    scriptSugaredTagCallStatement, // a sugared script tag statement, like `transaction action=rollback`
    scriptTagCallStatement         // similar to from tag, but its a script-call, like `cfhttpparam(...);`
}
const StatementTypeUiString : Record<StatementType, string> = {
    [StatementType.fromTag]: "fromTag",
    [StatementType.expressionWrapper]: "expressionWrapper",
    [StatementType.scriptSugaredTagCallStatement]: "scriptSugaredCallStatement",
    [StatementType.scriptTagCallStatement]: "scriptTagCallStatement"
}
export interface Statement extends NodeBase {
    kind: NodeType.statement,
    subType: StatementType,
    expr: Node | null,              // null if from tag (originating node will be in tagOrigin.startTag)
    scriptSugaredTagStatement: {    // sugaredScriptTagCalls with no body become statements, and may have attributes; like `transaction action=rollback;`
        attrs: TagAttribute[],   
    } | null,
    callStatement: {       // e.g, cfhttp(foo=bar);
        leftParen: Terminal,
        args: CallArgument[],
        rightParen: Terminal,
    } | null,
    semicolon : Terminal | null,    // null if from tag
    __debug_subtype?: string
}

// e.g, `transaction action="foo";`
export function ScriptSugaredTagCallStatement(name: Terminal, attrs: TagAttribute[], semicolon: Terminal | null) : Statement {
    const v = NodeBase<Statement>(NodeType.statement, mergeRanges(name, attrs, semicolon));
    v.subType = StatementType.scriptSugaredTagCallStatement;
    v.expr = name;
    v.scriptSugaredTagStatement = {attrs};
    v.callStatement = null;
    v.semicolon = semicolon;

    if (debug) {
        v.__debug_subtype = StatementTypeUiString[StatementType.scriptSugaredTagCallStatement];
    }

    return v;
}

// e.g, `cftransaction(action="foo");`
export function ScriptTagCallStatement(name: Terminal, leftParen: Terminal, args: CallArgument[], rightParen: Terminal, semicolon: Terminal | null) : Statement {
    const v = NodeBase<Statement>(NodeType.statement, mergeRanges(name, rightParen, semicolon));
    v.subType = StatementType.scriptTagCallStatement;
    v.expr = name;
    v.scriptSugaredTagStatement = null;
    v.callStatement = {leftParen, args, rightParen};
    v.semicolon = semicolon;

    if (debug) {
        v.__debug_subtype = StatementTypeUiString[StatementType.scriptTagCallStatement];
    }

    return v;
}

export function Statement(node: Node | null, semicolon: Terminal | null) : Statement {
    const v = NodeBase<Statement>(NodeType.statement, mergeRanges(node, semicolon));
    v.subType = StatementType.expressionWrapper;
    v.expr = node;
    v.scriptSugaredTagStatement = null;
    v.callStatement = null;
    v.semicolon = semicolon;

    if (debug) {
        v.__debug_subtype = StatementTypeUiString[StatementType.expressionWrapper];
    }

    return v;
}

export namespace FromTag {
    export function CfSetExpressionWrapper(tag: CfTag.ScriptLike) {
        let stmt : Statement;
        stmt = NodeBase<Statement>(NodeType.statement, tag.range);
        stmt.subType = StatementType.expressionWrapper;
        stmt.expr = tag.expr;
        stmt.tagOrigin.startTag = tag;
        stmt.semicolon = null;

        if (debug) {
            stmt.__debug_subtype = StatementTypeUiString[StatementType.expressionWrapper]
        }

        return stmt;
    }

    export function Statement(tag: CfTag.Common) : Statement {
        const stmt = NodeBase<Statement>(NodeType.statement, tag.range);
        stmt.subType = StatementType.fromTag;
        stmt.expr = null;
        stmt.tagOrigin.startTag = tag;
        stmt.semicolon = null;

        if (debug) {
            stmt.__debug_subtype = StatementTypeUiString[StatementType.fromTag]
        }

        return stmt;
    }
}

export interface ReturnStatement extends Omit<Statement, "kind"> {
    kind: NodeType.returnStatement;
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
    kind: NodeType.breakStatement,
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
    kind: NodeType.continueStatement,
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
export const enum BlockType {
    fromTag,                            // a full tag block, like <cftransaction> ... </cftransaction>
    scriptSugaredTagCallBlock, // a "named" block, where the name is a sugared tag name: `transaction <attrs> { ... }`
    scriptTagCallBlock,              // a tag call in script syntax with body: `cftransaction(action=foo) { ... }`
    cLike                               // a typical c-like block, just `{ ... }`
}
export interface Block extends NodeBase {
    kind: NodeType.block,
    subType: BlockType,
    name: Terminal | null,
    sugaredCallStatementAttrs: TagAttribute[] | null,
    tagCallStatementArgs: {
        leftParen: Terminal,
        args: CallArgument[],
        rightParen: Terminal
    } | null,
    leftBrace: Terminal | null; // tag-origin blocks will have no braces
    stmtList: Node[];
    rightBrace: Terminal | null;
}

// e.g, `transaction action='foo' {}`
export function ScriptSugaredTagCallBlock(name: Terminal, attrs: TagAttribute[], block: Block) : Block {
    block.subType = BlockType.scriptSugaredTagCallBlock;
    block.name = name;
    block.sugaredCallStatementAttrs = attrs;
    return block;
}

// e.g, `cftransaction(action='foo') {}`
export function ScriptTagCallBlock(name: Terminal, leftParen: Terminal, args: CallArgument[], rightParen: Terminal, block: Block) : Block {
    block.subType              = BlockType.scriptTagCallBlock;
    block.name                 = name;
    block.tagCallStatementArgs = {leftParen, args, rightParen};
    return block;
}

export function Block(leftBrace: Terminal | null, stmtList: Node[], rightBrace: Terminal | null) : Block {
    const v                = NodeBase<Block>(NodeType.block, mergeRanges(leftBrace, rightBrace));
    v.subType              = BlockType.cLike;
    v.name                 = null;
    v.tagCallStatementArgs = null;
    v.tagCallStatementArgs = null;
    v.leftBrace         = leftBrace;
    v.stmtList          = stmtList;
    v.rightBrace        = rightBrace;
    return v;
}

export namespace FromTag {
    export function looseStatementsBlock(stmtList: Node[]) {
        const v = NodeBase<Block>(NodeType.block, mergeRanges(...stmtList));
        v.tagOrigin.startTag   = null;
        v.tagOrigin.endTag     = null;

        v.subType              = BlockType.fromTag;
        v.name                 = null;
        v.tagCallStatementArgs = null;
        v.tagCallStatementArgs = null;
        v.leftBrace            = null;
        v.stmtList             = stmtList;
        v.rightBrace           = null;
        return v;
    }
    export function Block(startTag: CfTag, endTag: CfTag) : Block;                                      // overload 1
    export function Block(startTag: CfTag, stmtList: Node[], endTag: CfTag | null) : Block;             // overload 2
    export function Block(startTag: CfTag, endTagOrStmtList: CfTag | Node[], endTag?: CfTag | null) {
        if (endTag === undefined) { // overload 1
            const v = NodeBase<Block>(NodeType.block, mergeRanges(startTag, endTagOrStmtList));
            v.tagOrigin.startTag   = startTag;
            v.tagOrigin.endTag     = endTagOrStmtList as CfTag;

            v.subType              = BlockType.fromTag;
            v.name                 = null;
            v.tagCallStatementArgs = null;
            v.tagCallStatementArgs = null;
            v.leftBrace            = null;
            v.stmtList             = [];
            v.rightBrace           = null;
            return v;
        }
        else { // overload 2
            const v = NodeBase<Block>(NodeType.block, mergeRanges(startTag, endTag));
            v.tagOrigin.startTag   = startTag;
            v.tagOrigin.endTag     = endTag;

            v.subType              = BlockType.fromTag;
            v.name                 = null;
            v.tagCallStatementArgs = null;
            v.tagCallStatementArgs = null
            v.leftBrace            = null;
            v.stmtList             = endTagOrStmtList as Node[];
            v.rightBrace           = null;
            return v;
        }
    }
}

export interface SimpleStringLiteral extends NodeBase {
    kind: NodeType.simpleStringLiteral;
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
    kind: NodeType.interpolatedStringLiteral;
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
    kind: NodeType.numericLiteral;
    literal: Terminal;
}

export function NumericLiteral(literal: Terminal) : NumericLiteral {
    const v = NodeBase<NumericLiteral>(NodeType.numericLiteral, literal.range);
    v.literal = literal;
    return v;
}

export interface BooleanLiteral extends NodeBase {
    kind: NodeType.booleanLiteral;
    literal: Terminal;
    booleanValue: boolean;
}

export function BooleanLiteral(literal: Terminal, value: boolean) {
    const v = NodeBase<BooleanLiteral>(NodeType.booleanLiteral, literal.range);
    v.literal = literal;
    v.booleanValue = value;
    return v;
}

export interface Identifier extends NodeBase {
    kind: NodeType.identifier;
    source: Node; // can be e.g, `var x = 42`, `var 'x' = 42`, `var #x# = 42`; <cfargument name="#'interpolated_string_but_constant'#">`
    canonicalName: string | undefined; // undefined at least in the case of something like var '#foo#' = bar;
}

export function Identifier(identifier: Node, name: string | undefined) {
    const v = NodeBase<Identifier>(NodeType.identifier, identifier.range);
    v.source = identifier;
    v.canonicalName = name?.toLowerCase();
    return v;
}

export const enum IndexedAccessType { dot, bracket, optionalDot, optionalBracket, optionalCall };

export interface DotAccess extends NodeBase {
    kind: NodeType.indexedAccessChainElement,
    accessType: IndexedAccessType.dot,
    dot: Terminal,
    property: Terminal,
    __debug_access_type?: string
}

export interface BracketAccess extends NodeBase {
    kind: NodeType.indexedAccessChainElement,
    accessType: IndexedAccessType.bracket,
    leftBracket: Terminal,
    expr: Node,
    rightBracket: Terminal,
    __debug_access_type?: string
}

// note that for the optional accesses, like with the null coalescing operator `?:`
// it is not one token, but two separate tokens, possibly with comments between them
export interface OptionalDotAccess extends NodeBase {
    kind: NodeType.indexedAccessChainElement,
    accessType: IndexedAccessType.optionalDot,
    questionMark: Terminal,
    dot: Terminal,
    property: Terminal,
    __debug_access_type?: string
}

export interface OptionalBracketAccess extends NodeBase {
    kind: NodeType.indexedAccessChainElement,
    accessType: IndexedAccessType.optionalBracket,
    questionMark: Terminal,
    dot: Terminal,
    leftBracket: Terminal,
    expr: Node,
    rightBracket: Terminal,
    __debug_access_type?: string
}

export interface OptionalCall extends NodeBase {
    kind: NodeType.indexedAccessChainElement,
    accessType: IndexedAccessType.optionalCall,
    questionMark: Terminal,
    dot: Terminal,
    __debug_access_type?: string
    // the call itself will "own" this node as its left-side
    // foo?.() is a call expression where the left side is an indexed-access expression with a trailing OptionalCall access chain element
}

export interface SliceExpression extends NodeBase {
    kind: NodeType.sliceExpression,
    from : Node | null,
    colon1: Terminal,
    to : Node | null,
    colon2: Terminal,
    stride : Node | null,
}

export function SliceExpression(
    from : Node | null,
    colon1: Terminal,
    to : Node | null,
    colon2: Terminal,
    stride : Node | null) : SliceExpression {
    const v = NodeBase<SliceExpression>(NodeType.sliceExpression, mergeRanges(from, colon1, to, colon2, stride));
    v.from = from;
    v.colon1 = colon1;
    v.to = to;
    v.colon2 = colon2;
    v.stride = stride;
    return v;
}

export type IndexedAccessChainElement =
    | DotAccess
    | BracketAccess
    | OptionalDotAccess
    | OptionalBracketAccess
    | OptionalCall

export function DotAccess(dot: Terminal, property: Terminal) : DotAccess {
    const node = NodeBase<DotAccess>(NodeType.indexedAccessChainElement, mergeRanges(dot, property));
    node.accessType = IndexedAccessType.dot;
    node.dot = dot;
    node.property = property;
    if (debug) (<IndexedAccessChainElement>node).__debug_access_type = "dot";
    return node;
}

export function BracketAccess(leftBracket: Terminal, expr: Node, rightBracket: Terminal) : BracketAccess {
    const node = NodeBase<BracketAccess>(NodeType.indexedAccessChainElement, mergeRanges(leftBracket, rightBracket));
    node.accessType = IndexedAccessType.bracket;
    node.leftBracket = leftBracket;
    node.expr = expr;
    node.rightBracket = rightBracket;
    if (debug) (<IndexedAccessChainElement>node).__debug_access_type = "bracket";
    return node;
}

export function OptionalDotAccess(questionMark: Terminal, dot: Terminal, property: Terminal) : OptionalDotAccess {
    const node = NodeBase<OptionalDotAccess>(NodeType.indexedAccessChainElement, mergeRanges(questionMark, property));
    node.accessType = IndexedAccessType.optionalDot;
    node.questionMark = questionMark;
    node.dot = dot;
    node.property = property;
    if (debug) (<IndexedAccessChainElement>node).__debug_access_type = "optional-dot";
    return node;
}

export function OptionalBracketAccess(questionMark: Terminal, dot: Terminal, leftBracket: Terminal, expr: Node, rightBracket: Terminal) : OptionalBracketAccess {
    const node = NodeBase<OptionalBracketAccess>(NodeType.indexedAccessChainElement, mergeRanges(questionMark, rightBracket));
    node.accessType = IndexedAccessType.optionalBracket;
    node.questionMark = questionMark;
    node.dot = dot;
    node.leftBracket = leftBracket;
    node.expr = expr;
    node.rightBracket = rightBracket;
    if (debug) (<IndexedAccessChainElement>node).__debug_access_type = "optional-bracket";
    return node;
}

export function OptionalCall(questionMark: Terminal, dot: Terminal) : OptionalCall {
    const node = NodeBase<OptionalCall>(NodeType.indexedAccessChainElement, mergeRanges(questionMark, dot));
    node.accessType = IndexedAccessType.optionalCall;
    node.questionMark = questionMark;
    node.dot = dot;
    if (debug) (<IndexedAccessChainElement>node).__debug_access_type = "optional-call";
    return node;
}

export interface IndexedAccess extends NodeBase {
    kind: NodeType.indexedAccess,
    root: Node,
    accessElements: IndexedAccessChainElement[],
}

export function IndexedAccess(root: Node) : IndexedAccess {
    // make a copy of the source range, so that we can update the range of the full indexed access expression without mutating
    // the root's original range
    const v = NodeBase<IndexedAccess>(NodeType.indexedAccess, new SourceRange(root.range.fromInclusive, root.range.toExclusive));
    v.root = root;
    v.accessElements = [];
    return v;
}

export function pushAccessElement(base: IndexedAccess, element: IndexedAccessChainElement) : void {
    base.accessElements.push(element);
    base.range.toExclusive = element.range.toExclusive;
}

interface FunctionParameterBase extends NodeBase {
    kind: NodeType.functionParameter,
    required: boolean | null,
    fromTag: boolean,
}

export type FunctionParameter = Script.FunctionParameter | Tag.FunctionParameter;

export function copyFunctionParameterForTypePurposes(param: FunctionParameter) {
    if (param.fromTag) {
        return Tag.FunctionParameter(param.tagOrigin.startTag! as CfTag.Common, param.type);
    }
    return Script.FunctionParameter(param.requiredTerminal, param.javaLikeTypename, param.dotDotDot, param.identifier, param.equals, param.defaultValue, param.comma, param.type)
}

export namespace Script {
    export interface FunctionParameter extends FunctionParameterBase {
        kind: NodeType.functionParameter,
        fromTag: false,
        requiredTerminal: Terminal | null,
        javaLikeTypename: DottedPath<Terminal> | null,
        dotDotDot: Terminal | null,
        identifier: Identifier,
        equals: Terminal | null,
        defaultValue: Node | null,
        comma: Terminal | null,
        canonicalName: string,
        type: Type | null,
    }

    export function FunctionParameter(
        requiredTerminal : Terminal | null,
        javaLikeTypename: DottedPath<Terminal> | null,
        dotDotDot: Terminal | null,
        identifier: Identifier,
        equals: Terminal | null,
        defaultValue: Node | null,
        comma: Terminal | null,
        type: Type | null) : FunctionParameter {
        const v = NodeBase<FunctionParameter>(NodeType.functionParameter, mergeRanges(requiredTerminal, javaLikeTypename, identifier, defaultValue, comma));
        v.fromTag = false;
        v.requiredTerminal = requiredTerminal;
        v.javaLikeTypename = javaLikeTypename;
        v.dotDotDot = dotDotDot;
        v.identifier = identifier;
        v.equals = equals;
        v.defaultValue = defaultValue;
        v.comma = comma;
        v.canonicalName = identifier.canonicalName || "<<ERROR>>";
        v.type = type;
        v.required = !!(requiredTerminal);
        return v;
    }
}

export namespace Tag {
    export interface FunctionParameter extends FunctionParameterBase {
        kind: NodeType.functionParameter,
        fromTag: true,
        canonicalName: string,
        type: Type | null,
    }

    export function FunctionParameter(tag: CfTag.Common, type: Type | null) : FunctionParameter {
        const v = NodeBase<FunctionParameter>(NodeType.functionParameter, tag.range);
        v.fromTag = true;
        v.tagOrigin.startTag = tag;
        v.canonicalName = getTriviallyComputableString(getAttributeValue(tag.attrs, "name"))?.toLowerCase() ?? "<<ERROR>>";
        v.required = getTriviallyComputableBoolean(getAttributeValue(tag.attrs, "required")) ?? null;
        v.type = type;
        return v;
    }
}

interface FunctionDefinitionBase extends NodeBase {
    kind: NodeType.functionDefinition,
    fromTag: boolean,
}

export type FunctionDefinition = Script.FunctionDefinition | Tag.FunctionDefinition;

export namespace Script {
    export interface FunctionDefinition extends FunctionDefinitionBase {
        kind: NodeType.functionDefinition;
        fromTag        : false,
        accessModifier : Terminal | null,
        returnType     : DottedPath<Terminal> | null,
        functionToken  : Terminal,
        nameToken      : Terminal | null,
        leftParen      : Terminal,
        params         : FunctionParameter[],
        rightParen     : Terminal,
        attrs          : TagAttribute[],
        body           : Block,
        canonicalName  : string | null,
        returnTypeAnnotation : Type | null,
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
        body          : Block,
        returnTypeAnnotation : Type | null
    ) : FunctionDefinition {
        const v = NodeBase<FunctionDefinition>(NodeType.functionDefinition, mergeRanges(accessModifier, returnType, functionToken, body));
        v.fromTag        = false;
        v.accessModifier = accessModifier;
        v.returnType     = returnType;
        v.functionToken  = functionToken;
        v.nameToken      = nameToken;
        v.leftParen      = leftParen;
        v.params         = params;
        v.rightParen     = rightParen;
        v.attrs          = attrs;
        v.body           = body;
        v.returnTypeAnnotation = returnTypeAnnotation;
        v.canonicalName  = nameToken?.token.text.toLowerCase() ?? null;
        return v;
    }
}

export namespace Tag {
    export interface FunctionDefinition extends FunctionDefinitionBase {
        kind: NodeType.functionDefinition;
        fromTag        : true,
        params         : FunctionParameter[],
        body           : Node[], // fixme: block?
        canonicalName  : string | undefined,
    }

    export function FunctionDefinition(startTag: CfTag.Common, params: FunctionParameter[], body: Node[], endTag: CfTag.Common) : FunctionDefinition {
        const v = NodeBase<FunctionDefinition>(NodeType.functionDefinition, mergeRanges(startTag, endTag));
        v.fromTag            = true;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag   = endTag;
        v.params             = params;
        v.body               = body;
        v.canonicalName      = getTriviallyComputableString(getAttributeValue(startTag.attrs, "name"))?.toLowerCase();
        return v;
    }
}

export interface ArrowFunctionDefinition extends NodeBase {
    kind: NodeType.arrowFunctionDefinition;
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

// @fixme clean this up
export interface DottedPath<T extends NodeBase> extends NodeBase {
    kind: NodeType.dottedPath;
    headKey: T;
    rest: {dot: Terminal, key: T}[]
}
export function DottedPath<T extends NodeBase>(headKey: T) : DottedPath<T> {
    const v = NodeBase<DottedPath<T>>(NodeType.dottedPath, headKey.range);
    v.headKey = headKey;
    v.rest = [];
    return v;
}

interface SwitchBase extends NodeBase {
    kind: NodeType.switch;
    fromTag: boolean,
    cases: SwitchCaseBase[],
}

export type Switch = Script.Switch | Tag.Switch;

export namespace Script {
    export interface Switch extends SwitchBase {
        kind: NodeType.switch;
        fromTag: false,
        switchToken: Terminal;
        leftParen: Terminal;
        expr: Node;
        rightParen: Terminal;
        leftBrace: Terminal;
        cases: Script.SwitchCase[];
        rightBrace: Terminal;
    }

    export function Switch(
        switchToken: Terminal,
        leftParen: Terminal,
        expr: Node,
        rightParen: Terminal,
        leftBrace: Terminal,
        cases: Script.SwitchCase[],
        rightBrace: Terminal) : Switch {
        const v = NodeBase<Switch>(NodeType.switch, mergeRanges(switchToken, rightBrace));
        v.fromTag = false;
        v.switchToken = switchToken;
        v.leftParen = leftParen;
        v.expr = expr;
        v.rightParen = rightParen;
        v.leftBrace = leftBrace;
        v.cases = cases;
        v.rightBrace = rightBrace;
        return v;
    }
}

export namespace Tag {
    export interface Switch extends SwitchBase {
        kind: NodeType.switch,
        fromTag: true,
        cases: Tag.SwitchCase[],
    }
    export function Switch(startTag: CfTag.Common, cases: Tag.SwitchCase[], endTag: CfTag.Common) : Tag.Switch{
        const v = NodeBase<Switch>(NodeType.switch, mergeRanges(startTag, endTag));
        v.fromTag = true;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag = endTag;
        v.cases = cases;
        return v;
    }
}

interface SwitchCaseBase extends NodeBase {
    kind: NodeType.switchCase;
    fromTag: boolean,
    caseType: SwitchCaseType;
    body: Node[];
}

export type SwitchCase = Script.SwitchCase | Tag.SwitchCase;

export const enum SwitchCaseType { case, default };

export namespace Script {
    export interface SwitchCase extends SwitchCaseBase {
        kind: NodeType.switchCase,
        fromTag: false,
        caseType: SwitchCaseType,
        caseOrDefaultToken: Terminal,
        expr: Node | null,
        colon: Terminal,
        body: Node[],
    }

    export function SwitchCase(caseToken: Terminal, expr: Node, colon: Terminal, body: Node[]) : SwitchCase {
        const v = NodeBase<SwitchCase>(NodeType.switchCase, mergeRanges(caseToken, body));
        v.fromTag = false;
        v.caseType = SwitchCaseType.case
        v.caseOrDefaultToken = caseToken;
        v.expr = expr;
        v.colon = colon;
        v.body = body;
        return v;
    }
    export function SwitchDefault(defaultToken: Terminal, colon: Terminal, body: Node[]) : SwitchCase {
        const v = NodeBase<SwitchCase>(NodeType.switchCase, mergeRanges(defaultToken, body));
        v.fromTag = false;
        v.caseType = SwitchCaseType.default;
        v.caseOrDefaultToken = defaultToken;
        v.expr = null;
        v.colon = colon;
        v.body = body;
        return v;
    }
}

export namespace Tag {
    export interface SwitchCase extends SwitchCaseBase {
        kind: NodeType.switchCase,
        fromTag: true,
        caseType: SwitchCaseType,
        body: Node[],
    }

    export function SwitchCase(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) : SwitchCase {
        const v = NodeBase<SwitchCase>(NodeType.switchCase, mergeRanges(startTag, endTag));
        v.fromTag = true;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag = endTag;
        v.caseType = SwitchCaseType.case
        v.body = body;
        return v;
    }
    export function SwitchDefault(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) : SwitchCase {
        const v = NodeBase<SwitchCase>(NodeType.switchCase, mergeRanges(startTag, endTag));
        v.fromTag = true;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag = endTag;
        v.caseType = SwitchCaseType.default;
        v.body = body;
        return v;
    }
}

export interface Do extends NodeBase {
    kind: NodeType.do;
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
    kind: NodeType.while;
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
    kind: NodeType.ternary;
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
export type For = ForExpr | ForIn;

interface ForBase extends NodeBase {
    kind: NodeType.for;
    subType: ForSubType;
    forToken: Terminal;
    leftParen: Terminal;
    rightParen: Terminal;
    body: Node;
}

export interface ForExpr extends ForBase {
    subType: ForSubType.for,
    initExpr: Node | null;
    semi1: Terminal;
    conditionExpr: Node | null;
    semi2: Terminal;
    incrementExpr: Node | null;
}

export interface ForIn extends ForBase {
    subType: ForSubType.forIn,
    init: Node,
    inToken: Terminal,
    expr: Node,
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
        body: Node) : ForExpr {
        const v = NodeBase<ForExpr>(NodeType.for, mergeRanges(forToken, body));
        v.subType = ForSubType.for;
        v.forToken = forToken;
        v.leftParen = leftParen;
        v.initExpr = initExpr;
        v.semi1 = semi1;
        v.conditionExpr = conditionExpr;
        v.semi2 = semi2;
        v.incrementExpr = incrementExpr;
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
        body: Node) : ForIn {
        const v = NodeBase<ForIn>(NodeType.for, mergeRanges(forToken, body));
        v.subType = ForSubType.forIn;
        v.forToken = forToken;
        v.leftParen = leftParen;
        v.init = init;
        v.inToken = inToken;
        v.expr = expr;
        v.rightParen = rightParen;
        v.body = body;
        return v;
    }
}

export interface StructLiteral extends NodeBase {
    kind: NodeType.structLiteral,
    ordered: boolean,
    leftDelimiter: Terminal,
    members: StructLiteralInitializerMember[],
    emptyOrderedStructColon?: Terminal,
    rightDelimiter: Terminal,
}

export function StructLiteral(
    leftDelimiter: Terminal,
    members: StructLiteralInitializerMember[],
    rightDelimiter: Terminal,
) : StructLiteral {
    const v = NodeBase<StructLiteral>(NodeType.structLiteral, mergeRanges(leftDelimiter, rightDelimiter));
    v.ordered = false;
    v.leftDelimiter = leftDelimiter;
    v.members = members;
    v.rightDelimiter = rightDelimiter;
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

export type StructLiteralInitializerMember = KeyedStructLiteralInitializerMember | SpreadStructLiteralInitializerMember;

export const enum StructLiteralInitializerMemberSubtype { keyed, spread };

interface StructLiteralInitializerMemberBase extends NodeBase {
    kind: NodeType.structLiteralInitializerMember;
    subType: StructLiteralInitializerMemberSubtype,
}

export interface KeyedStructLiteralInitializerMember extends StructLiteralInitializerMemberBase {
    kind: NodeType.structLiteralInitializerMember;
    subType: StructLiteralInitializerMemberSubtype.keyed,
    key: Node,
    colon: Terminal,
    expr: Node,
    comma: Terminal | null
}

export function KeyedStructLiteralInitializerMember(
    key: Node,
    colon: Terminal,
    expr: Node,
    comma: Terminal | null) : KeyedStructLiteralInitializerMember {
    const v = NodeBase<KeyedStructLiteralInitializerMember>(NodeType.structLiteralInitializerMember, mergeRanges(key, expr, comma));
    v.subType = StructLiteralInitializerMemberSubtype.keyed;
    v.key = key;
    v.colon = colon;
    v.expr = expr;
    v.comma = comma;
    return v;
}

export interface SpreadStructLiteralInitializerMember extends StructLiteralInitializerMemberBase {
    kind: NodeType.structLiteralInitializerMember;
    subType: StructLiteralInitializerMemberSubtype.spread,
    dotDotDot: Terminal,
    expr: Node,
    comma: Terminal| null,
}

export function SpreadStructLiteralInitializerMember(
    dotDotDot: Terminal,
    expr: Node,
    comma: Terminal | null) : SpreadStructLiteralInitializerMember {
    const v = NodeBase<SpreadStructLiteralInitializerMember>(NodeType.structLiteralInitializerMember, mergeRanges(dotDotDot, expr, comma));
    v.subType = StructLiteralInitializerMemberSubtype.spread;
    v.dotDotDot = dotDotDot;
    v.expr = expr;
    v.comma = comma;
    return v;
}

export interface ArrayLiteral extends NodeBase {
    kind: NodeType.arrayLiteral,
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

export type ArrayLiteralInitializerMember = SimpleArrayLiteralInitializerMember | SpreadArrayLiteralInitializerMember;

export const enum ArrayLiteralInitializerMemberSubtype { simple, spread }

export interface SimpleArrayLiteralInitializerMember extends NodeBase {
    kind: NodeType.arrayLiteralInitializerMember,
    subType: ArrayLiteralInitializerMemberSubtype.simple,
    expr: Node,
    comma: Terminal | null
}

export function SimpleArrayLiteralInitializerMember(
    expr: Node,
    comma: Terminal | null
) : SimpleArrayLiteralInitializerMember {
    const v = NodeBase<SimpleArrayLiteralInitializerMember>(NodeType.arrayLiteralInitializerMember, mergeRanges(expr, comma));
    v.subType = ArrayLiteralInitializerMemberSubtype.simple;
    v.expr = expr;
    v.comma = comma;
    return v;
}

export interface SpreadArrayLiteralInitializerMember extends NodeBase {
    kind: NodeType.arrayLiteralInitializerMember,
    subType: ArrayLiteralInitializerMemberSubtype.spread,
    dotDotDot: Terminal,
    expr: Node,
    comma: Terminal | null
}

export function SpreadArrayLiteralInitializerMember(
    dotDotDot: Terminal,
    expr: Node,
    comma: Terminal | null
) : SpreadArrayLiteralInitializerMember {
    const v = NodeBase<SpreadArrayLiteralInitializerMember>(NodeType.arrayLiteralInitializerMember, mergeRanges(expr, comma));
    v.subType = ArrayLiteralInitializerMemberSubtype.spread;
    v.dotDotDot = dotDotDot;
    v.expr = expr;
    v.comma = comma;
    return v;
}

interface TryBase extends NodeBase {
    kind: NodeType.try,
    fromTag: boolean,
    body: Node[],
    catchBlocks: Catch[],
    finallyBlock: Finally | null
}

export type Try = Script.Try | Tag.Try;

export namespace Script {
    export interface Try extends TryBase {
        kind: NodeType.try,
        fromTag: false,
        tryToken: Terminal,
        leftBrace: Terminal,
        body: Node[],
        rightBrace: Terminal,
        catchBlocks: Script.Catch[],
        finallyBlock: Script.Finally | null
    }
    export function Try(
        tryToken: Terminal,
        leftBrace: Terminal,
        body: Node[],
        rightBrace: Terminal,
        catchBlocks: Script.Catch[],
        finallyBlock: Script.Finally | null
    ) : Try {
        const v = NodeBase<Try>(NodeType.try, mergeRanges(tryToken, finallyBlock));
        v.fromTag = false;
        v.tryToken = tryToken;
        v.leftBrace = leftBrace;
        v.body = body;
        v.rightBrace = rightBrace;
        v.catchBlocks = catchBlocks;
        v.finallyBlock = finallyBlock;
        return v;
    }
}

export namespace Tag {
    export interface Try extends TryBase {
        kind: NodeType.try,
        fromTag: true,
        body: Node[],
        catchBlocks: Tag.Catch[],
        finallyBlock: Tag.Finally | null
    }
    export function Try(startTag: CfTag.Common, body: Node[], catchBlocks: Tag.Catch[], finallyBlock: Tag.Finally | null, endTag: CfTag.Common) : Try {
        const v = NodeBase<Try>(NodeType.try, mergeRanges(startTag, endTag));
        v.fromTag = true;
        v.tagOrigin.startTag = startTag
        v.tagOrigin.endTag = endTag;
        v.body = body;
        v.catchBlocks = catchBlocks;
        v.finallyBlock = finallyBlock;
        return v;
    }
}

interface CatchBase extends NodeBase {
    kind: NodeType.catch,
    fromTag: boolean,
    body: Node[],
}

export type Catch = Script.Catch | Tag.Catch;

export namespace Script {
    export interface Catch extends CatchBase {
        kind: NodeType.catch
        fromTag: false,
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
        v.fromTag = false;
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
}

export namespace Tag {
    export interface Catch extends CatchBase {
        kind: NodeType.catch
        fromTag: true,
        body: Node[],
    }

    export function Catch(tag: CfTag.Common) : Catch;
    export function Catch(tag: CfTag.Common, body: Node[], endTag: CfTag.Common) : Catch;
    export function Catch(tag: CfTag.Common, body?: Node[], endTag?: CfTag.Common) {
        if (!body) {
            const v = NodeBase<Catch>(NodeType.catch, tag.range);
            v.fromTag = true;
            v.tagOrigin.startTag = tag;
            v.body = [];
            return v;
        }
        else {
            const v = NodeBase<Catch>(NodeType.catch, mergeRanges(tag, endTag));
            v.fromTag = true;
            v.tagOrigin.startTag = tag;
            v.tagOrigin.endTag = endTag!;
            v.body = body;
            return v;
        }
    }
}

interface FinallyBase extends NodeBase {
    kind: NodeType.finally,
    fromTag: boolean,
    body: Node[],
}

export type Finally = Script.Finally | Tag.Finally;

export namespace Script {
    export interface Finally extends FinallyBase {
        kind: NodeType.finally,
        fromTag: false,
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
        v.fromTag = false;
        v.finallyToken = finallyToken;
        v.leftBrace = leftBrace;
        v.body = body;
        v.rightBrace = rightBrace;
        return v;
    }
}

export namespace Tag {
    export interface Finally extends FinallyBase {
        kind: NodeType.finally,
        fromTag: true,
        body: Node[],
    }
    export function Finally(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) {
        const v = NodeBase<Finally>(NodeType.finally, mergeRanges(startTag, endTag));
        v.fromTag = true;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag = endTag;
        v.body = body;
        return v;
    }
}

export interface ImportStatement extends NodeBase {
    kind: NodeType.importStatement,
    importToken: Terminal,
    path: DottedPath<Terminal>,
    semicolon: Terminal | null,
}

export function ImportStatement(
    importToken: Terminal,
    path: DottedPath<Terminal>,
    semicolon: Terminal | null
) : ImportStatement {
    const v = NodeBase<ImportStatement>(NodeType.importStatement, mergeRanges(importToken, path, semicolon));
    v.importToken = importToken;
    v.path = path;
    v.semicolon = semicolon;
    return v;
}

export interface New extends NodeBase {
    kind: NodeType.new,
    newToken: Terminal,
    callExpr: CallExpression
}

export function New(
    newToken: Terminal,
    callExpr: CallExpression
) : New {
    const v = NodeBase<New>(NodeType.new, mergeRanges(newToken, callExpr));
    v.newToken = newToken;
    v.callExpr = callExpr;
    return v;
}
