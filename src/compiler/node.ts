import { Scanner, SourceRange, TokenType, Token, NilToken, TokenTypeUiString, CfFileType } from "./scanner";
import { exhaustiveCaseGuard, getAttributeValue, getTriviallyComputableBoolean, getTriviallyComputableString, Mutable } from "./utils";
import { Interface, Type } from "./types";

let debugNodeModule = false;
export function setDebug() { // can't unset after setting it, per program run
    if (debugNodeModule) {
        return;
    }
    debugNodeModule = true;
}

let nextNodeId : NodeId = 0;

export const enum NodeFlags {
    none         = 0,
    error        = 1 << 0,
    missing      = 1 << 1,
    checkerError = 1 << 2,
    docBlock     = 1 << 3,
    unreachable  = 1 << 4,
    checked      = 1 << 5,
}

export const enum NodeKind {
    sourceFile,
    terminal, textSpan, comment, hashWrappedExpr, parenthetical, tagAttribute,
    tag, callExpression, callArgument, unaryOperator, binaryOperator,
    conditional, variableDeclaration, statement, block, simpleStringLiteral, interpolatedStringLiteral, numericLiteral, booleanLiteral,
    identifier,
    indexedAccess, indexedAccessChainElement, sliceExpression,
    functionDefinition, arrowFunctionDefinition, functionParameter,
    dottedPath, dottedPathRest, switch, switchCase, do, while, ternary, for, structLiteral, arrayLiteral,
    structLiteralInitializerMember, arrayLiteralInitializerMember, try, catch, finally,
    breakStatement, continueStatement, returnStatement, importStatement,
    new, typeShim,
    property, paramStatement,
    staticAccess,
}

const NodeTypeUiString : Record<NodeKind, string> = {
    [NodeKind.sourceFile]: "program",
    [NodeKind.terminal]: "terminal",
    [NodeKind.textSpan]: "textSpan",
    [NodeKind.comment]: "comment",
    [NodeKind.hashWrappedExpr]: "hashWrappedExpr",
    [NodeKind.parenthetical]: "parenthetical",
    [NodeKind.tagAttribute]: "tagAttribute",
    [NodeKind.tag]: "tag",
    [NodeKind.callExpression]: "callExpression",
    [NodeKind.callArgument]: "callArgument",
    [NodeKind.unaryOperator]: "unaryOperator",
    [NodeKind.binaryOperator]: "binaryOperator",
    [NodeKind.conditional]: "conditional",
    [NodeKind.variableDeclaration]: "declaration",
    [NodeKind.statement]: "statement",
    [NodeKind.block]: "block",
    [NodeKind.simpleStringLiteral]: "simpleStringLiteral",
    [NodeKind.interpolatedStringLiteral]: "interpolatedStringLiteral",
    [NodeKind.numericLiteral]: "numericLiteral",
    [NodeKind.booleanLiteral]: "booleanLiteral",
    [NodeKind.identifier]: "identifier",
    [NodeKind.indexedAccess]: "indexedAccess",
    [NodeKind.indexedAccessChainElement]: "indexedAccessChainElement",
    [NodeKind.sliceExpression]: "sliceExpression",
    [NodeKind.functionDefinition]: "functionDefinition",
    [NodeKind.arrowFunctionDefinition]: "arrowFunctionDefinition",
    [NodeKind.functionParameter]: "functionParameter",
    [NodeKind.dottedPath]: "dottedPath",
    [NodeKind.dottedPathRest]: "dottedPathRest",
    [NodeKind.switch]: "switch",
    [NodeKind.switchCase]: "switchCase",
    [NodeKind.do]: "do",
    [NodeKind.while]: "while",
    [NodeKind.ternary]: "ternary",
    [NodeKind.for]: "for",
    [NodeKind.structLiteral]: "structLiteral",
    [NodeKind.arrayLiteral]: "arrayLiteral",
    [NodeKind.structLiteralInitializerMember]: "structLiteralInitializerMember",
    [NodeKind.arrayLiteralInitializerMember]: "arrayLiteralInitializerMember",
    [NodeKind.returnStatement]: "returnStatement",
    [NodeKind.try]: "try",
    [NodeKind.catch]: "catch",
    [NodeKind.finally]: "finally",
    [NodeKind.breakStatement]: "break",
    [NodeKind.continueStatement]: "continue",
    [NodeKind.importStatement]: "import",
    [NodeKind.new]: "new",
    [NodeKind.typeShim]: "typeshim",
    [NodeKind.property]: "property",
    [NodeKind.paramStatement]: "param",
    [NodeKind.staticAccess]: "staticAccess",
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
    | DottedPath // `"x"."x"` in a struct literal, `x.x` in a function parameter declaration, maybe others 
    | DottedPathRest
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
    | TypeShim // Node-based Type wrapper, would be nice to unify all types/nodes
    | Property
    | ParamStatement
    | StaticAccess

//
// wip:
// cf-declared-initial-type (really only relevant for function args)
// declaredInitialType is always any?
// inferredInitialType would be typeof assignment at declartion, if any 
// annotatedType is for first declaration
//
// "first lexical type" and "links.effectivelyDeclaredType" are painful to deal with, have to check 2 things for undefined at every use site
// do we NEED first lexical type? just have SymtabEntry.effectivelyDeclaredType, where
// // @! arg v : "A" | "B"
// function foo(string v) {}
// on binding, symtab entry gets effectively declared type of string; then is refined in checker to "A" | "B"
// during the refinement is when we'd make sure that the annotated type is a subtype of the original "effectively declared type"
//
export interface SymTabEntry {
    uiName: string,
    canonicalName: string,
    declarations: Node[] | null,
    firstLexicalType: Type | undefined,
    symbolId: SymbolId,
    links?: {
        effectiveDeclaredType?: Type,
        optional?: boolean,
    }
}

export type SymbolTable = Map<string, SymTabEntry>;

export function typeinfo() {
    return {
        interfaces: new Map<string, Interface[]>(),
        mergedInterfaces: new Map<string, Interface>(),
        // decorators: <Decorator[]>[], // 1/21/22 -- not supporting this
        aliases: new Map<string, Type>(),
    };
}

export type ScopeDisplay = {
    parentContainer: Node | null,
    typeinfo: ReturnType<typeof typeinfo>,
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
    "request",
    "server",
    "session",
    "this",
    "super",
    "thisTag",
    "thread",
    "threadLocal",
    "url",
    "variables",
    "__query", // magic scope inside <cfloop query=...> tags
    "__cfEngine", // things that the cf engine should provide (e.g. `encodeForHTML` or etc.)
    "__declaration", // for declaration files
    // @useless-transient "__transient", // for non-var-decl'd vars within functions that are not already defined outside of the function; we model them as visible during the function but they disappear on return to parent container
    "__property", // 'scope' of a (arbitrarily nested) property of a struct
] as const;

export type StaticallyKnownScopeName = (typeof staticallyKnownScopeName)[number];

export const isStaticallyKnownScopeName = (() => {
    const scopeNames = new Set<string>(staticallyKnownScopeName);
    return (name: string) : name is StaticallyKnownScopeName => scopeNames.has(name);
})();

export type NodeId = number;
export type TypeId = number;
export type FlowId = number;
export type SymbolId = number;

export type NodeWithScope<N extends Node = Node, T extends (StaticallyKnownScopeName | never) = never> = N & {containedScope: ScopeDisplay & {[k in T]: SymbolTable}};

export const enum FlowType {
    default,
    start,
    assignment,
    jumpTarget,
    switchCase,
    unreachable
}

export interface Flow {
    flowId: FlowId,
    flowType: FlowType,
    predecessors: Flow[],
    becameUnreachable: boolean,
    node: Node | null
}

export interface NodeBase {
    kind: NodeKind,
    nodeId: NodeId,
    parent: Node | null,
    range: SourceRange,

    typeAnnotation: TypeAnnotation | NonCompositeFunctionTypeAnnotation | null,

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

    if (debugNodeModule) {
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

export const enum DiagnosticKind { error, warning }
export interface Diagnostic {
    kind: DiagnosticKind,
    fromInclusive: number,
    toExclusive: number,
    msg: string,
    __debug_from_line?: number,
    __debug_from_col?: number,
    __debug_to_line?: number,
    __debug_to_col?: number,
}

export interface SymTabResolution {
    scopeName: StaticallyKnownScopeName,
    symTabEntry: SymTabEntry,
    alwaysVisibleEngineSymbol?: SymTabEntry,
}

export interface SymbolResolution extends SymTabResolution {
    container: Node
}

export interface NodeSourceMap {
    nodeId: number,
    range: SourceRange
}

export interface SourceFile extends NodeBase {
    kind: NodeKind.sourceFile,
    absPath: string,
    cfFileType: CfFileType,
    containedScope: ScopeDisplay,
    content: Node[],
    flatTree: NodeSourceMap[],
    libRefs: Map<string, SourceFile>,
    diagnostics: Diagnostic[],
    scanner: Scanner,
    nodeMap: Map<NodeId, Node>,
    cachedNodeTypes: Map<NodeId, Type>, // type of a particular node, exactly zero or one per node
    cachedFlowTypes: Map<FlowId, Map<SymbolId, Type>>, // types for symbols as determined at particular flow nodes, zero or more per flow node
    nodeToSymbol: Map<NodeId, SymbolResolution>,
    symbolIdToSymbol: Map<SymbolId, SymTabEntry>,
    endOfNodeFlowMap: Map<NodeId, Flow>,
    cfc: {
        extends: SourceFile | null,
        implements: SourceFile[]
    } | undefined,
}

export function resetSourceFileInPlace(target: SourceFile, newSource: string | Buffer) : void {
    target.containedScope = {parentContainer: null, typeinfo: typeinfo()};
    target.content = [];
    target.flatTree = [];
    // target.libRefs untouched
    target.diagnostics = [];
    target.scanner = Scanner(newSource);
    target.nodeMap = new Map();
    target.cachedNodeTypes = new Map();
    target.cachedFlowTypes = new Map();
    target.nodeToSymbol = new Map();
    target.symbolIdToSymbol = new Map();
    target.endOfNodeFlowMap = new Map<NodeId, Flow>();
    target.cfc = undefined;
}

export function SourceFile(absPath: string, cfFileType: CfFileType, sourceText: string | Buffer) : SourceFile {
    const sourceFile = NodeBase<SourceFile>(NodeKind.sourceFile);
    sourceFile.absPath = absPath;
    sourceFile.cfFileType = cfFileType;
    sourceFile.containedScope = {
        parentContainer: null,
        typeinfo: typeinfo(),
    };
    sourceFile.content = [];
    sourceFile.flatTree = [];
    sourceFile.libRefs = new Map();
    sourceFile.diagnostics = [];
    sourceFile.scanner = Scanner(sourceText);
    sourceFile.nodeMap = new Map<NodeId, Node>();
    sourceFile.cachedNodeTypes = new Map<NodeId, Type>();
    sourceFile.cachedFlowTypes = new Map<FlowId, Map<SymbolId, Type>>();
    sourceFile.nodeToSymbol = new Map<NodeId, SymbolResolution>();
    sourceFile.symbolIdToSymbol = new Map();
    sourceFile.endOfNodeFlowMap = new Map<NodeId, Flow>();
    return sourceFile;
}
export const DUMMY_CONTAINER = SourceFile("", CfFileType.cfc, "");

export const NilCfm = (text: string) => SourceFile("nil!", CfFileType.cfm, text);
export const NilCfc = (text: string) => SourceFile("nil!", CfFileType.cfc, text);
export const NilDCfm = (text: string) => SourceFile("nil!", CfFileType.dCfm, text);

export interface Terminal extends NodeBase {
    kind: NodeKind.terminal;
    rangeWithTrivia: SourceRange;
    token: Token;
    trivia: Node[];
    __debug_tokenType?: string;
}

export function Terminal(token: Token, trivia: Node[] = []) : Terminal {
    const v = NodeBase<Terminal>(NodeKind.terminal, token.range);
    v.rangeWithTrivia = mergeRanges(token.range, trivia);
    v.token = token;
    v.trivia = trivia;

    if (debugNodeModule) {
        v.__debug_tokenType = TokenTypeUiString[token.type];
    }

    return v;
}

export function NilTerminal(pos: number, text: string = "") { return Terminal(NilToken(pos, text)) };

export const freshFlow = (function() {
    let flowId = 0;
    return (predecessor: Flow | Flow[], flowType: FlowType, node: Node | null = null) : Flow => {
        const result = {
            flowId: flowId++,
            flowType: flowType,
            predecessors: Array.isArray(predecessor) ? predecessor : [predecessor],
            becameUnreachable: false,
            node
        }
        if (debugNodeModule) {
            Object.defineProperty(result, "__debugFlowInfo", {
                get() : string {
                    switch (flowType) {
                        case FlowType.assignment: return "assignment";
                        case FlowType.default: return "default";
                        case FlowType.jumpTarget: return "jumpTarget";
                        case FlowType.unreachable: return "unreachable";
                        case FlowType.switchCase: return "switchCase";
                        case FlowType.start: return "start";
                        default: exhaustiveCaseGuard(flowType);
                    }
                }
            })
        }
        return result;
    }
})();

export const UnreachableFlow = freshFlow([], FlowType.unreachable);

export const enum CommentType { tag, scriptSingleLine, scriptMultiLine };
export interface Comment extends NodeBase {
    kind: NodeKind.comment;
    commentType: CommentType;
    typedefs?: Typedef[],
}

export function Comment(tagOrigin: CfTag.Comment) : Comment;
export function Comment(commentType: CommentType, isDocBlock: boolean, range: SourceRange, typedefs?: Typedef[]) : Comment;
export function Comment(commentType: CfTag.Comment | CommentType, isDocBlock?: boolean, range?: SourceRange, typedefs?: Typedef[]) {
    if (typeof commentType === /*CommentType enum*/ "number") { // overload 2
        const comment = NodeBase<Comment>(NodeKind.comment, range);
        comment.commentType = commentType;
        if (isDocBlock) comment.flags |= NodeFlags.docBlock;
        if (typedefs) comment.typedefs = typedefs;
        return comment;
    }
    else { // overload 1
        const tagOrigin = commentType as CfTag.Comment;
        const comment = NodeBase<Comment>(NodeKind.comment, tagOrigin.range);
        comment.commentType = CommentType.tag;
        if (tagOrigin.typedefs) {
            comment.typedefs = tagOrigin.typedefs;
        }
        return comment;
    }
}

export interface TextSpan extends NodeBase {
    kind: NodeKind.textSpan;
    text: string;
}
export function TextSpan(sourceRange: SourceRange, text: string) : TextSpan {
    const textSpan = NodeBase<TextSpan>(NodeKind.textSpan, sourceRange);
    textSpan.text = text;
    return textSpan;
}

export interface HashWrappedExpr extends NodeBase {
    kind: NodeKind.hashWrappedExpr;
    leftHash: Terminal;
    expr: Node;
    rightHash: Terminal;
}
export function HashWrappedExpr(leftHash: Terminal, expr: Node, rightHash: Terminal) : HashWrappedExpr {
    const hashWrappedExpr = NodeBase<HashWrappedExpr>(NodeKind.hashWrappedExpr, mergeRanges(leftHash, rightHash));
    hashWrappedExpr.leftHash = leftHash;
    hashWrappedExpr.expr = expr;
    hashWrappedExpr.rightHash = rightHash;
    return hashWrappedExpr;
}

export interface Parenthetical extends NodeBase {
    kind: NodeKind.parenthetical;
    leftParen: Terminal;
    expr: Node;
    rightParen: Terminal;
}
export function Parenthetical(leftParen: Terminal, expr: Node, rightParen: Terminal) : Parenthetical {
    const parentWrappedExpr = NodeBase<Parenthetical>(NodeKind.parenthetical, mergeRanges(leftParen, rightParen));
    parentWrappedExpr.leftParen = leftParen;
    parentWrappedExpr.expr = expr;
    parentWrappedExpr.rightParen = rightParen;
    return parentWrappedExpr;
}

export interface TagAttribute extends NodeBase {
    kind: NodeKind.tagAttribute;
    name: Terminal;
    equals: Terminal | null;
    expr: Node | null;
    uiName: string;
    canonicalName: string;
}

export function TagAttribute(name: Terminal, uiName: string) : TagAttribute;
export function TagAttribute(name: Terminal, uiName: string, equals: Terminal, expr: Node) : TagAttribute;
export function TagAttribute(name: Terminal, uiName: string, equals?: Terminal, expr?: Node | undefined) : TagAttribute {
    let tagAttr : TagAttribute;
    if (name && equals && expr) {
        tagAttr = NodeBase<TagAttribute>(NodeKind.tagAttribute, mergeRanges(name, expr));
    }
    else {
        tagAttr = NodeBase<TagAttribute>(NodeKind.tagAttribute, name.range);
    }

    tagAttr.name = name;
    tagAttr.equals = equals ?? null;
    tagAttr.expr = expr ?? null;
    tagAttr.uiName = uiName;
    tagAttr.canonicalName = uiName.toLowerCase();
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
        kind: NodeKind.tag;
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

        const tag = NodeBase<TagBase>(NodeKind.tag, mergeRanges(tagStart, tagEnd));
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
        typedefs?: Typedef[],
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
    kind: NodeKind.callExpression;
    left: Node;
    leftParen: Terminal;
    args: CallArgument[];
    rightParen: Terminal;

}
export function CallExpression(left: Node, leftParen: Terminal, args: CallArgument[], rightParen: Terminal) {
    const v = NodeBase<CallExpression>(NodeKind.callExpression, mergeRanges(left, rightParen));
    v.left = left;
    v.leftParen = leftParen;
    v.args = args;
    v.rightParen = rightParen;
    return v;
}

export interface CallArgument extends NodeBase {
    kind: NodeKind.callArgument;
    name: SimpleStringLiteral | InterpolatedStringLiteral | Identifier | null;
    equals: Terminal | null;
    dotDotDot: Terminal | null;
    expr: Node;
    comma: Terminal | null;
}

export function CallArgument(name: SimpleStringLiteral | InterpolatedStringLiteral | Identifier | null, equals: Terminal | null, dotDotDot: Terminal | null, expr: Node, comma: Terminal | null) : CallArgument {
    const v = NodeBase<CallArgument>(NodeKind.callArgument, mergeRanges(name, expr, comma));
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
    kind: NodeKind.unaryOperator;
    pos: UnaryOperatorPos;
    optype: UnaryOpType;
    operator: Terminal;
    expr: Node;
}

export function UnaryOperator(expr: Node, op: Terminal) : UnaryOperator;
export function UnaryOperator(op: Terminal, expr: Node) : UnaryOperator;
export function UnaryOperator(lexicallyFirst: Node, lexicallyAfter: Node) {
    const v = NodeBase<UnaryOperator>(NodeKind.unaryOperator, mergeRanges(lexicallyFirst, lexicallyAfter));
    if (lexicallyFirst.kind === NodeKind.terminal) {
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
    add, sub, mul, div, quotient, mod, exp, cat, eq, neq, lt, lte, gt, gte, nullCoalesce,
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
    [BinaryOpType.quotient]:         "\\",
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
    kind: NodeKind.binaryOperator;
    optype: BinaryOpType;
    left: Node;
    operator: Terminal;
    right: Node;
    __debug_opType?: string;
}

export function BinaryOperator(left: Node, operator: Terminal, right: Node) : BinaryOperator {
    const v = NodeBase<BinaryOperator>(NodeKind.binaryOperator, mergeRanges(left, right));
    v.left = left;
    v.operator = operator;
    v.right = right;
    v.optype = tokenTypeToBinaryOpType(operator.token.type);

    if (debugNodeModule) {
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
        case TokenType.BACK_SLASH:            return BinaryOpType.quotient;
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
    kind: NodeKind.conditional,
    subType: ConditionalSubtype,
    fromTag: boolean,
    consequent: Node,
    alternative: ConditionalBase | null,
}

export type Conditional = Script.Conditional | Tag.Conditional;


export namespace Script {
    export interface Conditional extends ConditionalBase {
        kind: NodeKind.conditional,
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
        const v       = NodeBase<Conditional>(NodeKind.conditional, mergeRanges(ifToken, consequent));
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
        const v       = NodeBase<Conditional>(NodeKind.conditional, mergeRanges(ifToken, consequent));
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
        const v       = NodeBase<Conditional>(NodeKind.conditional, mergeRanges(elseToken, consequent));
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
        kind: NodeKind.conditional,
        fromTag: true,
        tagOrigin: {startTag: CfTag.ScriptLike | CfTag.Common, endTag: CfTag.Common | null},
        consequent: Node,
        alternative: Conditional | null,
    }

    export function If(startTag: CfTag.ScriptLike, consequent: Block, alternative: Conditional | null, endTag: CfTag.Common) : Conditional {
        const v = NodeBase<Conditional>(NodeKind.conditional, mergeRanges(startTag, endTag));
        v.fromTag            = true;
        v.subType            = ConditionalSubtype.if;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag   = endTag;
        v.consequent         = consequent;
        v.alternative        = alternative;
        return v;
    }

    export function ElseIf(startTag: CfTag.ScriptLike, block: Block, alternative: Conditional | null) : Conditional {
        const v = NodeBase<Conditional>(NodeKind.conditional, mergeRanges(startTag, block));
        v.fromTag            = true;
        v.subType            = ConditionalSubtype.elseif;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag   = null;
        v.consequent         = block;
        v.alternative        = alternative;
        return v;
    }

    export function Else(startTag: CfTag.Common, block: Block) : Conditional {
        const v = NodeBase<Conditional>(NodeKind.conditional, mergeRanges(startTag, block));
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
    kind: NodeKind.variableDeclaration,
    finalModifier: Terminal | null,
    varModifier: Terminal | null,
    expr: Node,
}

export function VariableDeclaration(
    finalModifier: Terminal | null,
    varModifier: Terminal | null,
    expr: Node
) : VariableDeclaration {
    const v = NodeBase<VariableDeclaration>(NodeKind.variableDeclaration, mergeRanges(finalModifier, varModifier, expr));
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
    kind: NodeKind.statement,
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
    const v = NodeBase<Statement>(NodeKind.statement, mergeRanges(name, attrs, semicolon));
    v.subType = StatementType.scriptSugaredTagCallStatement;
    v.expr = name;
    v.scriptSugaredTagStatement = {attrs};
    v.callStatement = null;
    v.semicolon = semicolon;

    if (debugNodeModule) {
        v.__debug_subtype = StatementTypeUiString[StatementType.scriptSugaredTagCallStatement];
    }

    return v;
}

// e.g, `cftransaction(action="foo");`
export function ScriptTagCallStatement(name: Terminal, leftParen: Terminal, args: CallArgument[], rightParen: Terminal, semicolon: Terminal | null) : Statement {
    const v = NodeBase<Statement>(NodeKind.statement, mergeRanges(name, rightParen, semicolon));
    v.subType = StatementType.scriptTagCallStatement;
    v.expr = name;
    v.scriptSugaredTagStatement = null;
    v.callStatement = {leftParen, args, rightParen};
    v.semicolon = semicolon;

    if (debugNodeModule) {
        v.__debug_subtype = StatementTypeUiString[StatementType.scriptTagCallStatement];
    }

    return v;
}

export function Statement(node: Node | null, semicolon: Terminal | null) : Statement {
    const v = NodeBase<Statement>(NodeKind.statement, mergeRanges(node, semicolon));
    v.subType = StatementType.expressionWrapper;
    v.expr = node;
    v.scriptSugaredTagStatement = null;
    v.callStatement = null;
    v.semicolon = semicolon;

    if (debugNodeModule) {
        v.__debug_subtype = StatementTypeUiString[StatementType.expressionWrapper];
    }

    return v;
}

export namespace FromTag {
    export function CfSetExpressionWrapper(tag: CfTag.ScriptLike) {
        let stmt : Statement;
        stmt = NodeBase<Statement>(NodeKind.statement, tag.range);
        stmt.subType = StatementType.expressionWrapper;
        stmt.expr = tag.expr;
        stmt.tagOrigin.startTag = tag;
        stmt.semicolon = null;

        if (debugNodeModule) {
            stmt.__debug_subtype = StatementTypeUiString[StatementType.expressionWrapper]
        }

        return stmt;
    }

    export function Statement(tag: CfTag.Common) : Statement {
        const stmt = NodeBase<Statement>(NodeKind.statement, tag.range);
        stmt.subType = StatementType.fromTag;
        stmt.expr = null;
        stmt.tagOrigin.startTag = tag;
        stmt.semicolon = null;

        if (debugNodeModule) {
            stmt.__debug_subtype = StatementTypeUiString[StatementType.fromTag]
        }

        return stmt;
    }
}

export interface ReturnStatement extends Omit<Statement, "kind"> {
    kind: NodeKind.returnStatement;
    returnToken: Terminal | null; // null if from tag
}
export function ReturnStatement(returnToken: Terminal, expr: Node | null, semicolon: Terminal | null) : ReturnStatement {
    const v = NodeBase<ReturnStatement>(NodeKind.returnStatement, mergeRanges(returnToken, expr, semicolon))
    v.returnToken = returnToken;
    v.expr = expr;
    v.semicolon;
    return v;
}
export namespace FromTag {
    export function ReturnStatement(tag: CfTag.ScriptLike) : ReturnStatement {
        const v = NodeBase<ReturnStatement>(NodeKind.returnStatement, tag.range);
        v.tagOrigin.startTag = tag;
        v.returnToken = null;
        v.expr = tag.expr;
        v.semicolon = null;
        return v;
    }
}

export interface BreakStatement extends NodeBase {
    kind: NodeKind.breakStatement,
    breakToken: Terminal | null,
    semicolon: Terminal | null
}
export function BreakStatement(
    breakToken: Terminal,
    semicolon: Terminal | null
) : BreakStatement {
    const v = NodeBase<BreakStatement>(NodeKind.breakStatement, mergeRanges(breakToken, semicolon));
    v.breakToken = breakToken;
    v.semicolon = semicolon;
    return v;
}
export namespace FromTag {
    export function BreakStatement(tag: CfTag.Common) : BreakStatement {
        const v = NodeBase<BreakStatement>(NodeKind.breakStatement, tag.range);
        v.tagOrigin.startTag = tag;
        v.breakToken = null;
        v.semicolon = null;
        return v;
    }
}

export interface ContinueStatement extends NodeBase {
    kind: NodeKind.continueStatement,
    continueToken: Terminal | null,
    semicolon: Terminal | null
}
export function ContinueStatement(
    continueToken: Terminal,
    semicolon: Terminal | null
) : ContinueStatement {
    const v = NodeBase<ContinueStatement>(NodeKind.continueStatement, mergeRanges(continueToken, semicolon));
    v.continueToken = continueToken;
    v.semicolon = semicolon;
    return v;
}
export namespace FromTag {
    export function ContinueStatement(tag: CfTag.Common) : ContinueStatement {
        const v = NodeBase<ContinueStatement>(NodeKind.continueStatement, tag.range);
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
    kind: NodeKind.block,
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
    const v                = NodeBase<Block>(NodeKind.block, mergeRanges(leftBrace, rightBrace));
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
        const v = NodeBase<Block>(NodeKind.block, mergeRanges(...stmtList));
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
            const v = NodeBase<Block>(NodeKind.block, mergeRanges(startTag, endTagOrStmtList));
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
            const v = NodeBase<Block>(NodeKind.block, mergeRanges(startTag, endTag));
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
    kind: NodeKind.simpleStringLiteral;
    leftQuote : Terminal;
    textSpan : TextSpan;
    rightQuote: Terminal;
}

export function SimpleStringLiteral(
    leftQuote: Terminal,
    textSpan: TextSpan,
    rightQuote: Terminal) : SimpleStringLiteral {
    const v = NodeBase<SimpleStringLiteral>(NodeKind.simpleStringLiteral, mergeRanges(leftQuote, rightQuote));
    v.leftQuote = leftQuote;
    v.textSpan = textSpan;
    v.rightQuote = rightQuote;
    return v;
}

export interface InterpolatedStringLiteral extends NodeBase {
    kind: NodeKind.interpolatedStringLiteral;
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

    const v = NodeBase<InterpolatedStringLiteral>(NodeKind.interpolatedStringLiteral, mergeRanges(leftQuote, rightQuote));
    v.delimiter = quoteType;
    v.leftQuote = leftQuote;
    v.elements = elements;
    v.rightQuote = rightQuote;
    return v;
}

export interface NumericLiteral extends NodeBase {
    kind: NodeKind.numericLiteral;
    literal: Terminal;
}

export function NumericLiteral(literal: Terminal) : NumericLiteral {
    const v = NodeBase<NumericLiteral>(NodeKind.numericLiteral, literal.range);
    v.literal = literal;
    return v;
}

export interface BooleanLiteral extends NodeBase {
    kind: NodeKind.booleanLiteral;
    literal: Terminal;
    booleanValue: boolean;
}

export function BooleanLiteral(literal: Terminal, value: boolean) {
    const v = NodeBase<BooleanLiteral>(NodeKind.booleanLiteral, literal.range);
    v.literal = literal;
    v.booleanValue = value;
    return v;
}

export interface Identifier extends NodeBase {
    kind: NodeKind.identifier;
    source: Terminal;
    /* might be nice to do the following, to know that if canonical is not undefined, then ui is not either
    name: {
        canonical: string,
        ui: string
    } | undefined
    */
    canonicalName: string | undefined; // undefined at least in the case of something like var '#foo#' = bar;
    uiName: string | undefined;
}

export function Identifier(identifier: Terminal, uiName: string | undefined) {
    const v = NodeBase<Identifier>(NodeKind.identifier, identifier.range);
    v.source = identifier;
    v.uiName = uiName;
    v.canonicalName = uiName?.toLowerCase();
    return v;
}

export interface StaticAccess extends NodeBase {
    kind: NodeKind.staticAccess,
    left: Identifier | DottedPath, // should be "qualified-id", the indexedAccess here should only be dot-access
    dblColon: Terminal,
    right: Node, // identifier or indexed-access-chain or call expr or ... probably not constrainable
    pathname: string
}

export function StaticAccess(left: Identifier | DottedPath, dblColon: Terminal, right: Node) {
    const v = NodeBase<StaticAccess>(NodeKind.staticAccess, mergeRanges(left, right));
    v.left = left;
    v.dblColon = dblColon;
    v.right = right;
    v.pathname = getTriviallyComputableString(left) ?? "<<PATHNAME-ERROR>>";
    return v;
}

export const enum IndexedAccessType { dot, bracket, optionalDot, optionalBracket, optionalCall };

export interface DotAccess extends NodeBase {
    kind: NodeKind.indexedAccessChainElement,
    accessType: IndexedAccessType.dot,
    dot: Terminal,
    property: Terminal,
    __debug_access_type?: string
}

export interface BracketAccess extends NodeBase {
    kind: NodeKind.indexedAccessChainElement,
    accessType: IndexedAccessType.bracket,
    leftBracket: Terminal,
    expr: Node,
    rightBracket: Terminal,
    __debug_access_type?: string
}

// note that for the optional accesses, like with the null coalescing operator `?:`
// it is not one token, but two separate tokens, possibly with comments between them
export interface OptionalDotAccess extends NodeBase {
    kind: NodeKind.indexedAccessChainElement,
    accessType: IndexedAccessType.optionalDot,
    questionMark: Terminal,
    dot: Terminal,
    property: Terminal,
    __debug_access_type?: string
}

export interface OptionalBracketAccess extends NodeBase {
    kind: NodeKind.indexedAccessChainElement,
    accessType: IndexedAccessType.optionalBracket,
    questionMark: Terminal,
    dot: Terminal,
    leftBracket: Terminal,
    expr: Node,
    rightBracket: Terminal,
    __debug_access_type?: string
}

export interface OptionalCall extends NodeBase {
    kind: NodeKind.indexedAccessChainElement,
    accessType: IndexedAccessType.optionalCall,
    questionMark: Terminal,
    dot: Terminal,
    __debug_access_type?: string
    // the call itself will "own" this node as its left-side
    // foo?.() is a call expression where the left side is an indexed-access expression with a trailing OptionalCall access chain element
}

export interface SliceExpression extends NodeBase {
    kind: NodeKind.sliceExpression,
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
    const v = NodeBase<SliceExpression>(NodeKind.sliceExpression, mergeRanges(from, colon1, to, colon2, stride));
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
    const node = NodeBase<DotAccess>(NodeKind.indexedAccessChainElement, mergeRanges(dot, property));
    node.accessType = IndexedAccessType.dot;
    node.dot = dot;
    node.property = property;
    if (debugNodeModule) (<IndexedAccessChainElement>node).__debug_access_type = "dot";
    return node;
}

export function BracketAccess(leftBracket: Terminal, expr: Node, rightBracket: Terminal) : BracketAccess {
    const node = NodeBase<BracketAccess>(NodeKind.indexedAccessChainElement, mergeRanges(leftBracket, rightBracket));
    node.accessType = IndexedAccessType.bracket;
    node.leftBracket = leftBracket;
    node.expr = expr;
    node.rightBracket = rightBracket;
    if (debugNodeModule) (<IndexedAccessChainElement>node).__debug_access_type = "bracket";
    return node;
}

export function OptionalDotAccess(questionMark: Terminal, dot: Terminal, property: Terminal) : OptionalDotAccess {
    const node = NodeBase<OptionalDotAccess>(NodeKind.indexedAccessChainElement, mergeRanges(questionMark, property));
    node.accessType = IndexedAccessType.optionalDot;
    node.questionMark = questionMark;
    node.dot = dot;
    node.property = property;
    if (debugNodeModule) (<IndexedAccessChainElement>node).__debug_access_type = "optional-dot";
    return node;
}

export function OptionalBracketAccess(questionMark: Terminal, dot: Terminal, leftBracket: Terminal, expr: Node, rightBracket: Terminal) : OptionalBracketAccess {
    const node = NodeBase<OptionalBracketAccess>(NodeKind.indexedAccessChainElement, mergeRanges(questionMark, rightBracket));
    node.accessType = IndexedAccessType.optionalBracket;
    node.questionMark = questionMark;
    node.dot = dot;
    node.leftBracket = leftBracket;
    node.expr = expr;
    node.rightBracket = rightBracket;
    if (debugNodeModule) (<IndexedAccessChainElement>node).__debug_access_type = "optional-bracket";
    return node;
}

export function OptionalCall(questionMark: Terminal, dot: Terminal) : OptionalCall {
    const node = NodeBase<OptionalCall>(NodeKind.indexedAccessChainElement, mergeRanges(questionMark, dot));
    node.accessType = IndexedAccessType.optionalCall;
    node.questionMark = questionMark;
    node.dot = dot;
    if (debugNodeModule) (<IndexedAccessChainElement>node).__debug_access_type = "optional-call";
    return node;
}

export interface IndexedAccess<RootNode = Node> extends NodeBase {
    kind: NodeKind.indexedAccess,
    root: RootNode,
    accessElements: IndexedAccessChainElement[],
}

export function IndexedAccess<T extends Node>(root: T) : IndexedAccess<T> {
    // make a copy of the source range, so that we can update the range of the full indexed access expression without mutating
    // the root's original range
    const v = NodeBase<IndexedAccess<T>>(NodeKind.indexedAccess, new SourceRange(root.range.fromInclusive, root.range.toExclusive));
    v.root = root;
    v.accessElements = [];
    return v;
}

export function pushAccessElement(base: IndexedAccess, element: IndexedAccessChainElement) : void {
    base.accessElements.push(element);
    base.range.toExclusive = element.range.toExclusive;
}

interface FunctionParameterBase extends NodeBase {
    kind: NodeKind.functionParameter,
    required: boolean,
    defaultValue: Node | null,
    fromTag: boolean,
    canonicalName: string,
    uiName: string,
    attrs: TagAttribute[],
    type: Type | null
    javaLikeTypename: string | DottedPath | null // fixme: can this be just `string | null`
}

export type FunctionParameter = Script.FunctionParameter | Tag.FunctionParameter;

export namespace Script {
    export interface FunctionParameter extends FunctionParameterBase {
        kind: NodeKind.functionParameter,
        fromTag: false,
        requiredTerminal: Terminal | null,
        javaLikeTypename: DottedPath | null,
        dotDotDot: Terminal | null,
        identifier: Identifier,
        equals: Terminal | null,
        comma: Terminal | null,
    }

    export function FunctionParameter(
        requiredTerminal : Terminal | null,
        javaLikeTypename: DottedPath | null,
        dotDotDot: Terminal | null,
        identifier: Identifier,
        equals: Terminal | null,
        defaultValue: Node | null,
        attrs: TagAttribute[],
        comma: Terminal | null,
        type: Type | null) : FunctionParameter {
        const v = NodeBase<FunctionParameter>(NodeKind.functionParameter, mergeRanges(requiredTerminal, javaLikeTypename, identifier, defaultValue, comma));
        v.fromTag = false;
        v.requiredTerminal = requiredTerminal;
        v.javaLikeTypename = javaLikeTypename;
        v.dotDotDot = dotDotDot;
        v.identifier = identifier;
        v.equals = equals;
        v.comma = comma;
        v.canonicalName = identifier.canonicalName || "<<ERROR>>";
        v.uiName = identifier.uiName || "<<ERROR>>";
        v.type = type;
        v.attrs = attrs;
        // it is legal to say something is required and give it a default; however, a required parameter with a default is not really required
        v.defaultValue = defaultValue;
        v.required = !!(requiredTerminal) && !defaultValue;
        return v;
    }
}

function defineNodeGetter<TNode extends Node, TKey extends keyof TNode>(node: TNode, k: TKey, get: (this: TNode) => TNode[TKey]) {
    Object.defineProperty(node, k, { get });
}

export namespace Tag {
    export interface FunctionParameter extends FunctionParameterBase {
        kind: NodeKind.functionParameter,
        fromTag: true,        
    }

    export function FunctionParameter(tag: CfTag.Common) : FunctionParameter {
        const name = getTriviallyComputableString(getAttributeValue(tag.attrs, "name"));

        const v = NodeBase<FunctionParameter>(NodeKind.functionParameter, tag.range);
        v.fromTag = true;
        v.tagOrigin.startTag = tag;
        v.canonicalName = name?.toLowerCase() || "<<ERROR>>";
        v.uiName = name || "<<ERROR>>";
        v.attrs = tag.attrs;
        // it is legal to say something is required and give it a default; however, a required parameter with a default is not really required
        v.defaultValue = getAttributeValue(tag.attrs, "default") ?? null;
        v.required = !v.defaultValue && (getTriviallyComputableBoolean(getAttributeValue(tag.attrs, "required")) ?? false);
        v.type = null;

        defineNodeGetter(v, "javaLikeTypename", function() {
            return getTriviallyComputableString(getAttributeValue(this.attrs, "type")) ?? null;
        });

        return v;
    }
}

interface FunctionDefinitionBase extends NodeBase {
    kind: NodeKind.functionDefinition,
    fromTag: boolean,
    name: null | {
        canonical: string,
        ui: string
    },
    attrs          : TagAttribute[],
    finalFlow      : Flow,
}

export type FunctionDefinition = Script.FunctionDefinition | Tag.FunctionDefinition;
export type NamedFunctionDefinition = FunctionDefinition & {[k in keyof Pick<FunctionDefinitionBase, "name">]: Exclude<FunctionDefinitionBase[k], null>};

export namespace Script {
    export interface FunctionDefinition extends FunctionDefinitionBase {
        kind: NodeKind.functionDefinition;
        fromTag        : false,
        accessModifier : Terminal | null,
        returnType     : DottedPath | null,
        functionToken  : Terminal,
        nameToken      : Identifier | null, // fixme: need to know if this null because of an error or because it's an anonymous function
        leftParen      : Terminal,
        params         : FunctionParameter[],
        rightParen     : Terminal,
        body           : Block,
        returnTypeAnnotation : Type | null, // fixme: this will be either in the full node type annotation, or as an attribute
    }

    export function FunctionDefinition(
        accessModifier: Terminal | null,
        returnType    : DottedPath | null,
        functionToken : Terminal,
        nameToken     : Identifier | null,
        leftParen     : Terminal,
        params        : FunctionParameter[],
        rightParen    : Terminal,
        attrs         : TagAttribute[],
        body          : Block,
        returnTypeAnnotation : Type | null
    ) : FunctionDefinition {
        const v = NodeBase<FunctionDefinition>(NodeKind.functionDefinition, mergeRanges(accessModifier, returnType, functionToken, body));
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

        if (nameToken?.canonicalName && nameToken.uiName) {
            v.name = {
                canonical: nameToken.canonicalName,
                ui: nameToken.uiName,
            }
        }
        else {
            v.name = null;
        }

        return v;
    }
}

export namespace Tag {
    export interface FunctionDefinition extends FunctionDefinitionBase {
        kind: NodeKind.functionDefinition;
        fromTag        : true,
        params         : FunctionParameter[],
        body           : Node[], // fixme: block?
    }

    export function FunctionDefinition(startTag: CfTag.Common, params: FunctionParameter[], body: Node[], endTag: CfTag.Common) : FunctionDefinition {
        const name = getTriviallyComputableString(getAttributeValue(startTag.attrs, "name")) ?? null;
        const v = NodeBase<FunctionDefinition>(NodeKind.functionDefinition, mergeRanges(startTag, endTag));
        v.fromTag            = true;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag   = endTag;
        v.params             = params;
        v.body               = body;
        
        if (name) {
            v.name = {
                canonical: name.toLowerCase(),
                ui: name,
            }
        }
        else {
            v.name = null;
        }

        v.attrs              = startTag.attrs;
        return v;
    }
}

export interface ArrowFunctionDefinition extends NodeBase {
    kind: NodeKind.arrowFunctionDefinition;
    parens: {left: Terminal, right: Terminal} | null,
    params: Script.FunctionParameter[]; // not possible to have a tag based arrow function def
    fatArrow: Terminal,
    body: Node,
    finalFlow: Flow,
}

export function ArrowFunctionDefinition(leftParen: Terminal | null, params: Script.FunctionParameter[], rightParen: Terminal | null, fatArrow: Terminal, body: Node) : ArrowFunctionDefinition {
    const v = NodeBase<ArrowFunctionDefinition>(NodeKind.arrowFunctionDefinition);
    v.range = mergeRanges(leftParen, body);
    v.parens = (leftParen && rightParen) ? {left: leftParen, right: rightParen} : null,
    v.params = params;
    v.fatArrow = fatArrow;
    v.body = body;
    return v;
};

export interface DottedPathRest extends NodeBase {
    kind: NodeKind.dottedPathRest,
    dot: Terminal,
    key: Terminal,
}

export interface DottedPath extends NodeBase {
    kind: NodeKind.dottedPath;
    headKey: Terminal;
    readonly rest: ReadonlyArray<DottedPathRest>,
    readonly length: number,
}

export function DottedPath(headKey: Terminal) : DottedPath {
    const v = NodeBase<Mutable<DottedPath>>(NodeKind.dottedPath, headKey.range);
    v.headKey = headKey;
    v.rest = [];
    v.length = 1;
    return v;
}

export function DottedPathRest(dot: Terminal, key: Terminal) {
    const v = NodeBase<DottedPathRest>(NodeKind.dottedPathRest, mergeRanges(dot, key));
    v.dot = dot;
    v.key = key;
    return v;
}

export function pushDottedPathElement(dottedPath: DottedPath, dot: Terminal, key: Terminal) {
    (dottedPath.rest as DottedPathRest[]).push(DottedPathRest(dot, key));
    dottedPath.range = mergeRanges(dottedPath, dot, key);
    (dottedPath as Mutable<DottedPath>).length += 1;
}

interface SwitchBase extends NodeBase {
    kind: NodeKind.switch;
    fromTag: boolean,
    cases: SwitchCaseBase[],
}

export type Switch = Script.Switch | Tag.Switch;

export namespace Script {
    export interface Switch extends SwitchBase {
        kind: NodeKind.switch;
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
        const v = NodeBase<Switch>(NodeKind.switch, mergeRanges(switchToken, rightBrace));
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
        kind: NodeKind.switch,
        fromTag: true,
        cases: Tag.SwitchCase[],
    }
    export function Switch(startTag: CfTag.Common, cases: Tag.SwitchCase[], endTag: CfTag.Common) : Tag.Switch{
        const v = NodeBase<Switch>(NodeKind.switch, mergeRanges(startTag, endTag));
        v.fromTag = true;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag = endTag;
        v.cases = cases;
        return v;
    }
}

interface SwitchCaseBase extends NodeBase {
    kind: NodeKind.switchCase;
    fromTag: boolean,
    caseType: SwitchCaseType;
    body: Node[];
}

export type SwitchCase = Script.SwitchCase | Tag.SwitchCase;

export const enum SwitchCaseType { case, default };

export namespace Script {
    export interface SwitchCase extends SwitchCaseBase {
        kind: NodeKind.switchCase,
        fromTag: false,
        caseType: SwitchCaseType,
        caseOrDefaultToken: Terminal,
        expr: Node | null,
        colon: Terminal,
        body: Node[],
    }

    export function SwitchCase(caseToken: Terminal, expr: Node, colon: Terminal, body: Node[]) : SwitchCase {
        const v = NodeBase<SwitchCase>(NodeKind.switchCase, mergeRanges(caseToken, body));
        v.fromTag = false;
        v.caseType = SwitchCaseType.case
        v.caseOrDefaultToken = caseToken;
        v.expr = expr;
        v.colon = colon;
        v.body = body;
        return v;
    }
    export function SwitchDefault(defaultToken: Terminal, colon: Terminal, body: Node[]) : SwitchCase {
        const v = NodeBase<SwitchCase>(NodeKind.switchCase, mergeRanges(defaultToken, body));
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
        kind: NodeKind.switchCase,
        fromTag: true,
        caseType: SwitchCaseType,
        body: Node[],
    }

    export function SwitchCase(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) : SwitchCase {
        const v = NodeBase<SwitchCase>(NodeKind.switchCase, mergeRanges(startTag, endTag));
        v.fromTag = true;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag = endTag;
        v.caseType = SwitchCaseType.case
        v.body = body;
        return v;
    }
    export function SwitchDefault(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) : SwitchCase {
        const v = NodeBase<SwitchCase>(NodeKind.switchCase, mergeRanges(startTag, endTag));
        v.fromTag = true;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag = endTag;
        v.caseType = SwitchCaseType.default;
        v.body = body;
        return v;
    }
}

export interface Do extends NodeBase {
    kind: NodeKind.do;
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
    const v = NodeBase<Do>(NodeKind.do, mergeRanges(doToken, rightParen));
    v.doToken    = doToken;
    v.body       = body;
    v.whileToken = whileToken;
    v.leftParen  = leftParen;
    v.expr       = expr;
    v.rightParen = rightParen;
    return v;
}

export interface While extends NodeBase {
    kind: NodeKind.while;
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
    const v = NodeBase<While>(NodeKind.while, mergeRanges(whileToken, body));
    v.whileToken = whileToken;
    v.leftParen  = leftParen;
    v.expr       = expr;
    v.rightParen = rightParen;
    v.body       = body;
    return v;
}

export interface Ternary extends NodeBase {
    kind: NodeKind.ternary;
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
    const v = NodeBase<Ternary>(NodeKind.ternary, mergeRanges(expr, ifFalse));
    v.expr = expr;
    v.questionMark = questionMark;
    v.ifTrue = ifTrue;
    v.colon = colon;
    v.ifFalse = ifFalse;
    return v;
}

export const enum ForSubType { for, forIn}
export type For = ForExpr | ForIn;

interface ForBase extends NodeBase {
    kind: NodeKind.for;
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
        const v = NodeBase<ForExpr>(NodeKind.for, mergeRanges(forToken, body));
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
        const v = NodeBase<ForIn>(NodeKind.for, mergeRanges(forToken, body));
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
    kind: NodeKind.structLiteral,
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
    const v = NodeBase<StructLiteral>(NodeKind.structLiteral, mergeRanges(leftDelimiter, rightDelimiter));
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
    const v = NodeBase<StructLiteral>(NodeKind.structLiteral, mergeRanges(leftBracket, rightBracket));
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
    const v = NodeBase<StructLiteral>(NodeKind.structLiteral, mergeRanges(leftBracket, rightBracket));
    v.ordered = true;
    v.leftDelimiter = leftBracket;
    v.members = [];
    v.emptyOrderedStructColon = colon;
    v.rightDelimiter = rightBracket;
    return v;
}

export type StructLiteralInitializerMember = 
    | KeyedStructLiteralInitializerMember
    | ShorthandStructLiteralInitializerMember
    | SpreadStructLiteralInitializerMember;

export const enum StructLiteralInitializerMemberSubtype { keyed, spread };

interface StructLiteralInitializerMemberBase extends NodeBase {
    readonly kind: NodeKind.structLiteralInitializerMember;
    readonly subType: StructLiteralInitializerMemberSubtype,
    readonly comma: Terminal | null
}

interface KeyedStructLiteralInitializerMemberBase extends StructLiteralInitializerMemberBase {
    readonly kind: NodeKind.structLiteralInitializerMember;
    readonly subType: StructLiteralInitializerMemberSubtype.keyed,
    readonly shorthand: boolean,
    readonly key: Node,
}

export interface KeyedStructLiteralInitializerMember extends KeyedStructLiteralInitializerMemberBase {
    readonly kind: NodeKind.structLiteralInitializerMember;
    readonly subType: StructLiteralInitializerMemberSubtype.keyed,
    readonly shorthand: false,
    readonly colon: Terminal,
    readonly expr: Node,
}

export interface ShorthandStructLiteralInitializerMember extends KeyedStructLiteralInitializerMemberBase {
    readonly shorthand: true,
}

export function KeyedStructLiteralInitializerMember(
    key: Node,
    colon: Terminal,
    expr: Node,
    comma: Terminal | null) : KeyedStructLiteralInitializerMember {
    const v = NodeBase<Mutable<KeyedStructLiteralInitializerMember>>(NodeKind.structLiteralInitializerMember, mergeRanges(key, expr, comma));
    v.subType = StructLiteralInitializerMemberSubtype.keyed;
    v.shorthand = false;
    v.key = key;
    v.comma = comma;
    v.colon = colon;
    v.expr = expr;
    return v;
}

export function ShorthandStructLiteralInitializerMember(
    key: Node,
    comma: Terminal | null) : ShorthandStructLiteralInitializerMember {
    const v = NodeBase<Mutable<ShorthandStructLiteralInitializerMember>>(NodeKind.structLiteralInitializerMember, mergeRanges(key, comma));
    v.subType = StructLiteralInitializerMemberSubtype.keyed;
    v.shorthand = true;
    v.key = key;
    v.comma = comma;
    return v;
}

export interface SpreadStructLiteralInitializerMember extends StructLiteralInitializerMemberBase {
    kind: NodeKind.structLiteralInitializerMember;
    subType: StructLiteralInitializerMemberSubtype.spread,
    dotDotDot: Terminal,
    expr: Node,
}

export function SpreadStructLiteralInitializerMember(
    dotDotDot: Terminal,
    expr: Node,
    comma: Terminal | null) : SpreadStructLiteralInitializerMember {
    const v = NodeBase<Mutable<SpreadStructLiteralInitializerMember>>(NodeKind.structLiteralInitializerMember, mergeRanges(dotDotDot, expr, comma));
    v.subType = StructLiteralInitializerMemberSubtype.spread;
    v.dotDotDot = dotDotDot;
    v.expr = expr;
    v.comma = comma;
    return v;
}

export interface ArrayLiteral extends NodeBase {
    kind: NodeKind.arrayLiteral,
    leftBracket: Terminal;
    members: ArrayLiteralInitializerMember[];
    rightBracket: Terminal;
}

export function ArrayLiteral(
    leftBracket: Terminal,
    members: ArrayLiteralInitializerMember[],
    rightBracket: Terminal
) : ArrayLiteral {
    const v = NodeBase<ArrayLiteral>(NodeKind.arrayLiteral, mergeRanges(leftBracket, rightBracket));
    v.leftBracket = leftBracket
    v.members = members;
    v.rightBracket = rightBracket;
    return v;
}

export type ArrayLiteralInitializerMember = SimpleArrayLiteralInitializerMember | SpreadArrayLiteralInitializerMember;

export const enum ArrayLiteralInitializerMemberSubtype { simple, spread }

export interface SimpleArrayLiteralInitializerMember extends NodeBase {
    kind: NodeKind.arrayLiteralInitializerMember,
    subType: ArrayLiteralInitializerMemberSubtype.simple,
    expr: Node,
    comma: Terminal | null
}

export function SimpleArrayLiteralInitializerMember(
    expr: Node,
    comma: Terminal | null
) : SimpleArrayLiteralInitializerMember {
    const v = NodeBase<SimpleArrayLiteralInitializerMember>(NodeKind.arrayLiteralInitializerMember, mergeRanges(expr, comma));
    v.subType = ArrayLiteralInitializerMemberSubtype.simple;
    v.expr = expr;
    v.comma = comma;
    return v;
}

export interface SpreadArrayLiteralInitializerMember extends NodeBase {
    kind: NodeKind.arrayLiteralInitializerMember,
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
    const v = NodeBase<SpreadArrayLiteralInitializerMember>(NodeKind.arrayLiteralInitializerMember, mergeRanges(expr, comma));
    v.subType = ArrayLiteralInitializerMemberSubtype.spread;
    v.dotDotDot = dotDotDot;
    v.expr = expr;
    v.comma = comma;
    return v;
}

interface TryBase extends NodeBase {
    kind: NodeKind.try,
    fromTag: boolean,
    body: Node[],
    catchBlocks: Catch[],
    finallyBlock: Finally | null
}

export type Try = Script.Try | Tag.Try;

export namespace Script {
    export interface Try extends TryBase {
        kind: NodeKind.try,
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
        const v = NodeBase<Try>(NodeKind.try, mergeRanges(tryToken, finallyBlock));
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
        kind: NodeKind.try,
        fromTag: true,
        body: Node[],
        catchBlocks: Tag.Catch[],
        finallyBlock: Tag.Finally | null
    }
    export function Try(startTag: CfTag.Common, body: Node[], catchBlocks: Tag.Catch[], finallyBlock: Tag.Finally | null, endTag: CfTag.Common) : Try {
        const v = NodeBase<Try>(NodeKind.try, mergeRanges(startTag, endTag));
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
    kind: NodeKind.catch,
    fromTag: boolean,
    body: Node[],
}

export type Catch = Script.Catch | Tag.Catch;

export namespace Script {
    export interface Catch extends CatchBase {
        kind: NodeKind.catch
        fromTag: false,
        catchToken: Terminal,
        leftParen: Terminal,
        exceptionType: SimpleStringLiteral | InterpolatedStringLiteral | DottedPath,
        exceptionBinding: Identifier,
        rightParen: Terminal,
        leftBrace: Terminal,
        body: Node[],
        rightBrace: Terminal
    }

    export function Catch(
        catchToken: Terminal,
        leftParen: Terminal,
        exceptionType: SimpleStringLiteral | InterpolatedStringLiteral | DottedPath,
        exceptionBinding: Identifier,
        rightParen: Terminal,
        leftBrace: Terminal,
        body: Node[],
        rightBrace: Terminal
    ) : Catch {
        const v = NodeBase<Catch>(NodeKind.catch, mergeRanges(leftParen, rightBrace));
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
        kind: NodeKind.catch
        fromTag: true,
        body: Node[],
    }

    export function Catch(tag: CfTag.Common) : Catch;
    export function Catch(tag: CfTag.Common, body: Node[], endTag: CfTag.Common) : Catch;
    export function Catch(tag: CfTag.Common, body?: Node[], endTag?: CfTag.Common) {
        if (!body) {
            const v = NodeBase<Catch>(NodeKind.catch, tag.range);
            v.fromTag = true;
            v.tagOrigin.startTag = tag;
            v.body = [];
            return v;
        }
        else {
            const v = NodeBase<Catch>(NodeKind.catch, mergeRanges(tag, endTag));
            v.fromTag = true;
            v.tagOrigin.startTag = tag;
            v.tagOrigin.endTag = endTag!;
            v.body = body;
            return v;
        }
    }
}

interface FinallyBase extends NodeBase {
    kind: NodeKind.finally,
    fromTag: boolean,
    body: Node[],
}

export type Finally = Script.Finally | Tag.Finally;

export namespace Script {
    export interface Finally extends FinallyBase {
        kind: NodeKind.finally,
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
        const v = NodeBase<Finally>(NodeKind.finally, mergeRanges(finallyToken, rightBrace));
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
        kind: NodeKind.finally,
        fromTag: true,
        body: Node[],
    }
    export function Finally(startTag: CfTag.Common, body: Node[], endTag: CfTag.Common) {
        const v = NodeBase<Finally>(NodeKind.finally, mergeRanges(startTag, endTag));
        v.fromTag = true;
        v.tagOrigin.startTag = startTag;
        v.tagOrigin.endTag = endTag;
        v.body = body;
        return v;
    }
}

export interface ImportStatement extends NodeBase {
    kind: NodeKind.importStatement,
    importToken: Terminal,
    path: DottedPath,
    semicolon: Terminal | null,
}

export function ImportStatement(
    importToken: Terminal,
    path: DottedPath,
    semicolon: Terminal | null
) : ImportStatement {
    const v = NodeBase<ImportStatement>(NodeKind.importStatement, mergeRanges(importToken, path, semicolon));
    v.importToken = importToken;
    v.path = path;
    v.semicolon = semicolon;
    return v;
}

export interface New extends NodeBase {
    kind: NodeKind.new,
    newToken: Terminal,
    callExpr: CallExpression
}

export function New(
    newToken: Terminal,
    callExpr: CallExpression
) : New {
    const v = NodeBase<New>(NodeKind.new, mergeRanges(newToken, callExpr));
    v.newToken = newToken;
    v.callExpr = callExpr;
    return v;
}

export const enum TypeShimKind {
    typedef,                           // `@!typedef Foo = sometype`, declaring the typeid "Foo" to be bound to "sometype"
    interfacedef,                      // `@!interface XYZ {}`, basically a typedef but different enough to need a separate treatement
    annotation,                        // `@!type sometype`, annotates the subsequent statement with a type
    namedAnnotation,             // `@!arg:<argname> <type>` annotates a single function argument
    nonCompositeFunctionTypeAnnotation // we combine any loose functionArgAnnotations into a type shim of this kind
    /*, decorator */ // unused
};

export type TypeShim = Typedef | Interfacedef | TypeAnnotation | NamedAnnotation ;
interface TypeShimBase extends NodeBase {
    kind: NodeKind.typeShim,
    shimKind: TypeShimKind,
    type: Type,
}

export interface Typedef extends TypeShimBase {
    shimKind: TypeShimKind.typedef,
    name: string,
}

export interface Interfacedef extends TypeShimBase {
    shimKind: TypeShimKind.interfacedef,
    type: Interface,
}
export interface TypeAnnotation extends TypeShimBase {
    shimKind: TypeShimKind.annotation,
}

export interface NonCompositeFunctionTypeAnnotation extends TypeShimBase {
    shimKind: TypeShimKind.nonCompositeFunctionTypeAnnotation,
    params: NamedAnnotation[],
    returns: NamedAnnotation | null
}

export interface NamedAnnotation extends TypeShimBase {
    shimKind: TypeShimKind.namedAnnotation,
    name: Token,
}

export function Typedef(type: Type, name: string) : Typedef {
    const v = NodeBase<TypeShimBase>(NodeKind.typeShim, SourceRange.Nil()) as Typedef;
    v.shimKind = TypeShimKind.typedef;
    v.type = type;
    v.name = name;
    return v;
}

export function Interfacedef(type: Interface) : Interfacedef {
    const v = NodeBase<TypeShimBase>(NodeKind.typeShim, SourceRange.Nil()) as Interfacedef;
    v.shimKind = TypeShimKind.interfacedef;
    v.type = type;
    return v;
}

export function TypeAnnotation(type: Type) : TypeAnnotation {
    const v = NodeBase<TypeShimBase>(NodeKind.typeShim, SourceRange.Nil()) as TypeAnnotation;
    v.shimKind = TypeShimKind.annotation;
    v.type = type;
    return v;
}

export function NamedAnnotation(name: Token, type: Type) : NamedAnnotation {
    const v = NodeBase<TypeShimBase>(NodeKind.typeShim, SourceRange.Nil()) as NamedAnnotation;
    v.shimKind = TypeShimKind.namedAnnotation;
    v.name = name;
    v.type = type;
    return v;
}

export function NonCompositeFunctionTypeAnnotation(params: NamedAnnotation[], returns: NamedAnnotation | null) : NonCompositeFunctionTypeAnnotation {
    const v = NodeBase<TypeShimBase>(NodeKind.typeShim, SourceRange.Nil()) as NonCompositeFunctionTypeAnnotation;
    v.shimKind = TypeShimKind.nonCompositeFunctionTypeAnnotation;
    v.params = params;
    v.returns = returns;
    return v;
}

interface PropertyBase extends NodeBase {
    kind: NodeKind.property,
    fromTag: boolean,
    attrs: TagAttribute[],
}

export type Property = Tag.Property | Script.Property;

export namespace Tag {
    export interface Property extends PropertyBase {
        fromTag: true,
        tagOrigin: {
            startTag: CfTag.Common,
            endTag: null
        }
    }

    export function Property(tag: CfTag.Common) : Property {
        const v = NodeBase<Property>(NodeKind.property, tag.range);
        v.fromTag = true;
        v.tagOrigin = {
            startTag: tag,
            endTag: null
        }
        v.attrs = tag.attrs;
        return v;
    }
}

export namespace Script {
    export interface Property extends PropertyBase {
        fromTag: false,
        propertyTerminal: Terminal,
        impliedTypeAttr: boolean,
        impliedNameAttr: boolean
    }

    export function Property(terminal: Terminal, attrs: TagAttribute[], impliedTypeAttr: boolean, impliedNameAttr: boolean) : Property {
        const v = NodeBase<Property>(NodeKind.property, mergeRanges(terminal, attrs));
        v.fromTag = false;
        v.propertyTerminal = terminal;
        v.attrs = attrs;
        v.impliedTypeAttr = impliedTypeAttr;
        v.impliedNameAttr = impliedNameAttr;
        return v;
    }
}

export const enum ParamStatementSubType { withImplicitTypeAndName, withImplicitName, default };

interface ParamStatementBase extends NodeBase {
    kind: NodeKind.paramStatement,
    subType: ParamStatementSubType,
    paramToken: Terminal,
    implicitType?: DottedPath,
    implicitName?: DottedPath,
    implicitNameEquals?: Terminal | null,
    /**
     * the "default" expression, but without an explicit default attribute binding, like:
     * `param foo = 42`
     *              ^^--- implicit default expr
     */
    implicitDefaultExpr?: Node | null
    attrs: TagAttribute[]
}

export interface ParamStatementDefault extends ParamStatementBase {
    subType: ParamStatementSubType.default,
}

export interface ParamStatementWithImplicitTypeAndName extends ParamStatementBase {
    subType: ParamStatementSubType.withImplicitTypeAndName,
    implicitType: DottedPath,
    implicitName: DottedPath,
    implicitNameEquals: Terminal | null,
    implicitDefaultExpr: Node | null
}

export interface ParamStatementWithImplicitName extends ParamStatementBase {
    subType: ParamStatementSubType.withImplicitName,
    implicitName: DottedPath,
    implicitNameEquals: Terminal | null,
    implicitDefaultExpr: Node | null
}

export type ParamStatement = ParamStatementDefault | ParamStatementWithImplicitName | ParamStatementWithImplicitTypeAndName;

export function ParamStatementWithImplicitTypeAndName(paramToken: Terminal,
                                                      type: DottedPath,
                                                      name: DottedPath,
                                                      equals: Terminal | null,
                                                      expr: Node | null,
                                                      attrs: TagAttribute[]) {
    const v = NodeBase<ParamStatementBase>(NodeKind.paramStatement, mergeRanges(paramToken, type, name, equals, expr, attrs)) as ParamStatementWithImplicitTypeAndName;
    v.subType = ParamStatementSubType.withImplicitTypeAndName;
    v.paramToken = paramToken;
    v.implicitType = type;
    v.implicitName = name;
    v.implicitNameEquals = equals;
    v.implicitDefaultExpr = expr;
    v.attrs = attrs;
    return v;
}

export function ParamStatementWithImplicitName(paramToken: Terminal,
                                               name: DottedPath,
                                               equals: Terminal | null,
                                               expr: Node | null,
                                               attrs: TagAttribute[]) {
    const v = NodeBase<ParamStatementBase>(NodeKind.paramStatement, mergeRanges(paramToken, name, equals, expr, attrs)) as ParamStatementWithImplicitName;
    v.subType = ParamStatementSubType.withImplicitName;
    v.paramToken = paramToken;
    v.implicitName = name;
    v.implicitNameEquals = equals;
    v.implicitDefaultExpr = expr;
    v.attrs = attrs;
    return v;
}

export function ParamStatement(paramToken: Terminal, attrs: TagAttribute[]) {
    const v = NodeBase<ParamStatementBase>(NodeKind.paramStatement, mergeRanges(paramToken, attrs)) as ParamStatementDefault;
    v.subType = ParamStatementSubType.default;
    v.paramToken = paramToken;
    v.attrs = attrs;
    return v;
}