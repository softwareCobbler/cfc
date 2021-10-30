import * as path from "path";
import * as fs from "fs";
import { ArrayLiteralInitializerMemberSubtype, ArrowFunctionDefinition, Block, BlockType, CallArgument, CfTag, DottedPath, ForSubType, FunctionDefinition, Identifier, IndexedAccess, IndexedAccessType, InterpolatedStringLiteral, isStaticallyKnownScopeName, Node, NodeFlags, NodeId, ParamStatementSubType, ScopeDisplay, SimpleStringLiteral, SourceFile, StatementType, StaticallyKnownScopeName, StructLiteralInitializerMemberSubtype, SymbolResolution, SymbolTable, SymTabResolution, TagAttribute, UnaryOperatorPos } from "./node";
import { NodeKind } from "./node";
import { Token, TokenType, CfFileType, SourceRange } from "./scanner";
import { cfFunctionSignature, isStructLike, TypeFlags } from "./types";
import { EngineSymbolResolver } from "./project";

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
    catch:          TagFact.ALLOW_BODY,
    component:      TagFact.REQUIRE_BODY,
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
    finally:        TagFact.REQUIRE_BODY,
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
    loop:           TagFact.REQUIRE_BODY,
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
    try:            TagFact.REQUIRE_BODY,
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

export function cfmOrCfc(fname: string) : CfFileType | undefined {
    return /\.cfc$/i.test(fname)
        ? CfFileType.cfc
        : /\.d\.cfm$/.test(fname)
        ? CfFileType.dCfm
        : /\.cfml?$/i.test(fname)
        ? CfFileType.cfm
        : undefined;
}

export function requiresEndTag(tag: CfTag) : boolean {
	const facts = getTagFacts(tag);
    return !!facts && !!(facts & TagFact.REQUIRE_BODY);
}

export function allowTagBody(tag: CfTag) : boolean {
    const facts = getTagFacts(tag);
    return !!(facts && (
        (facts & TagFact.ALLOW_BODY) ||
        (facts & TagFact.REQUIRE_BODY)));
}

export function isLexemeLikeToken(token: Token, allowNumeric = false) : boolean {
    const val = token.type;
    return val === TokenType.LEXEME
        || (allowNumeric && val === TokenType.NUMBER)
        || (val > TokenType._FIRST_KW && val < TokenType._LAST_KW)
        || (val > TokenType._FIRST_LIT && val < TokenType._LAST_LIT);
}

// a sugared tag is a tagname that kicks off special script-syntax:
// <sugared-tag-name> <attrs>* <braced-block-representing-tag-child-elements>?
// `param` almost follows this pattern, but lucee supports some different syntax for it; see parser for more details
const sugaredTagNames = new Set<string>(["component", "interface", "savecontent", "lock", "transaction"]);
export function isSugaredTagName(text: string) {
    return sugaredTagNames.has(text);
}

// based on testing against CF-2021
const illegalIdentifierNames = new Set<string>(["function", "final", "default"]);
export function isIllegalIdentifierName(text: string) {
    return illegalIdentifierNames.has(text);
}

/**
 * a string is trivially computable if, possibly stripping outer hash-wrapper and
 * any number of parentheses, we arrive at:
 *      a string with 1 element, which is a TextSpan | Terminal (InterpolatedStringLiteral | NumericLiteral)
 *      an integer numeric literal
 */
 export function getTriviallyComputableString(node: Node | undefined | null) : string | undefined {
    if (!node) return undefined;

    if (node.kind === NodeKind.simpleStringLiteral) {
        return node.textSpan.text;
    }
    else if (node.kind === NodeKind.terminal) {
        return node.token.text;
    }
    else if (node.kind === NodeKind.numericLiteral) {
        return node.literal.token.text;
    }
    else if (node.kind === NodeKind.hashWrappedExpr || node.kind === NodeKind.parenthetical) {
        return getTriviallyComputableString(node.expr);
    }
    else if (node.kind === NodeKind.interpolatedStringLiteral) {
        let result = "";
        for (let i = 0; i < node.elements.length; i++) {
            let trivialElement = getTriviallyComputableString(node.elements[0]);
            if (trivialElement === undefined) {
                return undefined;
            }
            else {
                result += trivialElement;
            }
        }
        return result;
    }
    else if (node.kind === NodeKind.identifier) {
        return getTriviallyComputableString(node.source);
    }

    return undefined;
}

export function getTriviallyComputableBoolean(node: Node | undefined | null) : boolean | undefined {
    if (!node) return undefined

    let trivialString = getTriviallyComputableString(node);
    if (trivialString) {
        return castCfStringAsCfBoolean(trivialString);
    }

    if (node.kind === NodeKind.booleanLiteral) {
        return node.literal.token.type === TokenType.KW_TRUE;
    }
    else if (node.kind === NodeKind.numericLiteral) {
        return castCfNumericLiteralAsCfBoolean(node.literal.token.text);
    }
    else if (node.kind === NodeKind.hashWrappedExpr || node.kind === NodeKind.parenthetical) {
        return getTriviallyComputableBoolean(node.expr);
    }

    return undefined;
}

/**
 * there are cases where a negative value is not accepted by the cf engine as a boolean in certain positions,
 * like <cfargument name="foo" required="-1">
 * but in others, it is OK, e.g. `if (-1) { true because non-zero } `
 * so -- should callers check for such cases before calling this ?
 */
export function castCfNumericLiteralAsCfBoolean(numericLiteralText: string) : boolean | undefined {
    const val = parseFloat(numericLiteralText);
    return isNaN(val)
        ? undefined
        : val !== 0;
}

export function castCfStringAsCfBoolean(stringText: string) : boolean | undefined {
    if (stringText.length <= "false".length) {
        switch (stringText.toLowerCase()) {
            case "yes":
            case "true":
                return true;
            case "no":
            case "false":
                return false;
            default: // no-op, fallthrough
        }
    }

    // didn't get a nice string match
    // maybe we got a number in the form of a string
    return castCfNumericLiteralAsCfBoolean(stringText);
}

/**
 * get the value for some attribute
 * returns:
 *      the attributes value if found,
 *      undefined if the attribute exists but there is no expression associated with the attribute,
 *      null if not found
 */
export function getAttributeValue(attrs: TagAttribute[], name: string) : Node | undefined | null {
    const canonicalName = name.toLowerCase();
    for (const attr of attrs) {
        if (attr.canonicalName === canonicalName) {
            return attr.expr
                ? attr.expr
                : undefined;
        }
    }
    return null;
}

export function getAttribute(attrs: TagAttribute[], name: string) : TagAttribute | null {
    const canonicalName = name.toLowerCase();
    for (const attr of attrs) {
        if (attr.canonicalName === canonicalName) {
            return attr;
        }
    }
    return null;
}

// falsy return values keep it going
function forEachNode<T>(nodeList: readonly Node[], f: (node: Node) => T) : T | undefined {
    for (let i = 0; i < nodeList.length; i++) {
        const result = f(nodeList[i]);
        if (result) return result;
    }
    return undefined;
}

// a falsy value returned by the visitor keeps it going
export function visit(node: Node | Node[], visitor: (arg: Node | undefined | null) => any) : void {
    if (Array.isArray(node)) {
        return forEachNode(node, visitor);
    }
    switch (node.kind) {
        case NodeKind.comment:
        case NodeKind.textSpan:
            // bottomed out
            return;
        case NodeKind.terminal:
            // trivia is generally expected to be text/comments,
            // but it can be anything; for example, an incorrectly placed tag between </cfcatch> ... </cftry>
            return forEachNode(node.trivia, visitor);
        case NodeKind.sourceFile:
            return forEachNode(node.content, visitor);
        case NodeKind.hashWrappedExpr:
            return visitor(node.leftHash)
                || visitor(node.expr)
                || visitor(node.rightHash);
        case NodeKind.parenthetical:
            return visitor(node.leftParen)
                || visitor(node.expr)
                || visitor(node.rightParen);
        case NodeKind.tagAttribute:
            return visitor(node.name)
                || visitor(node.equals)
                || visitor(node.expr);
        case NodeKind.tag:
            switch (node.tagType) {
                case CfTag.TagType.common:
                    return visitor(node.tagStart)
                        || visitor(node.tagName)
                        || forEachNode(node.attrs, visitor)
                        || visitor(node.voidSlash)
                        || visitor(node.tagEnd);
                case CfTag.TagType.scriptLike:
                    return visitor(node.tagStart)
                        || visitor(node.tagName)
                        || visitor(node.expr)
                        || visitor(node.voidSlash)
                        || visitor(node.tagEnd);
                case CfTag.TagType.script:
                    return visitor(node.tagStart)
                        || visitor(node.tagName)
                        || visitor(node.voidSlash)
                        || visitor(node.tagEnd);
                default: return;
            }
        case NodeKind.callExpression:
            return visitor(node.left)
                || visitor(node.leftParen)
                || forEachNode(node.args, visitor)
                || visitor(node.rightParen);
        case NodeKind.callArgument:
            return visitor(node.name)
                || visitor(node.equals)
                || visitor(node.expr)
                || visitor(node.comma);
        case NodeKind.unaryOperator:
            if (node.pos === UnaryOperatorPos.pre) {
                return visitor(node.operator)
                    || visitor(node.expr);
            }
            else {
                return visitor(node.expr)
                    || visitor(node.operator);
            }
        case NodeKind.binaryOperator:
            return visitor(node.left)
                || visitor(node.operator)
                || visitor(node.right);
        case NodeKind.conditional:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag)
                    || visitor(node.consequent)
                    || visitor(node.alternative)
                    || visitor(node.tagOrigin.endTag);
            }
            else {
                return visitor(node.elseToken)
                    || visitor(node.ifToken)
                    || visitor(node.leftParen)
                    || visitor(node.expr)
                    || visitor(node.rightParen)
                    || visitor(node.consequent)
                    || visitor(node.alternative);
            }
        case NodeKind.variableDeclaration:
            return visitor(node.finalModifier)
                || visitor(node.varModifier)
                || visitor(node.expr);
        case NodeKind.statement:
            switch (node.subType) {
                case StatementType.expressionWrapper:
                    // we have a <cfset> tag wrapped in a statement
                    if (node.tagOrigin.startTag) {
                        return visitor(node.tagOrigin.startTag.tagStart)
                            || visitor(node.tagOrigin.startTag.tagName)
                            || visitor(node.expr)
                            || visitor(node.tagOrigin.startTag.voidSlash)
                            || visitor(node.tagOrigin.startTag.tagEnd)
                    }
                    else {
                        return visitor(node.expr)
                            || visitor(node.semicolon)
                    }
                case StatementType.fromTag:
                    return visitor(node.tagOrigin.startTag);
                case StatementType.scriptSugaredTagCallStatement:
                case StatementType.scriptTagCallStatement:
                default: return;
            }
        case NodeKind.returnStatement:
            return visitor(node.returnToken)
                || visitor(node.expr)
                || visitor(node.semicolon);
        case NodeKind.breakStatement:
            return visitor(node.breakToken)
                || visitor(node.semicolon);
        case NodeKind.continueStatement:
            return visitor(node.continueToken)
                || visitor(node.semicolon);
        case NodeKind.block:
            switch (node.subType) {
                case BlockType.fromTag:
                    return visitor(node.tagOrigin.startTag)
                        || forEachNode(node.stmtList, visitor)
                        || visitor(node.tagOrigin.endTag);
                case BlockType.cLike: {
                    return visitor(node.leftBrace)
                        || forEachNode(node.stmtList, visitor)
                        || visitor(node.rightBrace);
                }
                case BlockType.scriptSugaredTagCallBlock: {
                    return visitor(node.name)
                        || forEachNode(node.sugaredCallStatementAttrs!, visitor)
                        || visitor(node.leftBrace)
                        || forEachNode(node.stmtList, visitor)
                        || visitor(node.rightBrace);
                }
                case BlockType.scriptTagCallBlock: {
                    return visitor(node.name)
                        || visitor(node.tagCallStatementArgs!.leftParen)
                        || forEachNode(node.tagCallStatementArgs!.args, visitor)
                        || visitor(node.tagCallStatementArgs!.rightParen)
                        || visitor(node.leftBrace)
                        || forEachNode(node.stmtList, visitor)
                        || visitor(node.rightBrace);
                }
            }
        case NodeKind.simpleStringLiteral:
            return visitor(node.leftQuote)
                || visitor(node.textSpan)
                || visitor(node.rightQuote);
        case NodeKind.interpolatedStringLiteral:
            return visitor(node.leftQuote)
                || forEachNode(node.elements, visitor)
                || visitor(node.rightQuote);
        case NodeKind.numericLiteral:
            return visitor(node.literal);
        case NodeKind.booleanLiteral:
            return visitor(node.literal);
        case NodeKind.identifier:
            return visitor(node.source);
        case NodeKind.indexedAccess:
            return visitor(node.root)
            || forEachNode(node.accessElements, visitor);
        case NodeKind.indexedAccessChainElement:
            switch (node.accessType) {
                case IndexedAccessType.dot:
                    return visitor(node.dot)
                        || visitor(node.property);
                case IndexedAccessType.bracket:
                    return visitor(node.leftBracket)
                        || visitor(node.expr)
                        || visitor(node.rightBracket)
                case IndexedAccessType.optionalDot:
                    return visitor(node.questionMark)
                        || visitor(node.dot)
                        || visitor(node.property);
                case IndexedAccessType.optionalBracket:
                    return visitor(node.questionMark)
                        || visitor(node.leftBracket)
                        || visitor(node.expr)
                        || visitor(node.rightBracket);
                case IndexedAccessType.optionalCall:
                    return visitor(node.questionMark)
                        || visitor(node.dot);
            }
        case NodeKind.sliceExpression:
            return visitor(node.from)
                || visitor(node.colon1)
                || visitor(node.to)
                || visitor(node.colon2)
                || visitor(node.stride);
        case NodeKind.functionParameter:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag);
            }
            return visitor(node.requiredTerminal)
                || visitor(node.javaLikeTypename)
                || visitor(node.dotDotDot)
                || visitor(node.identifier)
                || visitor(node.equals)
                || visitor(node.defaultValue)
                || visitor(node.comma)
        case NodeKind.functionDefinition:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag)
                    || forEachNode(node.params, visitor)
                    || forEachNode(node.body, visitor)
                    || visitor(node.tagOrigin.endTag);
            }
            return visitor(node.accessModifier)
                || visitor(node.returnType)
                || visitor(node.functionToken)
                || visitor(node.nameToken)
                || visitor(node.leftParen)
                || forEachNode(node.params, visitor)
                || visitor(node.rightParen)
                || forEachNode(node.attrs, visitor)
                || visitor(node.body);
        case NodeKind.arrowFunctionDefinition:
            return visitor(node.parens?.left)
                || forEachNode(node.params, visitor)
                || visitor(node.parens?.right)
                || visitor(node.fatArrow)
                || visitor(node.body)
        case NodeKind.dottedPath:
            return visitor(node.headKey)
                || forEachNode(node.rest, visitor);
        case NodeKind.dottedPathRest:
            return visitor(node.dot)
                || visitor(node.key);
        case NodeKind.switch:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag)
                    || forEachNode(node.cases, visitor)
                    || visitor(node.tagOrigin.endTag);
            }
            return visitor(node.switchToken)
                || visitor(node.leftParen)
                || visitor(node.expr)
                || visitor(node.rightParen)
                || visitor(node.leftBrace)
                || forEachNode(node.cases, visitor)
                || visitor(node.rightBrace);
        case NodeKind.switchCase:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag)
                    || forEachNode(node.body, visitor)
                    || visitor(node.tagOrigin.endTag);
            }
            return visitor(node.caseOrDefaultToken)
                || visitor(node.expr)
                || visitor(node.colon)
                || forEachNode(node.body, visitor)
        case NodeKind.do:
            return visitor(node.doToken)
                || visitor(node.body)
                || visitor(node.whileToken)
                || visitor(node.leftParen)
                || visitor(node.expr)
                || visitor(node.rightParen);
        case NodeKind.while:
            return visitor(node.whileToken)
                || visitor(node.leftParen)
                || visitor(node.expr)
                || visitor(node.rightParen)
                || visitor(node.body)
        case NodeKind.ternary:
            return visitor(node.expr)
                || visitor(node.questionMark)
                || visitor(node.ifTrue)
                || visitor(node.colon)
                || visitor(node.ifFalse);
        case NodeKind.for:
            if (node.subType === ForSubType.forIn) {
                return visitor(node.forToken)
                    || visitor(node.leftParen)
                    || visitor(node.init)
                    || visitor(node.inToken)
                    || visitor(node.expr)
                    || visitor(node.rightParen)
                    || visitor(node.body);
            }
            return visitor(node.forToken)
                || visitor(node.leftParen)
                || visitor(node.initExpr)
                || visitor(node.semi1)
                || visitor(node.conditionExpr)
                || visitor(node.semi2)
                || visitor(node.incrementExpr)
                || visitor(node.rightParen)
                || visitor(node.body);
        case NodeKind.structLiteral:
            return visitor(node.leftDelimiter)
                || forEachNode(node.members, visitor)
                || visitor(node.rightDelimiter);
        case NodeKind.structLiteralInitializerMember:
            if (node.subType === StructLiteralInitializerMemberSubtype.keyed) {
                if (node.shorthand) {
                    return visitor(node.key)
                        || visitor(node.comma);
                }
                else {
                    return visitor(node.key)
                        || visitor(node.colon)
                        || visitor(node.expr)
                        || visitor(node.comma);
                }
            }
            else {
                return visitor(node.dotDotDot)
                    || visitor(node.expr);
            }
        case NodeKind.arrayLiteral:
            return visitor(node.leftBracket)
                || forEachNode(node.members, visitor)
                || visitor(node.rightBracket)
        case NodeKind.arrayLiteralInitializerMember:
            if (node.subType === ArrayLiteralInitializerMemberSubtype.simple) {
                return visitor(node.expr)
                    || visitor(node.comma);
            }
            else {
                return visitor(node.dotDotDot)
                    || visitor(node.expr)
                    || visitor(node.comma);
            }
        case NodeKind.try:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag)
                    || forEachNode(node.body, visitor)
                    || forEachNode(node.catchBlocks, visitor)
                    || visitor(node.finallyBlock)
                    || visitor(node.tagOrigin.endTag);
            }
            return visitor(node.tryToken)
                || visitor(node.leftBrace)
                || forEachNode(node.body, visitor)
                || visitor(node.rightBrace)
                || forEachNode(node.catchBlocks, visitor)
                || visitor(node.finallyBlock);
        case NodeKind.catch:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag)
                    || forEachNode(node.body, visitor)
                    || visitor(node.tagOrigin.endTag);
            }
            
            return visitor(node.catchToken)
                || visitor(node.leftParen)
                || visitor(node.exceptionType)
                || visitor(node.exceptionBinding)
                || visitor(node.rightParen)
                || visitor(node.leftBrace)
                || forEachNode(node.body, visitor)
                || visitor(node.rightBrace);
        case NodeKind.finally:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag)
                    || forEachNode(node.body, visitor)
                    || visitor(node.tagOrigin.endTag);
            }
            return visitor(node.finallyToken)
                || visitor(node.leftBrace)
                || forEachNode(node.body, visitor)
                || visitor(node.rightBrace);
        case NodeKind.importStatement:
            return visitor(node.importToken)
                || visitor(node.path)
                || visitor(node.semicolon);
        case NodeKind.new:
            return visitor(node.newToken)
                || visitor(node.callExpr);
        case NodeKind.typeShim:
            return;
        case NodeKind.property:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag)
            }
            else {
                return visitor(node.propertyTerminal)
                || forEachNode(node.attrs, visitor)
            }
        case NodeKind.paramStatement: 
            return (node.subType === ParamStatementSubType.withImplicitTypeAndName ? visitor(node.implicitType) : undefined)
                || (node.subType === ParamStatementSubType.withImplicitName ? visitor(node.implicitName) || visitor(node.implicitNameEquals) || visitor(node.implicitNameExpr) : undefined)
                || forEachNode(node.attrs, visitor);
        default:
            exhaustiveCaseGuard(node);
    }
}

export interface NodeSourceMap {
    nodeId: number,
    range: SourceRange
}

export function exhaustiveCaseGuard(_:never) : never { throw "Non-exhaustive case or unintentional fallthrough."; }

export function flattenTree(tree: Node | Node[]) : NodeSourceMap[] {
    const result : NodeSourceMap[] = [];

    function pushNode(node: Node) {
        if (result.length > 0) {
            if (result[result.length-1].range.toExclusive > node.range.fromInclusive) {
                throw "each subsequent node should start on or after the exclusive end of the previous node";
            }
        }
        result.push({
            nodeId: node.nodeId,
            range: node.range
        });
    }

    function visitor(node: Node | undefined | null) {
        if (node) {
            // a docBlock attribute is sourced from a comment; the comments get visited and flattened, so we don't need to do the same
            // to the attributes the doc block generates (if we did so, they would be out-of-source order, too, which would break things)
            if (node.kind === NodeKind.tagAttribute && node.flags & NodeFlags.docBlock) {
                return;
            }
            
            if (node.kind === NodeKind.terminal || node.kind === NodeKind.comment || node.kind === NodeKind.textSpan) {
                pushNode(node);
            }

            // we also want the trivia for a terminal, too
            visit(node, visitor);
        }
    }

    Array.isArray(tree)
        ? forEachNode(tree, visitor)
        : visit(tree, visitor);

    return result;
}

// conform to java's java.util.Arrays.binarySearch
export function binarySearch<T>(vs: T[], comparator: (v: T) => number) : number {
    if (vs.length === 0) {
        return -1;
    }

    function mid(a: number, b: number) {
        return Math.floor((a+b)/2);
    }

    let floor = 0;
    let ceil = vs.length - 1;
    let index = mid(floor, ceil);

    while (floor <= ceil) {
        const compare = comparator(vs[index]);
        if (compare === 0) {
            // match
            return index;
        }

        if (index === 0) {
            return -1; // ~0 == -1
        }
        else if (index === ceil) {
            return ~index; // ~index == -(index+1)
        }
        else if (compare === -1) {
            // T is less than target, move floor
            floor = index+1;
            index = mid(floor, ceil);
            continue;
        }
        else if (compare === 1) {
            // T is more than target, move ceil
            ceil = index-1;
            index = mid(floor, ceil);
        }
    }

    return ~index;
}

export function findNodeInFlatSourceMap(flatSourceMap: NodeSourceMap[], nodeMap: ReadonlyMap<NodeId, Node>, index: number) {
    if (flatSourceMap.length === 0) return undefined;

    let match = binarySearch(flatSourceMap,
        (v) => {
            //
            // can be equal to range.toExclusive because if the cursor is "on" "abc.|x"
            //                                        single char '.' is [3,4)  ---^^--- cursor is "on" pos 4
            // so cursor is right after the dot, on position 4
            // we want to say were "in" the dot
            //
            if (v.range.fromInclusive < index && index <= v.range.toExclusive) {
                // match: on or in the target index
                return 0;
            }
            else if (v.range.toExclusive < index) { // move floor up, our target is further ahead in the input
                return -1;
            }
            else { // move ceiling down, our target node is before this node
                return 1;
            }
        });

    // if we didn't match get the closest to "leftmost" node
    match = match < 0 ? ~match : match;
    return nodeMap.get(flatSourceMap[match].nodeId);
}

export function isExpressionContext(node: Node | null) : boolean {
    outer:
    while (node) {
        switch (node.kind) {
            case NodeKind.sourceFile: // fallthough
            case NodeKind.comment:
                return false;
            case NodeKind.terminal: // fallthrough
            case NodeKind.identifier:
            case NodeKind.textSpan:
                node = node.parent;
                continue outer;
            case NodeKind.hashWrappedExpr:
                return true;
            case NodeKind.try:
            case NodeKind.catch:
            case NodeKind.finally:
            case NodeKind.switch:
            case NodeKind.functionDefinition:
                return !node.fromTag;
            case NodeKind.tag:
                return node.canonicalName === "if" || node.canonicalName === "elseif" || node.canonicalName === "return" || node.canonicalName === "set";
            case NodeKind.statement:
                switch (node.subType) {
                    case StatementType.fromTag:
                        return false;
                    case StatementType.expressionWrapper:
                    case StatementType.scriptSugaredTagCallStatement:
                    case StatementType.scriptTagCallStatement:
                        return true;
                    default:
                        // fixme: clean up union so we can detect we've exhaustively checked
                        return false;
                }
            case NodeKind.block:
                switch (node.subType) {
                    case BlockType.fromTag:
                        // startTag might be undefined if this is a "loose" tag block like `<cfif> [here] <cfelseif> ... </cfif>`
                        // should probably represent a "loose" tag block more explicitly
                        return node.tagOrigin.startTag?.canonicalName === "script";
                    case BlockType.scriptSugaredTagCallBlock:
                    case BlockType.cLike:
                    case BlockType.scriptSugaredTagCallBlock:
                        return true;
                    default:
                        return false;
                        // need to setup the Block union such that we can detect that we've exhausted all options
                }
            default:
                node = node.parent;
        }
    }

    return false;
}

export function isCfScriptTagBlock(node: Node | null) : boolean {
    while (node) {
        if (node.kind === NodeKind.block && node.subType === BlockType.fromTag) {
            if (node.tagOrigin.startTag?.canonicalName === "script") {
                return true;
            }
        }
        node = node.parent;
    }
    return false;
}

export function getNearestEnclosingScope(node: Node, scopeName: StaticallyKnownScopeName) : SymbolTable | undefined {
    while (true) {
        // scope on this node contains the target scope
        if (node.containedScope?.[scopeName]) {
            return node.containedScope[scopeName];
        }
        // didn't find it, but there is a container; we can jump to the parent container
        else if (node.containedScope?.parentContainer) {
            node = node.containedScope.parentContainer;
        }
        // didn't find it, and there is no container; climb to parent
        else if (node.parent) {
            node = node.parent;
        }
        // at the root, didn't find it
        else {
            return undefined;
        }
    }
}

export class BiMap<K,V> {
    #forward = new Map<K,V>();
    #reverse = new Map<V,K>();
    constructor() {}

    set(key: K, value: V) {
        this.#forward.set(key, value);
        this.#reverse.set(value, key);
    }

    getByKey(key: K) {
        return this.#forward.get(key);
    }

    getByValue(value: V) {
        return this.#reverse.get(value);
    }

    deleteByKey(key: K) {
        const value = this.getByKey(key);
        if (value) {
            this.#forward.delete(key);
            this.#reverse.delete(value);
        }
    }

    deleteByValue(value: V) {
        const key = this.getByValue(value);
        if (key) {
            this.#forward.delete(key);
            this.#reverse.delete(value);
        }
    }

    allKeys() : K[] {
        return [...this.#forward.keys()];
    }

    allValues() : V[] {
        return [...this.#reverse.keys()];
    }
}

/**
 * if predicate returns true, return the current Node
 * if predicate returns false, set current Node to parent, and try again
 * if predicate returns "bail", return undefined
 */
export function findAncestor(node: Node, predicate: (node: Node) => true | false | "bail", followCfcInheritance = false) : Node | undefined {
    return node.parent ? findSelfOrAncestor(node.parent, predicate, followCfcInheritance) : undefined;
}

export function findSelfOrAncestor(node: Node, predicate: (node: Node) => true | false | "bail", followCfcInheritance = false) : Node | undefined {
    let current : Node | null = node;
    while (current) {
        const result = predicate(current);
        if (result === true) {
            return current;
        }
        else if (result === false) {
            if (current.kind === NodeKind.sourceFile && followCfcInheritance && current.cfc?.extends) {
                current = current.cfc.extends;
            }
            else {
                current = current.parent;
            }
        }
        else if (result === "bail") {
            break;
        }
    }
    return undefined;
}

export function getContainingFunction(node: Node) : FunctionDefinition | ArrowFunctionDefinition | undefined {
    return findAncestor(node, (node) => node.kind === NodeKind.functionDefinition || node.kind === NodeKind.arrowFunctionDefinition) as FunctionDefinition | ArrowFunctionDefinition;
}

export function isCfcMemberFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition) : boolean {
    if (node.kind === NodeKind.arrowFunctionDefinition) return false;
    return !getContainingFunction(node) // there is no outer function
        && getSourceFile(node)?.cfFileType === CfFileType.cfc; // and the file type is a cfc
}

export function getNodeLinks(node: Node) {
    return node.links ?? (node.links = {});
}

export function getSourceFile(node: Node) : SourceFile | undefined {
    return findSelfOrAncestor(node, (node) => node.kind === NodeKind.sourceFile) as SourceFile | undefined;
}

export function getNearestConstruct(node: Node) : Node | undefined {
    return findAncestor(node, (node) => {
        return !!node
            && node.kind !== NodeKind.terminal
            && node.kind !== NodeKind.textSpan;
    });
}

export function isInCfcPsuedoConstructor(node: Node) : boolean {
    return !!findAncestor(node, (node) => {
        // if we have an ancestor of a function definition, we weren't in a psuedo constructor
        if (node.kind === NodeKind.functionDefinition) return "bail";
        // otherwise, if this is a block, check if the block is:
        // 1) a component tag block (i.e., `<cfcomponent>...</cfcomponent>`)
        // 2) a script sugared component block (i.e., `component { ... }`)
        // if it is, then OK, we are in a psuedo constructor
        // otherwise, keep climbing
        return node.kind === NodeKind.block && (
            (node.subType === BlockType.fromTag && node.tagOrigin.startTag?.canonicalName === "component")
            || (node.subType === BlockType.scriptSugaredTagCallBlock && node.name?.token.text.toLowerCase() === "component")
        );
    })
}

export function getAllNamesOfScopeDisplay(scopeDisplay : ScopeDisplay, ...targetKeys: StaticallyKnownScopeName[]) : [StaticallyKnownScopeName, string][] {
    const result : [StaticallyKnownScopeName, string][] = [];
    for (const scopeName of targetKeys) {
        if (scopeDisplay.hasOwnProperty(scopeName)) {
            for (const symTabEntry of scopeDisplay[scopeName]!.values()) {
                result.push([scopeName, symTabEntry.uiName]);
            }
        }
    }

    return result;
}

export function getFunctionSignatureParamNames(sig: cfFunctionSignature, ...omit: string[]) {
    const result : string[] = [];
    const omitSet = new Set(omit);
    for (const param of sig.params) {
        if (!omitSet.has(param.canonicalName)) result.push(param.canonicalName);
    }
    return result;
}

export function isHoistableFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition) : node is FunctionDefinition {
    // fixme: need a more explicit way to say "this function is anonymous", right now we imply it by saying a function has a name
    return node.kind === NodeKind.functionDefinition && (typeof node.canonicalName === "string");
}

export interface CanonicalizedName {
    ui: string,
    canonical: string
}

export function stringifyLValue(node: Identifier | IndexedAccess) : CanonicalizedName | undefined {
    if (node.kind === NodeKind.identifier) return node.uiName && node.canonicalName ? {ui: node.uiName, canonical: node.canonicalName} : undefined;
    let result : {ui: string, canonical: string};

    if (node.root.kind === NodeKind.identifier && node.root.canonicalName && node.root.uiName) {
        result = {ui: node.root.uiName, canonical: node.root.canonicalName};
    }
    else {
        return undefined;
    }

    for (let i = 0; i < node.accessElements.length; i++) {
        const element = node.accessElements[i];
        if (element.accessType === IndexedAccessType.dot) {
            result.canonical += "." + element.property.token.text.toLowerCase();
            result.ui += "." + element.property.token.text;
        }
        else if (element.accessType === IndexedAccessType.bracket) {
            const propertyName = getTriviallyComputableString(element.expr)
            if (!propertyName) return undefined;
            result.canonical += "." + propertyName.toLowerCase();
            result.ui += "." + propertyName;
        }
        else {
            return undefined;
        }
    }
    return result;
}

export function stringifyStringAsLValue(node: SimpleStringLiteral | InterpolatedStringLiteral) : CanonicalizedName | undefined {
    const sval = getTriviallyComputableString(node);
    return sval ? {ui: sval, canonical: sval.toLowerCase()} : undefined;
}

/**
 * if the passed call argument is a named call argument, return the stringified name
 */
export function stringifyCallExprArgName(node: CallArgument) : CanonicalizedName | undefined {
    return node.name
        ? node.name.kind === NodeKind.identifier
            ? stringifyLValue(node.name)
            : stringifyStringAsLValue(node.name)
        : undefined;
}

export function stringifyDottedPath(node: DottedPath) {
    let result = node.headKey.token.text;
    for (let next of node.rest) {
        result += "." + next.key.token.text;
    }
    return {ui: result, canonical: result.toLowerCase()}
}

export function filterNodeList(node: Node | Node[] | null | undefined, cb: (node: Node) => boolean) : Node[] {
    if (!node) return [];
    if (Array.isArray(node)) {
        return node.filter(cb);
    }
    else if (cb(node)) {
        return [node];
    }
    else {
        return [];
    }
}

export type Mutable<T> = {-readonly [K in keyof T]: T[K]};

export function getComponentBlock(sourceFile: SourceFile) : Block | undefined {
    if (sourceFile.cfFileType !== CfFileType.cfc) return undefined;
    let result : Block | undefined = undefined;
    visit(sourceFile, (node) => {
        if (node?.kind === NodeKind.comment || node?.kind === NodeKind.textSpan) {
            return undefined;
        }
        else if (node?.kind === NodeKind.block) {
            if (node.subType === BlockType.fromTag && node.tagOrigin.startTag?.canonicalName === "component") {
                result = node;
                return "bail";
            }
            else if (node.subType === BlockType.scriptSugaredTagCallBlock && node.name?.token.text.toLowerCase() === "component") {
                result = node;
                return "bail";
            }
        }
        return "bail"; // a component file should always be textspans/comments followed by a component block; if we don't match that pattern, we're not going to keep searching the entire file
    });
    return result;
}

export function getComponentAttrs(sourceFile: SourceFile) {
    const componentBlock = getComponentBlock(sourceFile);
    if (componentBlock) {
        return componentBlock.subType === BlockType.fromTag
            ? (componentBlock.tagOrigin.startTag as CfTag.Common).attrs
            : componentBlock.sugaredCallStatementAttrs ?? [];
    }
    else {
        return undefined;
    }
    /*
    if (sourceFile.cfFileType !== CfFileType.cfc) return undefined;
    let attrs : TagAttribute[] | undefined = undefined;
    visit(sourceFile, (node) => {
        if (node?.kind === NodeType.comment || node?.kind === NodeType.textSpan) {
            return undefined;
        }
        else if (node?.kind === NodeType.block) {
            if (node.subType === BlockType.fromTag && node.tagOrigin.startTag?.canonicalName === "component") {
                attrs = (node.tagOrigin.startTag as CfTag.Common).attrs;
                return "bail";
            }
            else if (node.subType === BlockType.scriptSugaredTagCallBlock && node.name?.token.text.toLowerCase() === "component") {
                attrs = node.sugaredCallStatementAttrs ?? [];
                return "bail";
            }
        }
        return "bail"; // a component file should always be textspans/comments followed by a component block; if we don't match that pattern, we're not going to keep searching the entire file
    });
    return attrs;*/
}

export function recursiveGetFiles(root: string, pattern: RegExp) : string [] {
	const result : string[] = [];
	const fds = fs.readdirSync(root, {withFileTypes: true});
	for (const fd of fds) {
		if (fd.isDirectory()) result.push(...recursiveGetFiles(path.resolve(root, fd.name), pattern));
		else if (pattern.test(fd.name)) {
			const fspath = path.resolve(root, fd.name);
			result.push(fspath);
		}
	}
	return result;
}

export function isSimpleOrInterpolatedStringLiteral(node: Node | null) : node is SimpleStringLiteral | InterpolatedStringLiteral {
    return node?.kind === NodeKind.simpleStringLiteral || node?.kind === NodeKind.interpolatedStringLiteral;
}

export function isNamedFunctionArgumentName(node: Node) {
    return node.parent?.kind === NodeKind.callArgument && node === node.parent.name
}

export function isObjectLiteralPropertyName(node: Node) {
    return node.parent?.kind === NodeKind.structLiteralInitializerMember
        && node.parent.subType === StructLiteralInitializerMemberSubtype.keyed
        && node.parent.key === node;
}

export function isInScriptBlock(node: Node) {
    // @fixme perhaps better to just mark script nodes as such with a flag, during parse node finalization
    // `return !!(node.flags & NodeFlags.script)` would be way faster than a tree walk
    return !!findAncestor(node, (node) => {
        if (node.kind === NodeKind.functionDefinition && node.fromTag) return "bail";
        if (node.kind === NodeKind.block && node.subType === BlockType.fromTag && node.tagOrigin.startTag?.canonicalName === "script") return true;
        if (node.kind === NodeKind.block && node.subType === BlockType.scriptSugaredTagCallBlock && (node.name?.token.text === "component" || node.name?.token.text === "interface")) return true;
        return false;
    })
}

//https://helpx.adobe.com/coldfusion/developing-applications/the-cfml-programming-language/using-coldfusion-variables/about-scopes.html
const defaultScopeLookupOrder : readonly StaticallyKnownScopeName[] = [
    "local",
    "arguments",
    "__query", // magic inaccessible scope inside a <cfloop query=#q#>...</cfquery> body
    "__transient", // found a quasi-declaration (raw assignment) which we can possibly use; keep climbing for an actual decl, but use this if we find no other
    "thread",
    "variables",
    "cgi",
    "file",
    "url",
    "form",
    "cookie",
    "client",
    "__cfEngine"
];

export function getScopeDisplayMember(scope: ScopeDisplay,
                                      canonicalName: string,
                                      orderedScopes: readonly StaticallyKnownScopeName[] = defaultScopeLookupOrder) : SymTabResolution | undefined {
    // could we not do this, if we stored a link to the symTab for nested things ?
    // how would that work for arrays? there wouldn't be a symTab for those...
    const path = canonicalName.split(".");
    if (path.length > 1) {
        if (!isStaticallyKnownScopeName(path[0])) return undefined;
        const scopeName = path[0];
        if (!scope.hasOwnProperty(scopeName) || !scope[scopeName as StaticallyKnownScopeName]!.get(path[1])) return undefined;
        let current = scope[scopeName as StaticallyKnownScopeName]!.get(path[1])!;
        for (let i = 2; i < path.length; i++) {
            if (!isStructLike(current.type)) return undefined;
            let maybeNext = current.type.members.get(path[i]);
            if (!maybeNext) return undefined;
            current = maybeNext;
        }
        return {scopeName: path[0], symTabEntry: current};
    }

    for (const scopeName of orderedScopes) {
        if (scope.hasOwnProperty(scopeName)) {
            const entry = scope[scopeName]!.get(canonicalName);
            if (entry) {
                return {scopeName: scopeName, symTabEntry: entry};
            }
        }
    }
    return undefined;
}

function tryResolveFromLibs(sourceFile: SourceFile, canonicalName: string) : SymbolResolution | undefined {
    for (const libFile of sourceFile.libRefs.values()) {
        const symbol = libFile.containedScope.__declaration?.get(canonicalName);
        if (symbol) {
            return {
                container: sourceFile,
                scopeName: "__declaration",
                symTabEntry: symbol
            }
        }
    }
    return undefined;
}

export function walkupScopesToResolveSymbol(base: Node,
                                            canonicalName: string,
                                            engineSymbolResolver?: EngineSymbolResolver, // can remove this, engine resolution happens via well-known interface "__cfEngine"
                                            orderedScopes: readonly StaticallyKnownScopeName[] = defaultScopeLookupOrder) : SymbolResolution | undefined {
    const engineSymbol = engineSymbolResolver?.(canonicalName);
    let node : Node | null = base;

    while (node) {
        if (node.containedScope) {
            const varEntry = getScopeDisplayMember(node.containedScope, canonicalName, orderedScopes);
            if (varEntry) {
                (varEntry as SymbolResolution).container = node;
                return varEntry as SymbolResolution;
            }
            else {
                // lookup symbols from visible interface definitions
                const scopeLookup : readonly StaticallyKnownScopeName[] = node.kind === NodeKind.sourceFile ? orderedScopes : ["variables"];
                for (const scopeName of scopeLookup) {
                    if (node.containedScope.typedefs.mergedInterfaces.has(scopeName)) {
                        const interfaceDef = node.containedScope.typedefs.mergedInterfaces.get(scopeName)!;
                        if (interfaceDef.members.has(canonicalName)) {
                            const symTabEntry = interfaceDef.members.get(canonicalName)!;
                            if (symTabEntry) {
                                return {scopeName, container: node, symTabEntry};
                            }
                        }
                    }
                }
            }

            if (node.kind === NodeKind.sourceFile) {
                const libResolution = tryResolveFromLibs(node, canonicalName);
                if (libResolution) {
                    return libResolution;
                }

                if (node.cfc?.extends) {
                    node = node.cfc.extends;
                    continue;
                }
                
                return engineSymbol
                    ? {scopeName: "__cfEngine", symTabEntry: engineSymbol, container: node} // fixme - this container is not super accurate, but better than null
                    : undefined;
            }

            else {
                node = node.containedScope.parentContainer;
            }
        }
        else {
            node = node.parent;
        }
    }

    return undefined;
}

export function getFunctionDefinitionReturnsLiteral(node: FunctionDefinition) : CanonicalizedName | undefined {
    if (node.fromTag) {
        const attrVal = getAttributeValue(node.attrs, "returns");
        const v = getTriviallyComputableString(attrVal);
        if (!v) return undefined;
        else return {ui: v, canonical: v.toLowerCase()}
    }
    else {
        return node.returnType ? stringifyDottedPath(node.returnType) : undefined;
    }
}

export function getFunctionDefinitionAccessLiteral(node: FunctionDefinition) {
    let val : string;

    if (node.fromTag) {
        const attrVal = getAttributeValue(node.attrs, "access");
        val = getTriviallyComputableString(attrVal)?.toLowerCase() ?? "public";
    }
    else {
        val = node.accessModifier?.token.text.toLowerCase() ?? "public";
    }

    switch (val) {
        case "remote":
        case "public":
        case "protected":
        case "private":
            return val;
        default:
            return "public";
    }
}

/**
 * check if `maybeDescendant` is a descendant of `ancestor`;
 * i.e. by one or more extends clauses can we climb from `maybeDescendant` to `ancestor`
 */
export function cfcIsDescendantOf(ancestor: SourceFile, maybeIsDescendant: SourceFile) : boolean {
    if (maybeIsDescendant.cfFileType !== CfFileType.cfc || ancestor.cfFileType !== CfFileType.cfc) return false;
    let working : SourceFile | null = maybeIsDescendant;
    while (working) {
        if (working === ancestor) return true;
        working = working.cfc?.extends ?? null;
    }
    return false;
}

export function isPublicMethod(sig: cfFunctionSignature) : boolean {
    // a method is "public" if it is NOT protected or private
    // this ensures (though maybe shouldn't be necessary but some completions logic right now relies on it) that non-cfc-bound methods are always considered "public"
    // really what we need is a way to check if a function is "cfc-bound"
    return !(sig.flags & TypeFlags.private || sig.flags & TypeFlags.protected);
}
