import { ArrayLiteralInitializerMemberSubtype, BlockType, CfTag, ForSubType, IndexedAccessType, Node, NodeId, StatementType, StaticallyKnownScopeName, StructLiteralInitializerMemberSubtype, SymTab, TagAttribute, UnaryOperatorPos } from "./node";
import { NodeType } from "./node";
import { Token, TokenType, CfFileType, SourceRange } from "./scanner";

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

    if (node.kind === NodeType.simpleStringLiteral) {
        return node.textSpan.text;
    }
    else if (node.kind === NodeType.terminal) {
        return node.token.text;
    }
    else if (node.kind === NodeType.numericLiteral) {
        return node.literal.token.text;
    }
    else if (node.kind === NodeType.hashWrappedExpr || node.kind === NodeType.parenthetical) {
        return getTriviallyComputableString(node.expr);
    }
    else if (node.kind === NodeType.interpolatedStringLiteral) {
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
    else if (node.kind === NodeType.identifier) {
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

    if (node.kind === NodeType.booleanLiteral) {
        return node.literal.token.type === TokenType.KW_TRUE;
    }
    else if (node.kind === NodeType.numericLiteral) {
        return castCfNumericLiteralAsCfBoolean(node.literal.token.text);
    }
    else if (node.kind === NodeType.hashWrappedExpr || node.kind === NodeType.parenthetical) {
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
    const name_ = name.toLowerCase();
    for (const attr of attrs) {
        if (attr.lcName === name_) {
            return attr.expr
                ? attr.expr
                : undefined;
        }
    }
    return null;
}

// falsy return values keep it going
function forEachNode<T>(nodeList: Node[], f: (node: Node) => T) : T | undefined {
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
        case NodeType.comment:
        case NodeType.textSpan:
            // bottomed out
            return;
        case NodeType.terminal:
            // trivia is generally expected to be text/comments,
            // but it can be anything; for example, an incorrectly placed tag between </cfcatch> ... </cftry>
            return forEachNode(node.trivia, visitor);
        case NodeType.sourceFile:
            return forEachNode(node.content, visitor);
        case NodeType.hashWrappedExpr:
            return visitor(node.leftHash)
                || visitor(node.expr)
                || visitor(node.rightHash);
        case NodeType.parenthetical:
            return visitor(node.leftParen)
                || visitor(node.expr)
                || visitor(node.rightParen);
        case NodeType.tagAttribute:
            return visitor(node.name)
                || visitor(node.equals)
                || visitor(node.expr);
        case NodeType.tag:
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
        case NodeType.callExpression:
            return visitor(node.left)
                || visitor(node.leftParen)
                || forEachNode(node.args, visitor)
                || visitor(node.rightParen);
        case NodeType.callArgument:
            return visitor(node.name)
                || visitor(node.equals)
                || visitor(node.expr)
                || visitor(node.comma);
        case NodeType.unaryOperator:
            if (node.pos === UnaryOperatorPos.pre) {
                return visitor(node.operator)
                    || visitor(node.expr);
            }
            else {
                return visitor(node.expr)
                    || visitor(node.operator);
            }
        case NodeType.binaryOperator:
            return visitor(node.left)
                || visitor(node.operator)
                || visitor(node.right);
        case NodeType.conditional:
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
        case NodeType.variableDeclaration:
            return visitor(node.finalModifier)
                || visitor(node.varModifier)
                || visitor(node.expr);
        case NodeType.statement:
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
        case NodeType.returnStatement:
            return visitor(node.returnToken)
                || visitor(node.expr)
                || visitor(node.semicolon);
        case NodeType.breakStatement:
            return visitor(node.breakToken)
                || visitor(node.semicolon);
        case NodeType.continueStatement:
            return visitor(node.continueToken)
                || visitor(node.semicolon);
        case NodeType.block:
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
        case NodeType.simpleStringLiteral:
            return visitor(node.leftQuote)
                || visitor(node.textSpan)
                || visitor(node.rightQuote);
        case NodeType.interpolatedStringLiteral:
            return visitor(node.leftQuote)
                || forEachNode(node.elements, visitor)
                || visitor(node.rightQuote);
        case NodeType.numericLiteral:
            return visitor(node.literal);
        case NodeType.booleanLiteral:
            return visitor(node.literal);
        case NodeType.identifier:
            return visitor(node.source);
        case NodeType.indexedAccess:
            return visitor(node.root)
            || forEachNode(node.accessElements, visitor);
        case NodeType.indexedAccessChainElement:
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
        case NodeType.sliceExpression:
            return visitor(node.from)
                || visitor(node.colon1)
                || visitor(node.to)
                || visitor(node.colon2)
                || visitor(node.stride);
        case NodeType.functionParameter:
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
        case NodeType.functionDefinition:
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
        case NodeType.arrowFunctionDefinition:
            return visitor(node.parens?.left)
                || forEachNode(node.params, visitor)
                || visitor(node.parens?.right)
                || visitor(node.fatArrow)
                || visitor(node.body)
        case NodeType.dottedPath:
            // @fixme dottedpath is just wrong
            // the ...rest part of node needs to be a Node[],
            // or we need to find the more correct abstraction of DottedPath
            // really it is an indexed access where all elements are Dot?
            return visitor(node.headKey)
                || false;
        case NodeType.switch:
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
        case NodeType.switchCase:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag)
                    || forEachNode(node.body, visitor)
                    || visitor(node.tagOrigin.endTag);
            }
            return visitor(node.caseOrDefaultToken)
                || visitor(node.expr)
                || visitor(node.colon)
                || forEachNode(node.body, visitor)
        case NodeType.do:
            return visitor(node.doToken)
                || visitor(node.body)
                || visitor(node.whileToken)
                || visitor(node.leftParen)
                || visitor(node.expr)
                || visitor(node.rightParen);
        case NodeType.while:
            return visitor(node.whileToken)
                || visitor(node.leftParen)
                || visitor(node.expr)
                || visitor(node.rightParen)
                || visitor(node.body)
        case NodeType.ternary:
            return visitor(node.expr)
                || visitor(node.questionMark)
                || visitor(node.ifTrue)
                || visitor(node.colon)
                || visitor(node.ifFalse);
        case NodeType.for:
            if (node.subType === ForSubType.forIn) {
                return visitor(node.forToken)
                    || visitor(node.leftParen)
                    || visitor(node.forIn!.init)
                    || visitor(node.forIn!.inToken)
                    || visitor(node.forIn!.expr)
                    || visitor(node.rightParen)
                    || visitor(node.body);
            }
            return visitor(node.forToken)
                || visitor(node.leftParen)
                || visitor(node.for!.initExpr)
                || visitor(node.for!.semi1)
                || visitor(node.for!.conditionExpr)
                || visitor(node.for!.semi2)
                || visitor(node.for!.incrementExpr)
                || visitor(node.rightParen)
                || visitor(node.body);
        case NodeType.structLiteral:
            return visitor(node.leftDelimiter)
                || forEachNode(node.members, visitor)
                || visitor(node.rightDelimiter);
        case NodeType.structLiteralInitializerMember:
            if (node.subType === StructLiteralInitializerMemberSubtype.keyed) {
                return visitor(node.key)
                    || visitor(node.colon)
                    || visitor(node.expr)
                    || visitor(node.comma);
            }
            else {
                return visitor(node.dotDotDot)
                    || visitor(node.expr);
            }
        case NodeType.arrayLiteral:
            return visitor(node.leftBracket)
                || forEachNode(node.members, visitor)
                || visitor(node.rightBracket)
        case NodeType.arrayLiteralInitializerMember:
            if (node.subType === ArrayLiteralInitializerMemberSubtype.simple) {
                return visitor(node.expr)
                    || visitor(node.comma);
            }
            else {
                return visitor(node.dotDotDot)
                    || visitor(node.expr)
                    || visitor(node.comma);
            }
        case NodeType.try:
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
        case NodeType.catch:
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
        case NodeType.finally:
            if (node.fromTag) {
                return visitor(node.tagOrigin.startTag)
                    || forEachNode(node.body, visitor)
                    || visitor(node.tagOrigin.endTag);
            }
            return visitor(node.finallyToken)
                || visitor(node.leftBrace)
                || forEachNode(node.body, visitor)
                || visitor(node.rightBrace);
        case NodeType.importStatement:
            return visitor(node.importToken)
                || visitor(node.path)
                || visitor(node.semicolon);
        case NodeType.new:
            return visitor(node.newToken)
                || visitor(node.callExpr);
        case NodeType.type:
            return;
        default:
            ((_:never) => { throw "Non-exhaustive case or unintentional fallthrough." })(node);
    }
}

export interface NodeSourceMap {
    nodeId: number,
    range: SourceRange
}

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
            if (node.kind === NodeType.terminal || node.kind === NodeType.comment || node.kind === NodeType.textSpan) {
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

    function mid(a: number, b: number, floorOrCeil: 'f' | 'c') {
        return floorOrCeil === 'f'
            ? Math.floor((a+b)/2)
            : Math.ceil((a+b)/2);
    }

    let floor = 0;
    let ceil = vs.length - 1;
    let index = mid(floor, ceil, 'f');

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
            index = mid(floor, ceil, 'f');
            continue;
        }
        else if (compare === 1) {
            // T is more than target, move ceil
            ceil = index-1;
            index = mid(floor, ceil, 'f');
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
            //                                            single char is [3,4)  ---^^--- on pos 4
            // so cursor is right after the dot, on position 4
            // we want to say were "in" the dot
            //
            if (v.range.fromInclusive < index && index <= v.range.toExclusive) {
                // match: on or in the target index
                return 0;
            }
            else if (v.range.toExclusive < index) {
                return -1;
            }
            else {
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
            case NodeType.sourceFile: // fallthough
            case NodeType.comment:
                return false;
            case NodeType.terminal: // fallthrough
            case NodeType.identifier:
            case NodeType.textSpan:
                node = node.parent;
                continue outer;
            case NodeType.hashWrappedExpr:
                return true;
            case NodeType.try:
            case NodeType.catch:
            case NodeType.finally:
            case NodeType.switch:
            case NodeType.functionDefinition:
                return !node.fromTag;
            case NodeType.tag:
                return node.canonicalName === "if" || node.canonicalName === "elseif" || node.canonicalName === "return" || node.canonicalName === "set";
            case NodeType.statement:
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
            case NodeType.block:
                switch (node.subType) {
                    case BlockType.fromTag:
                        return node.tagOrigin.startTag!.canonicalName === "script";
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
        if (node.kind === NodeType.block && node.subType === BlockType.fromTag) {
            if (node.tagOrigin.startTag?.canonicalName === "script") {
                return true;
            }
        }
        node = node.parent;
    }
    return false;
}

export function getNearestEnclosingScope(node: Node, scopeName: StaticallyKnownScopeName) : SymTab | undefined {
    while (true) {
        // scope on this node contains the target scope
        if (node.containedScope?.[scopeName]) {
            return node.containedScope[scopeName];
        }
        // didn't find it, but there is a container; we can jump to the parent container
        else if (node.containedScope?.container) {
            node = node.containedScope.container;
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