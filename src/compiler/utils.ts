import { CfTag, Node, TagAttribute } from "./node";
import { NodeType } from "./node";
import { Token, TokenType, CfFileType } from "./scanner";

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

export function cfmOrCfc(fname: string) : CfFileType {
    return /\.cfc$/.test(fname)
        ? CfFileType.cfc
        : CfFileType.cfm
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

    if (node.type === NodeType.simpleStringLiteral) {
        return node.textSpan.text;
    }
    else if (node.type === NodeType.terminal) {
        return node.token.text;
    }
    else if (node.type === NodeType.numericLiteral) {
        return node.literal.token.text;
    }
    else if (node.type === NodeType.hashWrappedExpr || node.type === NodeType.parenthetical) {
        return getTriviallyComputableString(node.expr);
    }
    else if (node.type === NodeType.interpolatedStringLiteral) {
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
    else if (node.type === NodeType.identifier) {
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

    if (node.type === NodeType.booleanLiteral) {
        return node.literal.token.type === TokenType.KW_TRUE;
    }
    else if (node.type === NodeType.numericLiteral) {
        return castCfNumericLiteralAsCfBoolean(node.literal.token.text);
    }
    else if (node.type === NodeType.hashWrappedExpr || node.type === NodeType.parenthetical) {
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
    for (const attr of attrs) {
        if (attr.lcName === name) {
            return attr.expr
                ? attr.expr
                : undefined;
        }
    }
    return null;
}
