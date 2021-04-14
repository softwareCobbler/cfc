import { NodeList, NodeBase, Statement, NamedBlock, TagAttribute, StringLiteral, TextSpan, HashWrappedExpr, Parenthetical, NumericLiteral, CfTag } from "./parser";

function isFromTag(node: NodeBase) {
    return node.tagOrigin.startTag !== null;
}

/**
 * get the value for some attribute
 * returns:
 *      the attributes value if found,
 *      undefined if the attribute exists but there is no expression associated with the attribute,
 *      null if not found
 */
 function getAttributeValue(attrs: NodeList<TagAttribute>, name: string) : NodeBase | undefined | null {
    for (const attr of attrs.list) {
        if (attr.lcName === name) {
            return attr.expr
                ? attr.expr
                : undefined;
        }
    }
    return null;
}

function bind(list: NodeList<NodeBase>) {
    for (const node of list.list) {
        if (node instanceof Statement) {
            if (isFromTag(node)) {
                bindTagOriginNode(node);
            }
        }
    }
}

function bindTagOriginNode(tagOriginNode: Statement | NamedBlock) {
    switch (tagOriginNode.tagOrigin.startTag!.canonicalName) {
        case "function": bindTagFunction(tagOriginNode as NamedBlock);
    }
}

/**
 * a string is trivially computable if, possibly stripping outer hash-wrapper and
 * any number of parentheses, we arrive at:
 *      a string with 1 element, which is a TextSpan | Terminal (StringLiteral | NumericLiteral)
 *      an integer numeric literal
 */
function getTriviallyComputableString(node: NodeBase | undefined | null) : string | null {
    if (!node) return null;

    if (node instanceof StringLiteral) {
        if (node.elements.list.length === 1 && node.elements.list[0] instanceof TextSpan) {
            //return (node.elements.list[0] as TextSpan).text;
            // if is instanceof BasicString return basicString.text; then remove text from textspan
            return "got it";
        }
    }
    else if (node instanceof NumericLiteral) {
        return node.literal.token.text;
    }
    else if (node instanceof HashWrappedExpr || node instanceof Parenthetical) {
        return getTriviallyComputableString(node.expr);
    }

    return null;
}

function bindTagFunction(tagOriginNode: NamedBlock) {
    const functionNameExpr = getAttributeValue(tagOriginNode.attrs, "name");
    let functionName = getTriviallyComputableString(functionNameExpr);
    if (!functionName) {
        // errorAtRange(tagOriginNode.tagOrigin.start, "<cffunction> requires a name attribute")
    }

    for (const node of tagOriginNode.stmtList.list) {
        if (node instanceof TextSpan || node instanceof Comment) {

        }
        if (node instanceof CfTag.TagBase && node.canonicalName === "argument") {
            // handle function param
            continue;
        }
        break;
    }
}