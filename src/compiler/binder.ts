/*import { NodeList, NodeBase, Statement, NamedBlock, TagAttribute, StringLiteral, TextSpan, HashWrappedExpr, Parenthetical, NumericLiteral, CfTag } from "./parser";





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


*/