import { BinaryOperator, Block, BlockType, CallArgument, FunctionParameter, NilTerminal, Node, NodeType, Statement, StatementType, UnaryOperator } from "./node";

export function Binder() {
    const RootNode = NilTerminal;

    function bindProgram(node: Node[]) {
        bindList(node, RootNode);
    }

    function bindNode(node: Node, parent: Node) {
        node.parent = parent;
        switch (node.type) {
            case NodeType.binaryOperator:
                bindBinaryOperator(node);
                break;
            case NodeType.statement:
                bindStatement(node);
                break;
            case NodeType.block:
                bindBlock(node);
                break;
            case NodeType.arrayLiteral:
            case NodeType.structLiteral:
                break;
            case NodeType.callArgument:
                bindCallArgument(node);
            case NodeType.functionParameter:
        }
    }

    function bindList(nodes: Node[], parent: Node) {
        for (let i = 0; i < nodes.length; ++i) {
            bindNode(nodes[i], parent);
        }
    }

    function bindBinaryOperator(node: BinaryOperator) {
        bindNode(node.left, node);
        bindNode(node.right, node);
    }

    function bindCallArgument(node: CallArgument) {
        if (node.name) bindNode(node.name, node);
        bindNode(node.expr, node);
    }

    function bindFunctionParameter(node: FunctionParameter) {
        // differentiate between tag/script param
    }

    function bindStatement(node: Statement) {
        switch (node.subType) {
            case StatementType.tagCallStatement:
                bindList(node.callStatement!.args, node);
            case StatementType.expressionWrapper:
                bindNode(node.expr!, node);
            case StatementType.fromTag:
                // this is where we'd check against the cf tag meta json
                break;
            case StatementType.scriptSugaredTagCallStatement:
                // check attrs against cf tag meta here
                bindList(node.scriptSugaredTagStatement!.attrs, node);
        }
    }

    function bindBlock(node: Block) {
        switch (node.subType) {
            case BlockType.fromTag:
                // check against cf tag meta...
                bindList(node.stmtList, node);
                break;
            case BlockType.scriptSugaredTagCallBlock:
                // check against cf tag meta
                bindList(node.sugaredCallStatementAttrs!, node);
                bindList(node.stmtList, node);
                break;
            case BlockType.tagCallBlock:
                // check against cf tag meta
                // maybe push context to make sure children are correct
                bindList(node.tagCallStatementArgs!.args, node);
                bindList(node.stmtList, node);
                break;
            case BlockType.cLike:
                bindList(node.stmtList, node);
                break;
        }
    }



    return {
        bindProgram
    }
}