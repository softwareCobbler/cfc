import { NodeWithScope, ArrowFunctionDefinition, BinaryOperator, Block, BlockType, CallArgument, FunctionDefinition, Identifier, initContainer, NilTerminal, Node, NodeType, Statement, StatementType, VariableDeclaration, mergeRanges } from "./node";
import { getTriviallyComputableString } from "./utils";
import { Diagnostic } from "./parser";
import { Scanner, SourceRange } from "./scanner";

export function Binder() {
    let RootNode : Node; // @todo - make this a ProgramNode or something like that
    let currentContainer : NodeWithScope;
    let scanner: Scanner; // needed to get source positions translated from index -> utf16pos
    let diagnostics: Diagnostic[];

    function bindProgram(node_: Node[], scanner_: Scanner, diagnostics_: Diagnostic[]) {
        scanner = scanner_;
        diagnostics = diagnostics_;

        RootNode = NilTerminal;
        initContainer(RootNode, /*parent*/ null);
        currentContainer = RootNode as NodeWithScope;
        bindList(node_, RootNode);
    }

    function getContainingFunction(node: Node) : FunctionDefinition | ArrowFunctionDefinition | null {
        while (node.parent) {
            if (node.type === NodeType.functionDefinition || node.type === NodeType.arrowFunctionDefinition) {
                return node;
            }
            node = node.parent;
        }
        return null;
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
            case NodeType.variableDeclaration:
                bindDeclaration(node);
                break;
            case NodeType.block:
                bindBlock(node);
                break;
            case NodeType.arrayLiteral:
            case NodeType.structLiteral:
                break;
            case NodeType.callArgument:
                bindCallArgument(node);
                break;
            case NodeType.functionDefinition:
            case NodeType.arrowFunctionDefinition:
                bindFunctionDefinition(node);
                break;
            case NodeType.textSpan:
            case NodeType.comment:
                return;
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

    function bindFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition) {
        bindList(node.params, node);
        initContainer(node, currentContainer);
        const savedContainer = currentContainer;
        currentContainer = node;

        const paramIdentifiers = new Map<string, Identifier>();
        for (let i = 0; i < node.params.length; i++) {
            const staticallyKnownName = getTriviallyComputableString(node.params[i].identifier)?.toLowerCase(); // @fixme: just use canonicalName prop?
            if (staticallyKnownName) {
                paramIdentifiers.set(staticallyKnownName, node.params[i].identifier);
            }
        }

        node.containedScope.arguments = paramIdentifiers;
        node.containedScope.local = new Map();

        bindNode(node.body, node);
        currentContainer = savedContainer;
    }

    function errorAtSpan(fromInclusive: number, toExclusive: number, msg: string) {
        const freshDiagnostic : Diagnostic = {fromInclusive, toExclusive, msg };

        if (debugBinder) {
            const debugFrom = scanner.getAnnotatedChar(freshDiagnostic.fromInclusive);
            const debugTo = scanner.getAnnotatedChar(freshDiagnostic.toExclusive);
            // bump 0-offsetted info to editor-centric 1-offset
            freshDiagnostic.__debug_from_line = debugFrom.line+1;
            freshDiagnostic.__debug_from_col = debugFrom.col+1;
            freshDiagnostic.__debug_to_line = debugTo.line+1;
            freshDiagnostic.__debug_to_col = debugTo.col+1;
        }

        diagnostics.push(freshDiagnostic);
    }

    function errorAtRange(range: SourceRange, msg: string) : void {
        errorAtSpan(range.fromInclusive, range.toExclusive, msg);
    }

    function errorAtNode(node: Node, msg: string) : void {
        errorAtSpan(node.range.fromInclusive, node.range.toExclusive, msg);
    }
    errorAtNode; // unused

    function bindDeclaration(node: VariableDeclaration) {
        let identifierBaseName : string | undefined;
        if (node.identifier.source.type === NodeType.indexedAccess) {
            identifierBaseName = getTriviallyComputableString(node.identifier.source.root)?.toLowerCase();
        }
        else {
            identifierBaseName = getTriviallyComputableString(node.identifier)?.toLowerCase();
        }

        if (identifierBaseName) {
            if (currentContainer.containedScope.local) {
                (<Map<string, Identifier>>currentContainer.containedScope.local).set(identifierBaseName, node.identifier);
            }
            else {
                errorAtRange(mergeRanges(node.finalModifier, node.varModifier, node.identifier), "Local variables may not be declared at top-level scope.");
            }
        
            const containingFunction = getContainingFunction(node);
            if (containingFunction) {
                // if name is same as a parameter definition's name, emit an error,
                // e.g, 
                // function foo(bar) { var bar = 42; }
                // is an error: "bar is already defined in argument scope"
                if (containingFunction.containedScope!.arguments!.has(identifierBaseName)) {
                    errorAtRange(mergeRanges(node.finalModifier, node.varModifier, node.identifier), `'${identifierBaseName}' is already defined in argument scope.`);
                }
            }
        }

    }

    function bindCallArgument(node: CallArgument) {
        if (node.name) bindNode(node.name, node);
        bindNode(node.expr, node);
    }

   // function bindFunctionParameter(node: FunctionParameter) {
        // differentiate between tag/script param
  //  }

    function bindStatement(node: Statement) {
        switch (node.subType) {
            case StatementType.scriptTagCallStatement:
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

    function setDebug(isDebug: boolean) {
        debugBinder = isDebug;
        return self;
    }

    const self = {
        bindProgram,
        setDebug
    }

    return self;
}

let debugBinder = false;
