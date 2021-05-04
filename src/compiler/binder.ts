import { NodeWithScope, Variable, ArrowFunctionDefinition, BinaryOperator, Block, BlockType, CallArgument, FunctionDefinition, initContainer, Node, NodeType, Statement, StatementType, VariableDeclaration, mergeRanges, BinaryOpType, Scope, IndexedAccessType, ScopeDisplay, NodeId, IndexedAccess, IndexedAccessChainElement, SourceFile } from "./node";
import { getTriviallyComputableString, visit, BiMap } from "./utils";
import { Diagnostic } from "./parser";
import { Scanner, SourceRange } from "./scanner";

type InternedStringId = number;
const InternedStrings = new BiMap<number, string>();
let internStringId = 0;

function internString(s: string) {
    const internId = InternedStrings.getByValue(s);
    if (internId !== undefined) {
        return internId;
    }
    const freshInternId = internStringId++;
    InternedStrings.set(freshInternId, s);
    return freshInternId;
}

const staticCgiScope = (function () {
    const result = new Map<number, Variable>();
    // https://helpx.adobe.com/coldfusion/cfml-reference/reserved-words-and-variables/cgi-environment-cgi-scope-variables.html
    const staticNames = [
        "auth_password",
        "auth_type",
        "auth_user",
        "cert_cookie",
        "cert_flags",
        "cert_issuer",
        "cert_keysize",
        "cert_secretkeysize",
        "cert_serialnumber",
        "cert_server_issuer",
        "cert_server_subject",
        "cert_subject",
        "cf_template_path",
        "content_length",
        "content_type",
        "context_path",
        "gateway_interface",
        "https",
        "https_keysize",
        "https_secretkeysize",
        "https_server_issuer",
        "https_server_subject",
        "http_accept",
        "http_accept_encoding",
        "http_accept_language",
        "http_connection",
        "http_cookie",
        "http_host",
        "http_referer",
        "http_user_agent",
        "query_string",
        "remote_addr",
        "remote_host",
        "remote_user",
        "request_method",
        "script_name",
        "server_name",
        "server_port",
        "server_port_secure",
        "server_protocol",
        "server_software",
    ];
    for (const name of staticNames) {
        const internId = internString(name);
        result.set(
            internId, {
                type: "any",
                name: internId,
                final: false,
                var: false,
                initializer: undefined,
            })
    }
    return result;
})();

export function Binder() {
    let RootNode : NodeWithScope // @todo - make this a ProgramNode or something like that
    let currentContainer : NodeWithScope;
    let scanner : Scanner;
    let diagnostics: Diagnostic[];

    const NodeMap = new Map<number, Node>();

    function bind(sourceFile: SourceFile, scanner_: Scanner, diagnostics_: Diagnostic[]) {
        scanner = scanner_;
        diagnostics = diagnostics_;

        RootNode = sourceFile as NodeWithScope<SourceFile>;
        RootNode.containedScope = {
            container: null,
            cgi: staticCgiScope,
            variables: new Map(),
            url: new Map(),
            form: new Map(),
        };

        currentContainer = RootNode;
        bindList(sourceFile.content, sourceFile);
    }

    function getContainingFunction(node: Node) : NodeWithScope<FunctionDefinition | ArrowFunctionDefinition> | null {
        while (node.parent) {
            if (node.type === NodeType.functionDefinition || node.type === NodeType.arrowFunctionDefinition) {
                return node as NodeWithScope<FunctionDefinition | ArrowFunctionDefinition>;
            }
            node = node.parent;
        }
        return null;
    }

    function bindNode(node: Node, parent: Node) {
        NodeMap.set(node.nodeId, node);
        bindDirectTerminals(node);
        node.parent = parent;

        switch (node.type) {
            case NodeType.sourceFile:
                throw "Bind source files by binding its content";
            case NodeType.terminal:
            case NodeType.comment:
            case NodeType.textSpan:
                // ?
                return;
            case NodeType.hashWrappedExpr:
            case NodeType.parenthetical:
            case NodeType.tagAttribute:
            case NodeType.tag:
                return;
            case NodeType.callExpression:
                return;
            case NodeType.callArgument:
                bindCallArgument(node);
                return;
            case NodeType.unaryOperator:
                return;
            case NodeType.binaryOperator:
                bindBinaryOperator(node);
                return;
            case NodeType.conditional:
                return;
            case NodeType.variableDeclaration:
                bindDeclaration(node);
                return;
            case NodeType.statement:
                bindStatement(node);
                return;
            case NodeType.returnStatement:
                return;
            case NodeType.breakStatement:
                return;
            case NodeType.continueStatement:
                return;
            case NodeType.block:
                bindBlock(node);
                return;
            case NodeType.simpleStringLiteral:
            case NodeType.interpolatedStringLiteral:
            case NodeType.numericLiteral:
            case NodeType.booleanLiteral:
            case NodeType.identifier:
                return;
            case NodeType.indexedAccess:
                bindIndexedAccess(node);
                return;
            case NodeType.indexedAccessChainElement:
                bindIndexedAccessChainElement(node);
                return;
            case NodeType.functionParameter:
                return;
            case NodeType.functionDefinition:
            case NodeType.arrowFunctionDefinition:
                bindFunctionDefinition(node);
                return;
            case NodeType.dottedPath:
                return;
            case NodeType.switch:
                return;
            case NodeType.switchCase:
                return;
            case NodeType.do:
            case NodeType.while:
            case NodeType.ternary:
            case NodeType.for:
            case NodeType.structLiteral:
            case NodeType.structLiteralInitializerMember:
            case NodeType.arrayLiteral:
            case NodeType.arrayLiteralInitializerMember:
            case NodeType.try:
            case NodeType.catch:
            case NodeType.finally:
            case NodeType.importStatement:
            case NodeType.new:
                return;
            default:
                ((_:never) => { throw "Non-exhaustive case or unintentional fallthrough." })(node);
        }
    }

    function bindDirectTerminals(node: Node) {
        visit(node, function(visitedNode: Node | null | undefined) {
            if (visitedNode?.type === NodeType.terminal) {
                NodeMap.set(visitedNode.nodeId, visitedNode);
                visitedNode.parent = node;
            }
        });
    }

    function bindList(nodes: Node[], parent: Node) {
        for (let i = 0; i < nodes.length; ++i) {
            bindNode(nodes[i], parent);
        }
    }

    function bindIndexedAccess(node: IndexedAccess) {
        bindNode(node.root, node);
        let parent : Node = node;
        for (let i = 0; i < node.accessElements.length; i++) {
            bindNode(node.accessElements[i], parent);
            parent = node.accessElements[i];
        }
    }

    function bindIndexedAccessChainElement(node: IndexedAccessChainElement) {
        switch (node.accessType) {
            case IndexedAccessType.dot:          // fallthrough
            case IndexedAccessType.optionalDot:  // fallthrough
            case IndexedAccessType.optionalCall: // fallthrough
                // no-op, just terminals
                return;
            case IndexedAccessType.optionalBracket:
            case IndexedAccessType.bracket:
                bindNode(node.expr, node);
                return;
        }
    }

    function bindBinaryOperator(node: BinaryOperator) {
        bindNode(node.left, node);
        bindNode(node.right, node);

        if (node.optype === BinaryOpType.assign) {
            bindAssignment(node);
        }
    }

    function isBuiltinScopeName(s: string | undefined) : s is keyof Omit<ScopeDisplay, "container"> {
        switch (s) {
            case "url":
            case "form":
            case "cgi":
            case "variables":
            case "local":
                return true;
            default:
                return false;
        }
    }

    /**
     * support weakly binding identifiers, for the common scenario of
     * `x = y`, where x has never been seen before
     * we just want to know that some name is available in this scope
     */
    function weakBindIdentifierToScope(name: string, scope: Scope) : void {
        const internId = internString(name);
        if (scope.has(internId)) {
            return;
        }
        scope.set(internId, {
            type: "any",
            name: internId,
            final: false,
            var: false,
            initializer: undefined
        })
    }

    function bindAssignment(node: BinaryOperator) {
        const target = node.left;
        if (target.type === NodeType.indexedAccess) {
            const targetBaseName = getTriviallyComputableString(target.root)?.toLowerCase();

            if (!targetBaseName) {
                return;
            }

            // if it's a built in scope name, we'll try to write through to that scope on the root node
            // unless the scopename is local, in which case we'll try to write to the current local scope, if it exists
            if (isBuiltinScopeName(targetBaseName)) {
                // because it is a built-in scope name, we want to pull out the following name
                // so from `url.foo`, we want `foo`
                const firstAccessElement = target.accessElements[0];
                let firstAccessAsString : string | undefined = undefined;
                if (firstAccessElement.accessType === IndexedAccessType.dot) {
                    firstAccessAsString = firstAccessElement.dot.token.text.toLowerCase();
                }
                else if (firstAccessElement.accessType === IndexedAccessType.bracket) {
                    firstAccessAsString = getTriviallyComputableString(firstAccessElement.expr)
                }

                if (!firstAccessAsString) {
                    return;
                }

                if (targetBaseName === "local") {
                    if (currentContainer.containedScope.local) {
                        weakBindIdentifierToScope(firstAccessAsString, currentContainer.containedScope.local);
                    }
                    else {
                        // assigning to `local.x` in a non-local scope just binds the name `local` to the root variables scope
                        weakBindIdentifierToScope("local", RootNode.containedScope.variables!);
                    }
                }
                else {
                    if (targetBaseName in RootNode.containedScope) {
                        weakBindIdentifierToScope(firstAccessAsString, RootNode.containedScope[targetBaseName]!);
                    }
                }
            }
            // not a built-in scope name, just use target base name as identifier, and write to the local scope (if it exists), falling
            // back to the global variables scope
            else {
                if (currentContainer.containedScope.local) {
                    weakBindIdentifierToScope(targetBaseName, currentContainer.containedScope.local);
                }
                else {
                    weakBindIdentifierToScope(targetBaseName, RootNode.containedScope.variables!);
                }
            }
        }
        else {
            const targetBaseName = getTriviallyComputableString(target)?.toLowerCase();
            if (targetBaseName) {
                if (currentContainer.containedScope.local) {
                    weakBindIdentifierToScope(targetBaseName, currentContainer.containedScope.local);
                }
                else {
                    weakBindIdentifierToScope(targetBaseName, RootNode.containedScope.variables!);
                }
            }
        }
    }

    function bindFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition) {
        bindList(node.params, node);
        initContainer(node, currentContainer);

        if (node.type === NodeType.functionDefinition) {
            // this is a non-arrow function definition
            // it should have a name unless the source text is invalid, in which case it would be "" (the empty string)
            // tag functions and named script functions like `function foo() {}` are hoisted
            if (node.canonicalName) {
                const internId = internString(node.canonicalName);
                RootNode.containedScope.variables!.set(
                    internId, {
                        type: {
                            params: node.params,
                            return: "any"
                        },
                        name: internId,
                        final: false,
                        var: false,
                        initializer: undefined
                    });
            }
        }

        const savedContainer = currentContainer;
        currentContainer = node;

        const paramIdentifiers = new Map<InternedStringId, Variable>();
        for (let i = 0; i < node.params.length; i++) {
            const staticallyKnownName = node.params[i].canonicalName;
            if (staticallyKnownName) {
                const internId = internString(staticallyKnownName)
                paramIdentifiers.set(
                    internId, {
                        type: "any", // we could pull info from the type specifier if there is one
                        name: internId,
                        final: false,
                        var: false,
                        initializer: node.params[i].defaultValue ?? undefined
                    });

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

    function bindDeclaration(node: VariableDeclaration) {
        let identifierBaseName : string | undefined;
        if (node.identifier.source.type === NodeType.indexedAccess) {
            identifierBaseName = getTriviallyComputableString(node.identifier.source.root)?.toLowerCase();
        }
        else {
            identifierBaseName = getTriviallyComputableString(node.identifier)?.toLowerCase();
        }

        if (identifierBaseName) {
            const internId = internString(identifierBaseName);
            if (currentContainer.containedScope.local) {
                (<Map<number, Variable>>currentContainer.containedScope.local).set(
                    internId, {
                        type: "any",
                        name: internId,
                        final: !!node.finalModifier,
                        var: !!node.varModifier,
                        initializer: node.expr ?? undefined
                    });
            }
            else {
                // there is no local scope, so we must be at top-level scope
                errorAtRange(mergeRanges(node.finalModifier, node.varModifier, node.identifier), "Local variables may not be declared at top-level scope.");
            }
        
            const containingFunction = getContainingFunction(node);
            if (containingFunction) {
                // if name is same as a parameter definition's name, emit an error,
                // e.g, 
                // function foo(bar) { var bar = 42; }
                // is an error: "bar is already defined in argument scope"
                if (containingFunction.containedScope.arguments!.has(internId)) {
                    errorAtRange(mergeRanges(node.finalModifier, node.varModifier, node.identifier), `'${identifierBaseName}' is already defined in argument scope.`);
                }
            }
        }

    }

    function bindCallArgument(node: CallArgument) {
        if (node.name) bindNode(node.name, node);
        bindNode(node.expr, node);
    }

    function bindStatement(node: Statement) {
        switch (node.subType) {
            case StatementType.scriptTagCallStatement:
                bindList(node.callStatement!.args, node);
                return;
            case StatementType.expressionWrapper:
                bindNode(node.expr!, node);
                return;
            case StatementType.fromTag:
                // this is where we'd check against the cf tag meta json
                break;
            case StatementType.scriptSugaredTagCallStatement:
                // check attrs against cf tag meta here
                bindList(node.scriptSugaredTagStatement!.attrs, node);
                return;
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
        bind,
        setDebug,
        NodeMap: <ReadonlyMap<NodeId, Node>>NodeMap,
    }

    return self;
}

let debugBinder = false;
