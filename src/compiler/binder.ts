import { NodeWithScope, Variable, ArrowFunctionDefinition, BinaryOperator, Block, BlockType, CallArgument, FunctionDefinition, Node, NodeType, Statement, StatementType, VariableDeclaration, mergeRanges, BinaryOpType, Scope, IndexedAccessType, ScopeDisplay, NodeId, IndexedAccess, IndexedAccessChainElement, SourceFile, CfTag, CallExpression, UnaryOperator, Conditional, ReturnStatement, BreakStatement, ContinueStatement, FunctionParameter, Switch, SwitchCase, Do, While, Ternary, For, ForSubType, StructLiteral, StructLiteralInitializerMember, ArrayLiteral, ArrayLiteralInitializerMember, Try, Catch, Finally, ImportStatement, New, SimpleStringLiteral, InterpolatedStringLiteral } from "./node";
import { getTriviallyComputableString, visit, BiMap, getAttributeValue } from "./utils";
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
    let RootNode : NodeWithScope;
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

    function bindNode(node: Node | null | undefined, parent: Node) {
        if (!node) return;

        NodeMap.set(node.nodeId, node);
        bindDirectTerminals(node);
        node.parent = parent;

        switch (node.type) {
            case NodeType.sourceFile:
                throw "Bind source files by binding its content";
            case NodeType.terminal: // fallthrough
            case NodeType.comment:  // fallthrough
            case NodeType.textSpan:
                node.parent = node;
                return;
            case NodeType.hashWrappedExpr: // fallthrough
            case NodeType.parenthetical:   // fallthrough
            case NodeType.tagAttribute:
                bindNode(node.expr, node);
                return;
            case NodeType.tag:
                bindTag(node);
                return;
            case NodeType.callExpression:
                bindCallExpression(node);
                return;
            case NodeType.callArgument:
                bindCallArgument(node);
                return;
            case NodeType.unaryOperator:
                bindUnaryOperator(node);
                return;
            case NodeType.binaryOperator:
                bindBinaryOperator(node);
                return;
            case NodeType.conditional:
                bindConditional(node);
                return;
            case NodeType.variableDeclaration:
                bindDeclaration(node);
                return;
            case NodeType.statement:
                bindStatement(node);
                return;
            case NodeType.returnStatement:
                bindReturnStatement(node);
                return;
            case NodeType.breakStatement:
                bindBreakStatement(node);
                return;
            case NodeType.continueStatement:
                bindContinueStatement(node);
                return;
            case NodeType.block:
                bindBlock(node);
                return;
            case NodeType.simpleStringLiteral:
                bindSimpleStringLiteral(node);
                return;
            case NodeType.interpolatedStringLiteral:
                bindInterpolatedStringLiteral(node);
                return;
            case NodeType.numericLiteral: // fallthrough
            case NodeType.booleanLiteral:
                // no-op, just a terminal
                return;
            case NodeType.identifier:
                bindNode(node.source, node);
                return;
            case NodeType.indexedAccess:
                bindIndexedAccess(node);
                return;
            case NodeType.indexedAccessChainElement:
                bindIndexedAccessChainElement(node);
                return;
            case NodeType.functionParameter:
                bindFunctionParameter(node);
                return;
            case NodeType.functionDefinition: // fallthrough
            case NodeType.arrowFunctionDefinition:
                bindFunctionDefinition(node);
                return;
            case NodeType.dottedPath:
                // ?
                return;
            case NodeType.switch:
                bindSwitch(node);
                return;
            case NodeType.switchCase:
                bindSwitchCase(node);
                return;
            case NodeType.do:
                bindDo(node);
                return;
            case NodeType.while:
                bindWhile(node);
                return;
            case NodeType.ternary:
                bindTernary(node);
                return;
            case NodeType.for:
                bindFor(node);
                return;
            case NodeType.structLiteral:
                bindStructLiteral(node);
                return;
            case NodeType.structLiteralInitializerMember:
                bindStructLiteralInitializerMember(node);
                return;
            case NodeType.arrayLiteral:
                bindArrayLiteral(node);
                return;
            case NodeType.arrayLiteralInitializerMember:
                bindArrayLiteralInitializerMember(node);
                return;
            case NodeType.try:
                bindTry(node);
                return;
            case NodeType.catch:
                bindCatch(node);
                return;
            case NodeType.finally:
                bindFinally(node);
                return;
            case NodeType.importStatement:
                bindImportStatement(node);
                return;
            case NodeType.new:
                bindNew(node);
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

    function bindTag(node: CfTag) {
        if (node.which === CfTag.Which.end) {
            // all terminals, already bound
            return;
        }
        
        switch (node.tagType) {
            case CfTag.TagType.common:
                bindList(node.attrs, node);
                return;
            case CfTag.TagType.scriptLike:
                bindNode(node.expr, node);
                return;
            case CfTag.TagType.script:
            case CfTag.TagType.comment:
            case CfTag.TagType.text:
                throw "These should be dealt with prior to binding.";
            default:
                ((_:never) => { throw "Non-exhaustive case or unintentional fallthrough." })(node);
        }
    }

    function bindCallExpression(node: CallExpression) {
        bindNode(node.left, node);
        bindList(node.args, node);
    }

    function bindCallArgument(node: CallArgument) {
        if (node.name) bindNode(node.name, node);
        bindNode(node.expr, node);
    }

    function bindUnaryOperator(node: UnaryOperator) {
        bindNode(node.expr, node);
    }

    function bindBinaryOperator(node: BinaryOperator) {
        bindNode(node.left, node);
        bindNode(node.right, node);

        if (node.optype === BinaryOpType.assign) {
            bindAssignment(node);
        }
    }

    function bindConditional(node: Conditional) {
        if (node.tagOrigin.startTag) {
            bindNode(node.tagOrigin.startTag, node);
            bindNode(node.consequent, node);
            bindNode(node.alternative, node);
            bindNode(node.tagOrigin.endTag, node);
        }
        else {
            bindNode(node.expr, node);
            bindNode(node.consequent, node);
            bindNode(node.alternative, node);
        }
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
        
            const enclosingFunction = getEnclosingFunction(node);
            if (enclosingFunction) {
                // if name is same as a parameter definition's name, emit an error,
                // e.g, 
                // function foo(bar) { var bar = 42; }
                // is an error: "bar is already defined in argument scope"
                if (enclosingFunction.containedScope.arguments.has(internId)) {
                    errorAtRange(mergeRanges(node.finalModifier, node.varModifier, node.identifier), `'${identifierBaseName}' is already defined in argument scope.`);
                }
            }
        }
    }

    function bindStatement(node: Statement) {
        switch (node.subType) {
            case StatementType.scriptTagCallStatement:
                // e.g, cftransaction(action="rollback");
                // bind parens specially; callStatement is not a Node so it wasn't considered
                // can probably make callStatement a Parenthetical<CallArgument> or something
                bindNode(node.callStatement!.leftParen, node);
                bindList(node.callStatement!.args, node);
                bindNode(node.callStatement!.rightParen, node);
                return;
            case StatementType.expressionWrapper:
                // if it is a tagOrigin node, it is a <cfset> tag
                if (node.tagOrigin.startTag) {
                    bindNode(node.tagOrigin.startTag, node);
                    return;
                }
                bindNode(node.expr, node);
                return;
            case StatementType.fromTag:
                bindNode(node.tagOrigin.startTag, node);
                break;
            case StatementType.scriptSugaredTagCallStatement:
                // check attrs against cf tag meta here
                bindNode(node.expr, node);
                bindList(node.scriptSugaredTagStatement!.attrs, node);
                return;
        }
    }

    function bindReturnStatement(node: ReturnStatement) {
        if (node.tagOrigin.startTag) {
            bindNode(node.tagOrigin.startTag, node);
            return;
        }
        bindNode(node.expr, node);
    }

    function bindBreakStatement(node: BreakStatement) {
        if (node.tagOrigin.startTag) {
            bindNode(node.tagOrigin.startTag, node);
            return;
        }
        // otherwise, all terminals, no work to do
    }

    function bindContinueStatement(node: ContinueStatement) {
        if (node.tagOrigin.startTag) {
            bindNode(node.tagOrigin.startTag, node);
            return;
        }
        // otherwise all terminals, no work to do
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

    function bindSimpleStringLiteral(node: SimpleStringLiteral) {
        bindNode(node.textSpan, node);
    }

    function bindInterpolatedStringLiteral(node: InterpolatedStringLiteral) {
        bindList(node.elements, node);
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

    function bindFunctionParameter(node: FunctionParameter) {
        if (node.tagOrigin.startTag) {
            bindNode(node.tagOrigin.startTag, node);
            const nameAttr = getAttributeValue((<CfTag.Common>node.tagOrigin.startTag).attrs, "name");
            if (nameAttr) {
                const name = getTriviallyComputableString(nameAttr)?.toLowerCase();
                if (name) weakBindIdentifierToScope(name, currentContainer.containedScope.arguments!);
            }
            return;
        }

        weakBindIdentifierToScope(node.canonicalName!, currentContainer.containedScope.arguments!);
        bindNode(node.javaLikeTypename, node);
        bindNode(node.identifier, node);
        bindNode(node.defaultValue, node);
    }

    function getEnclosingFunction(node: Node | null) : NodeWithScope<FunctionDefinition | ArrowFunctionDefinition, "arguments"> | undefined {
        while (node) {
            if (node.type === NodeType.functionDefinition || node.type === NodeType.arrowFunctionDefinition) {
                return node as NodeWithScope<FunctionDefinition | ArrowFunctionDefinition, "arguments">;
            }
            node = node.parent;
        }
        return undefined;
    }

    function getAncestorOfType(node: Node | null, nodeType: NodeType) : Node | undefined {
        while (node) {
            if (node.type === nodeType) {
                return node;
            }
        }
        return undefined;
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
        node.containedScope = {
            container: currentContainer,
            arguments: new Map()
        };

        currentContainer = node as NodeWithScope;

        bindList(node.params, node);

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
        currentContainer = node.containedScope.container! as NodeWithScope;
    }

    function bindSwitch(node: Switch) {
        if (node.tagOrigin.startTag) {
            bindNode(node.tagOrigin.startTag, node);
            bindList(node.cases, node);
            bindNode(node.tagOrigin.endTag, node);
            return;
        }
        bindNode(node.expr, node);
        bindList(node.cases, node);
    }

    function bindSwitchCase(node: SwitchCase) {
        if (node.tagOrigin.startTag) {
            bindNode(node.tagOrigin.startTag, node);
            bindList(node.statements, node);
            bindNode(node.tagOrigin.endTag, node);
            return;
        }
        bindNode(node.expr, node);
        bindList(node.statements, node);
    }

    function bindDo(node: Do) {
        bindNode(node.body, node); // do (body) while (expr)
        bindNode(node.expr, node);
    }

    function bindWhile(node: While) {
        bindNode(node.expr, node); // while (expr) (body)
        bindNode(node.body, node);
    }

    function bindTernary(node: Ternary) {
        bindNode(node.expr, node);
        bindNode(node.ifTrue, node);
        bindNode(node.ifFalse, node);
    }

    function bindFor(node: For) {
        if (node.subType === ForSubType.forIn) {
            bindNode(node.forIn!.init, node);
            bindNode(node.forIn!.inToken, node);
            bindNode(node.forIn!.expr, node);
            bindNode(node.body, node);
            return;
        }
        bindNode(node.for!.initExpr, node);
        bindNode(node.for!.semi1, node);
        bindNode(node.for!.conditionExpr, node);
        bindNode(node.for!.semi2, node);
        bindNode(node.for!.incrementExpr, node);
        bindNode(node.body, node);
    }

    function bindStructLiteral(node: StructLiteral) {
        bindList(node.members, node);
    }

    function bindStructLiteralInitializerMember(node: StructLiteralInitializerMember) {
        bindNode(node.key, node);
        bindNode(node.expr, node);
    }

    function bindArrayLiteral(node: ArrayLiteral) {
        bindList(node.members, node);
    }

    function bindArrayLiteralInitializerMember(node: ArrayLiteralInitializerMember) {
        bindNode(node.expr, node);
    }

    function bindTry(node: Try) {
        if (node.tagOrigin.startTag) {
            bindNode(node.tagOrigin.startTag, node);
            bindList(node.body, node);
            bindNode(node.tagOrigin.endTag, node);
            return;
        }
        bindList(node.body, node);
        bindList(node.catchBlocks, node);
        bindNode(node.finallyBlock, node);
    }

    function bindCatch(node: Catch) {
        if (node.tagOrigin.startTag) {
            bindNode(node.tagOrigin.startTag, node);
            bindList(node.body, node);
            bindNode(node.tagOrigin.endTag, node);
            if (!getAncestorOfType(node, NodeType.try)) {
                errorAtRange(node.tagOrigin.startTag.range, "A catch tag must be contained within a try tag-block.");
            }
            return;
        }

        bindNode(node.exceptionType, node);
        bindNode(node.exceptionBinding, node);
        bindList(node.body, node);
    }

    function bindFinally(node: Finally) {
        if (node.tagOrigin.startTag) {
            bindNode(node.tagOrigin.startTag, node);
            bindList(node.body, node);
            bindNode(node.tagOrigin.endTag, node);
            if (!getAncestorOfType(node, NodeType.try)) {
                errorAtRange(node.tagOrigin.startTag.range, "A finally tag must be contained within a try tag-block.");
            }
            return;
        }

        bindList(node.body, node);
    }

    function bindImportStatement(node: ImportStatement) {
        bindNode(node.path, node);
    }

    function bindNew(node: New) {
        bindNode(node.callExpr, node);
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
