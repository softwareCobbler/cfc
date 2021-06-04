import { ArrowFunctionDefinition, BinaryOperator, Block, BlockType, CallArgument, FunctionDefinition, Node, NodeType, Statement, StatementType, VariableDeclaration, mergeRanges, BinaryOpType, IndexedAccessType, ScopeDisplay, NodeId, IndexedAccess, IndexedAccessChainElement, SourceFile, CfTag, CallExpression, UnaryOperator, Conditional, ReturnStatement, BreakStatement, ContinueStatement, FunctionParameter, Switch, SwitchCase, Do, While, Ternary, For, ForSubType, StructLiteral, StructLiteralInitializerMember, ArrayLiteral, ArrayLiteralInitializerMember, Try, Catch, Finally, ImportStatement, New, SimpleStringLiteral, InterpolatedStringLiteral, Identifier, isStaticallyKnownScopeName, StructLiteralInitializerMemberSubtype, SliceExpression, NodeWithScope, Flow, freshFlow, ReachableFlow, FlowType, Script, Tag, ConditionalSubtype } from "./node";
import { getTriviallyComputableString, visit, getAttributeValue } from "./utils";
import { Diagnostic } from "./parser";
import { CfFileType, Scanner, SourceRange } from "./scanner";
import { cfFunctionSignature, cfStruct, SyntheticType, Type } from "./types";

const staticCgiScope : cfStruct = (function () {
    // https://helpx.adobe.com/coldfusion/cfml-reference/reserved-words-and-variables/cgi-environment-cgi-scope-variables.html
    const staticStruct = new Map([
        ["auth_password",        SyntheticType.string],
        ["auth_type",            SyntheticType.string],
        ["auth_user",            SyntheticType.string],
        ["cert_cookie",          SyntheticType.string],
        ["cert_flags",           SyntheticType.string],
        ["cert_issuer",          SyntheticType.string],
        ["cert_keysize",         SyntheticType.string],
        ["cert_secretkeysize",   SyntheticType.string],
        ["cert_serialnumber",    SyntheticType.string],
        ["cert_server_issuer",   SyntheticType.string],
        ["cert_server_subject",  SyntheticType.string],
        ["cert_subject",         SyntheticType.string],
        ["cf_template_path",     SyntheticType.string],
        ["content_length",       SyntheticType.string],
        ["content_type",         SyntheticType.string],
        ["context_path",         SyntheticType.string],
        ["gateway_interface",    SyntheticType.string],
        ["https",                SyntheticType.string],
        ["https_keysize",        SyntheticType.string],
        ["https_secretkeysize",  SyntheticType.string],
        ["https_server_issuer",  SyntheticType.string],
        ["https_server_subject", SyntheticType.string],
        ["http_accept",          SyntheticType.string],
        ["http_accept_encoding", SyntheticType.string],
        ["http_accept_language", SyntheticType.string],
        ["http_connection",      SyntheticType.string],
        ["http_cookie",          SyntheticType.string],
        ["http_host",            SyntheticType.string],
        ["http_referer",         SyntheticType.string],
        ["http_user_agent",      SyntheticType.string],
        ["query_string",         SyntheticType.string],
        ["remote_addr",          SyntheticType.string],
        ["remote_host",          SyntheticType.string],
        ["remote_user",          SyntheticType.string],
        ["request_method",       SyntheticType.string],
        ["script_name",          SyntheticType.string],
        ["server_name",          SyntheticType.string],
        ["server_port",          SyntheticType.string],
        ["server_port_secure",   SyntheticType.string],
        ["server_protocol",      SyntheticType.string],
        ["server_software",      SyntheticType.string],
    ]);
    return SyntheticType.struct(staticStruct);
})();

export function Binder() {
    let RootNode : NodeWithScope<SourceFile>;
    let currentContainer : NodeWithScope;
    let scanner : Scanner;
    let diagnostics: Diagnostic[];

    let currentFlow : Flow;
    let detachedClosureFlows : Flow[] = [];

    let nodeMap = new Map<NodeId, Node>();

    function bind(sourceFile: SourceFile, scanner_: Scanner, diagnostics_: Diagnostic[]) {
        nodeMap = new Map<NodeId, Node>();
        scanner = scanner_;
        diagnostics = diagnostics_;

        RootNode = sourceFile as NodeWithScope<SourceFile>;
        RootNode.containedScope = {
            container: null,
            typedefs: new Map<string, Type>(),
            cgi: staticCgiScope,
            variables: SyntheticType.struct(),
            url: SyntheticType.struct(),
            form: SyntheticType.struct(),
        };

        if (sourceFile.cfFileType === CfFileType.cfc) {
            RootNode.containedScope.this = SyntheticType.struct();
        }

        currentFlow = freshFlow([], FlowType.default);
        bindFlowToNode(currentFlow, RootNode);

        if (sourceFile.cfFileType === CfFileType.cfc) {
            RootNode.containedScope.this = SyntheticType.struct();
        }

        currentContainer = RootNode;
        bindListFunctionsFirst(sourceFile.content, sourceFile);
        connectDetachedClosureFlowsToCurrentFlow();
    }

    function newFlowGraphRootedAt(node: Node) : Flow {
        const flow = freshFlow([], FlowType.default);
        bindFlowToNode(flow, node);
        return flow;
    }

    function bindFlowToNode(flow: Flow, node: Node) : void {
        flow.node = node;
        node.flow = flow;
    }

    function isBoundFlow(flow: Flow) : flow is ReachableFlow {
        return !!flow.node;
    }

    function extendCurrentFlowToNode(node: Node, flowType: FlowType = FlowType.default) : void {
        // if the current flow is already bound to a node, make a new flow and connect them
        if (isBoundFlow(currentFlow)) {
            const flow = freshFlow(currentFlow, flowType);
            bindFlowToNode(flow, node);
            currentFlow.successor = flow;
            currentFlow = flow;
        }
        else {
            // otherwise, the current flow is "dangling", just bind the dangling flow to the current node
            bindFlowToNode(currentFlow, node);
        }
    }

    /**
     *    <0>       <- current flow
     *  /     \
     * a<1> = b<2>; <- assignment, with new flows <1> and <2>; program continues with <1> as predecessor
     * |
     * <program continues>
     */
    function bindAssignmentFlow(assignmentTarget: Node, expr: Node) {
        bindFlowToNode(freshFlow(currentFlow, FlowType.default), expr);
        extendCurrentFlowToNode(assignmentTarget, FlowType.assignment);
    }

    /*function dangleFreshFlowFromCurrentFlow(flowType: FlowType = FlowType.default) {
        if (!isBoundFlow(currentFlow)) {
            throw "dangling flow must dangle from a bound flow";
        }
        const flow = freshFlow(currentFlow, flowType);
        currentFlow.successor = flow;
        currentFlow = flow;
    }*/

    // closures have their predecessor flows connected to the final flow node of their containing scope
    // there is no flow node with a successor of a closure flow, it is not possible to "flow into" a closure
    // we can only use the flow to walk out of a closure and into the surrounding environment
    function connectDetachedClosureFlowsToCurrentFlow() {
        for (const flow of detachedClosureFlows) {
            flow.predecessor.push(currentFlow);
        }
        detachedClosureFlows = [];
    }

    function bindNode(node: Node | null | undefined, parent: Node) {
        if (!node) return;

        nodeMap.set(node.nodeId, node);
        bindDirectTerminals(node);
        node.parent = parent;

        switch (node.kind) {
            case NodeType.sourceFile:
                throw "Bind source files by binding its content";
            case NodeType.comment:
                if (node.typedefs) {
                    bindList(node.typedefs, node);
                }
                return;
            case NodeType.type:
                bindType(node);
                return;
            case NodeType.textSpan:
                return;
            case NodeType.terminal:
                bindList(node.trivia, node);
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
                bindVariableDeclaration(node);
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
                bindIdentifier(node);
                return;
            case NodeType.indexedAccess:
                bindIndexedAccess(node);
                return;
            case NodeType.indexedAccessChainElement:
                bindIndexedAccessChainElement(node);
                return;
            case NodeType.sliceExpression:
                bindSliceExpression(node);
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
            if (visitedNode?.kind === NodeType.terminal) {
                nodeMap.set(visitedNode.nodeId, visitedNode);
                visitedNode.parent = node;
                bindList(visitedNode.trivia, visitedNode);
            }
        });
    }

    function bindList(nodes: Node[], parent: Node) {
        for (let i = 0; i < nodes.length; ++i) {
            bindNode(nodes[i], parent);
        }
    }

    function bindListFunctionsFirst(nodes: Node[], parent: Node) {
        bindList(nodes.filter(node => node.kind === NodeType.functionDefinition), parent);
        bindList(nodes.filter(node => node.kind !== NodeType.functionDefinition), parent);
    }

    function bindType(node: Type) {
        // types always have names here?
        // we get them from `@type x = ` so presumably always...
        // also `@declare function foo` and possibly `@declare global <identifier-name> : type`
        currentContainer.containedScope.typedefs.set(node.name!, node);
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
                // this seems like a bit of a kludge:
                // the script tag's trailing ">" in "<cfscript>" has trivia bound to it
                // we would like the trivia's parent to be the block the <cfscript> tag represents,
                // not the <cfscript> tag itself
                // then when doing lookups, a cursor in the the first part of a cfscript block naturally climbs to a block
                // rather than a tag
                bindList(node.tagEnd.trivia, node.parent!);
                return;
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
        if (node.optype === BinaryOpType.assign) {
            bindAssignment(node);
            return;
        }

        const savedFlow = currentFlow;
        extendCurrentFlowToNode(node.left);
        bindNode(node.left, node);

        currentFlow = savedFlow;
        extendCurrentFlowToNode(node.right);
        bindNode(node.right, node);

        currentFlow = savedFlow;
    }

    function bindConditional(node: Conditional) {
        if (node.subType === ConditionalSubtype.if || node.subType === ConditionalSubtype.elseif) {
            let expr : Node;
            if (node.fromTag) {
                expr = (node.tagOrigin.startTag as CfTag.ScriptLike).expr!;
            }
            else {
                expr = node.expr!;
            }

            const savedFlow = currentFlow;
            extendCurrentFlowToNode(expr);
            bindNode(expr, node);
            extendCurrentFlowToNode(node.consequent);
            bindNode(node.consequent, node);
            if (node.alternative) {
                currentFlow = savedFlow;
                extendCurrentFlowToNode(node.alternative);
                bindNode(node.alternative, node);
            }
            currentFlow = savedFlow;
        }
        else {
            const savedFlow = currentFlow;
            bindNode(node.consequent, node);
            currentFlow = savedFlow;
        }
    }

    // fixme: the following is not true, in the case of `for (var x in y)`
    // all declarations should be of the s-expr form (decl (binary-op<assignment>))
    // that is, VariableDeclarations just wrap assignment nodes (which are themselves just binary operators with '=' as the operator)
    function bindVariableDeclaration(node: VariableDeclaration) {
        if (node.expr.kind === NodeType.binaryOperator) {
            bindAssignmentFlow(node.expr.left, node.expr.right);
            bindNode(node.expr.left, node);

            const savedFlow = currentFlow;
            currentFlow = node.expr.right.flow!;
            bindNode(node.expr.right, node);
            currentFlow = savedFlow;
        }

        let identifierBaseName : string | undefined = undefined;
        if (node.expr.kind === NodeType.binaryOperator && node.expr.optype === BinaryOpType.assign) {
            if (node.expr.left.kind === NodeType.indexedAccess) {
                identifierBaseName = getTriviallyComputableString(node.expr.left.root)?.toLowerCase();
            }
            else {
                identifierBaseName = getTriviallyComputableString(node.expr.left)?.toLowerCase();
            }
        }

        // make sure we got a useable name
        if (!identifierBaseName) {
            return;
        }

        if (isStaticallyKnownScopeName(identifierBaseName)) {
            if (node.varModifier) {
                // we might have to consider our current container, like are we after a <cffile> tag? Does that matter?
                errorAtRange(mergeRanges(node.finalModifier, node.varModifier, (<BinaryOperator>node.expr).left), "Variable declaration shadows built-in scope `" + identifierBaseName + "`");
            }

            // if we got `url.foo`, put `foo` into the `url` scope
            // only descend the one child level, `url.foo.bar` still only puts `foo` into `url`
            if ((<BinaryOperator>node.expr).left.kind === NodeType.indexedAccess) {
                const indexedAccess = (<BinaryOperator>node.expr).left as IndexedAccess;
                const element = indexedAccess.accessElements[0];

                let accessName : string | undefined;
                if (element?.accessType === IndexedAccessType.dot) {
                    accessName = getTriviallyComputableString((element.property));
                }
                else if (element?.accessType === IndexedAccessType.bracket) {
                    accessName = getTriviallyComputableString((element.expr));
                }
                accessName;
/*
                if (accessName) {
                    RootNode.containedScope[identifierBaseName]!.set(
                        accessName, {
                            type: cfAny(),
                            name: accessName,
                            final: true,
                            var: false,
                            target: (<BinaryOperator>node.expr).right
                        });
                }*/
            }

            return;
        }

        if (node.finalModifier || node.varModifier) {
            if (currentContainer.containedScope.local) {
                currentContainer.containedScope.local.membersMap.set(identifierBaseName, node.typeAnnotation || SyntheticType.any);
            }
            else {
                // there is no local scope, so we must be at top-level scope
                errorAtRange(mergeRanges(node.finalModifier, node.varModifier, (<BinaryOperator>node.expr)?.left), "Local variables may not be declared at top-level scope.");
            }
        
            const enclosingFunction = getEnclosingFunction(node);
            if (enclosingFunction) {
                // if name is same as a parameter definition's name, emit an error,
                // e.g, 
                // function foo(bar) { var bar = 42; }
                // is an error: "bar is already defined in argument scope"
                if (enclosingFunction.containedScope.arguments.caselessMembersMap.has(identifierBaseName)) {
                    errorAtRange(mergeRanges(node.finalModifier, node.varModifier, node.expr), `'${identifierBaseName}' is already defined in argument scope.`);
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
                extendCurrentFlowToNode(node);
                bindNode(node.callStatement!.leftParen, node);
                bindList(node.callStatement!.args, node);
                bindNode(node.callStatement!.rightParen, node);
                return;
            case StatementType.expressionWrapper:
                // if it is a tagOrigin node, it is a <cfset> tag
                if (node.tagOrigin.startTag) {
                    extendCurrentFlowToNode(node.tagOrigin.startTag);
                    bindNode(node.tagOrigin.startTag, node);
                    return;
                }
                
                if (node.expr) extendCurrentFlowToNode(node.expr); // it could be a "null statement"
                bindNode(node.expr, node);

                return;
            case StatementType.fromTag:
                if (node.tagOrigin.startTag) extendCurrentFlowToNode(node.tagOrigin.startTag);
                bindNode(node.tagOrigin.startTag, node);
                break;
            case StatementType.scriptSugaredTagCallStatement:
                // check attrs against cf tag meta here
                if (node.expr) extendCurrentFlowToNode(node.expr);
                bindNode(node.expr, node);
                bindList(node.scriptSugaredTagStatement!.attrs, node);
                return;
        }
    }

    function bindReturnStatement(node: ReturnStatement) {
        if (node.tagOrigin.startTag) {
            const expr = (node.tagOrigin.startTag as CfTag.ScriptLike).expr!;
            extendCurrentFlowToNode(expr);
            bindNode(expr, node);
            return;
        }
        extendCurrentFlowToNode(node);
        bindNode(node.expr, node);
        currentFlow = freshFlow([], FlowType.postReturn);
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
        // kludge-ish: after binding block contents, extend the current flow to the block terminator (whatever it is, as per the block type)
        // goal here is we get a flow node from which every block-contained flow is reachable; especially helpful for using as the predecessor
        // for closureflows
        // n.b, cf is not block scoped, but function scoped; so while we extend the flow here, it may well just carry on into the next block, that 
        // should be no problem
        switch (node.subType) {
            // @fixme better fromTag type safety (always a common tag? never scriptlike, definitely never script or comment or text)
            // and so a cLike block has non-null left/right braces, and etc.
            case BlockType.fromTag:
                maybeBindTagResult(node.tagOrigin.startTag);
                bindNode(node.tagOrigin.startTag!, node);
                bindListFunctionsFirst(node.stmtList, node);
                bindNode(node.tagOrigin.endTag!, node);
                break;
            case BlockType.scriptSugaredTagCallBlock:
                // check against cf tag meta
                bindList(node.sugaredCallStatementAttrs!, node);
                bindListFunctionsFirst(node.stmtList, node);
                break;
            case BlockType.scriptTagCallBlock:
                // check against cf tag meta
                // maybe push context to make sure children are correct
                bindList(node.tagCallStatementArgs!.args, node);
                bindListFunctionsFirst(node.stmtList, node);
                break;
            case BlockType.cLike:
                bindListFunctionsFirst(node.stmtList, node);
                break;
        }
    }

    function STUB_RELOCATEME_isValidIdentifier(s: string) {
        s;
        return true;
    }

    /**
     * some tags write their results in the current environment
     * it would be better if we defined this at a library level, but then we would need some minimal effect system
     * to say "this binds the name E to a type of T in some visible scope G"
     */
    function maybeBindTagResult(tag: CfTag | null) : void {
        if (!tag) return;
        if (tag.tagType !== CfTag.TagType.common) {
            return;
        }

        function getReturnValueIdentifier(attrName: string) {
            const string = getTriviallyComputableString(getAttributeValue((<CfTag.Common>tag).attrs, attrName));
            if (string !== undefined && STUB_RELOCATEME_isValidIdentifier(string)) {
               return string.split(".");
            }
            return undefined;
        }

        if (!tag.typeAnnotation) {
            return;
        }

        let name : string[] | undefined = undefined;

        switch (tag.canonicalName) {
            case "directory":
            case "param":
            case "query": {
                name = getReturnValueIdentifier("name");
                break;
            }
            case "savecontent": {
                name = getReturnValueIdentifier("variable");
                break;
            }
            case "http": {
                name = getReturnValueIdentifier("result");
                break;
            }
        }

        if (!name || name.length > 2) {
            return;
        }

        let targetScope : cfStruct = RootNode.containedScope.variables!;
        let targetName = name.length === 1 ? name[0] : name[1];

        if (name.length === 2) {
            switch (name[0].toLowerCase()) {
                case "local": {
                    if (currentContainer.containedScope.local) {
                        targetScope = currentContainer.containedScope.local;
                    }
                    else {
                        return;
                    }
                }
                case "variables": {
                    break;
                }
                default: {
                    return;
                }
            }
        }

        targetScope.membersMap.set(targetName, tag.typeAnnotation);
    }

    function bindSimpleStringLiteral(node: SimpleStringLiteral) {
        bindNode(node.textSpan, node);
    }

    function bindInterpolatedStringLiteral(node: InterpolatedStringLiteral) {
        bindList(node.elements, node);
    }

    function bindIdentifier(node: Identifier) {
        bindNode(node.source, node);
    }

    function bindIndexedAccess(node: IndexedAccess) {
        bindNode(node.root, node);
        let parent : Node = node.root;
        for (let i = 0; i < node.accessElements.length; i++) {
            const element = node.accessElements[i];
            bindNode(element, parent);
            parent = element;
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

    function bindSliceExpression(node: SliceExpression) {
        bindNode(node.from, node);
        bindNode(node.to, node);
        bindNode(node.stride, node);
    }

    function bindFunctionParameter(node: FunctionParameter) {
        if (node.fromTag) {
            bindNode(node.tagOrigin.startTag, node);

            if (node.canonicalName !== undefined) {
                weakBindIdentifierToScope(node.canonicalName, currentContainer.containedScope.arguments!);
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
            if (node.kind === NodeType.functionDefinition || node.kind === NodeType.arrowFunctionDefinition) {
                return node as NodeWithScope<FunctionDefinition | ArrowFunctionDefinition, "arguments">;
            }
            node = node.parent;
        }
        return undefined;
    }

    function getAncestorOfType(node: Node | null, nodeType: NodeType) : Node | undefined {
        while (node) {
            if (node.kind === nodeType) {
                return node;
            }
            node = node.parent;
        }
        return undefined;
    }

    function isBuiltinScopeName(s: string | undefined) : s is keyof Omit<ScopeDisplay, "container" | "typedefs"> {
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
    function weakBindIdentifierToScope(name: string, scope: cfStruct) : void {
        if (scope.caselessMembersMap.has(name)) {
            return;
        }
        scope.caselessMembersMap.set(name, SyntheticType.any);
    }

    // assignment and declaration should share more code
    function bindAssignment(node: BinaryOperator) {
        bindAssignmentFlow(node.left, node.right);
        bindNode(node.left, node);

        const savedFlow = currentFlow;
        currentFlow = node.right.flow!;
        bindNode(node.right, node);
        currentFlow = savedFlow;

        const target = node.left;

        if (target.kind === NodeType.indexedAccess) {
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
                    firstAccessAsString = firstAccessElement.property.token.text.toLowerCase();
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
            // not a built-in scope name, just use target base name as identifier
            // this is not a declaration so it binds to the root variables scope
            else {
                //weakBindIdentifierToScope(targetBaseName, RootNode.containedScope.variables!);
            }
        }
        else {
            const targetBaseName = getTriviallyComputableString(target)?.toLowerCase();
            if (targetBaseName) {
                const targetScope = currentContainer.containedScope.local
                    ? currentContainer.containedScope.local
                    : RootNode.containedScope.variables!;

                if (targetScope.caselessMembersMap.has(targetBaseName)) {
                    return;
                }

                targetScope.caselessMembersMap.set(targetBaseName, SyntheticType.any);
            }
        }
    }

    function isHoistableFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition) : node is FunctionDefinition {
        return node.kind === NodeType.functionDefinition && node.parent?.kind !== NodeType.binaryOperator;
    }

    function bindFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition) {
        if (isHoistableFunctionDefinition(node) && typeof node.canonicalName === "string") {
            // lucee appears to not err on the following, but acf does
            // we need to model that hoistable functions are always hoisted into the root scope,
            // but are only visible within their declaration container:
            // function foo() { 
            //      function bar() {}
            // }
            // function bar() {} -- error, functions may only be defined once
            // bar(); -- error, bar is not visible

            let scopeTarget : cfStruct;
            if (currentContainer.containedScope.local) {
                scopeTarget = currentContainer.containedScope.local;
            }
            else {
                scopeTarget = RootNode.containedScope.variables!;
            }
            
            scopeTarget.caselessMembersMap.set(node.canonicalName, cfFunctionSignature(
                node.canonicalName,
                (<any[]>node.params).map((param: Script.FunctionParameter | Tag.FunctionParameter) => ({...param, type: SyntheticType.any})),
                SyntheticType.any));
        }

        node.containedScope = {
            container: currentContainer,
            typedefs: new Map(),
            local: SyntheticType.struct(),
            arguments: SyntheticType.struct(),
        };

        for (const param of node.params) {
            node.containedScope.arguments!.caselessMembersMap.set(param.canonicalName, SyntheticType.any);
        }

        const detachedFlow = newFlowGraphRootedAt(node);
        detachedClosureFlows.push(detachedFlow);

        const savedFlow = currentFlow;
        const savedContainer = currentContainer;
        const savedDetachedClosureFlows = detachedClosureFlows;
        
        currentFlow = detachedFlow;
        currentContainer = node as NodeWithScope;
        detachedClosureFlows = [];

        bindList(node.params, node);

        if (node.kind === NodeType.functionDefinition && node.fromTag) {
            bindList(node.body, node);
        }
        else {
            bindNode(node.body, node);
        }

        connectDetachedClosureFlowsToCurrentFlow();

        currentFlow = savedFlow;
        currentContainer = savedContainer;
        detachedClosureFlows = savedDetachedClosureFlows;
    }

    function bindSwitch(node: Switch) {
        if (node.fromTag) {
            bindNode(node.tagOrigin.startTag, node);
            bindList(node.cases, node);
            bindNode(node.tagOrigin.endTag, node);
            return;
        }
        bindNode(node.expr, node);
        bindList(node.cases, node);
    }

    function bindSwitchCase(node: SwitchCase) {
        if (node.fromTag) {
            bindNode(node.tagOrigin.startTag, node);
            bindList(node.body, node);
            bindNode(node.tagOrigin.endTag, node);
            return;
        }
        bindNode(node.expr, node);
        bindList(node.body, node);
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
            extendCurrentFlowToNode(node.forIn!.init);
            bindNode(node.forIn!.init, node);
            bindNode(node.forIn!.inToken, node);
            extendCurrentFlowToNode(node.forIn!.expr);
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
        if (node.subType === StructLiteralInitializerMemberSubtype.keyed) {
            bindNode(node.key, node);
            bindNode(node.expr, node);
        }
        else {
            bindNode(node.expr, node);
        }
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
            bindList(node.catchBlocks, node);
            bindNode(node.finallyBlock, node);
            bindNode(node.tagOrigin.endTag, node);
            return;
        }
        bindList(node.body, node);
        bindList(node.catchBlocks, node);
        bindNode(node.finallyBlock, node);
    }

    function bindCatch(node: Catch) {
        if (node.fromTag) {
            bindNode(node.tagOrigin.startTag, node);
            bindList(node.body, node);
            bindNode(node.tagOrigin.endTag, node);
            if (!getAncestorOfType(node, NodeType.try)) {
                errorAtRange(node.tagOrigin.startTag!.range, "A catch tag must be contained within a try tag-block.");
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
        getNodeMap: () => <ReadonlyMap<NodeId, Node>>nodeMap,
    }

    return self;
}

let debugBinder = false;
