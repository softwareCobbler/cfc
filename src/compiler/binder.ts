import { Diagnostic, SymTabEntry, ArrowFunctionDefinition, BinaryOperator, Block, BlockType, CallArgument, FunctionDefinition, Node, NodeKind, Statement, StatementType, VariableDeclaration, mergeRanges, BinaryOpType, IndexedAccessType, ScopeDisplay, NodeId, IndexedAccess, IndexedAccessChainElement, SourceFile, CfTag, CallExpression, UnaryOperator, Conditional, ReturnStatement, BreakStatement, ContinueStatement, FunctionParameter, Switch, SwitchCase, Do, While, Ternary, For, ForSubType, StructLiteral, StructLiteralInitializerMember, ArrayLiteral, ArrayLiteralInitializerMember, Try, Catch, Finally, ImportStatement, New, SimpleStringLiteral, InterpolatedStringLiteral, Identifier, isStaticallyKnownScopeName, StructLiteralInitializerMemberSubtype, SliceExpression, NodeWithScope, Flow, freshFlow, FlowType, ConditionalSubtype, SymTab, TypeShim, UnreachableFlow, NodeFlags } from "./node";
import { getTriviallyComputableString, visit, getAttributeValue, getContainingFunction, getNodeLinks, isInCfcPsuedoConstructor, isHoistableFunctionDefinition, stringifyLValue } from "./utils";
import { CfFileType, Scanner, SourceRange } from "./scanner";
import { SyntheticType, _Type, extractCfFunctionSignature, isFunctionSignature } from "./types";

export function Binder() {
    let RootNode : NodeWithScope<SourceFile>;
    let currentContainer : NodeWithScope;
    let scanner : Scanner;
    let diagnostics: Diagnostic[];

    let currentFlow : Flow;
    let detachedClosureFlows : Flow[] = [];

    let nodeMap = new Map<NodeId, Node>();

    function bind(sourceFile_: SourceFile) {
        if (sourceFile_.cfFileType === CfFileType.dCfm) {
            bindDeclarationFile(sourceFile_);
            return;
        }

        scanner = sourceFile_.scanner;
        diagnostics = sourceFile_.diagnostics;
        nodeMap = new Map<NodeId, Node>();

        RootNode = sourceFile_ as NodeWithScope<SourceFile>;

        RootNode.containedScope = {
            container: null,
            typedefs: new Map<string, _Type>(),
            cgi: new Map<string, SymTabEntry>(),
            variables: new Map<string, SymTabEntry>(),
            url: new Map<string, SymTabEntry>(),
            form: new Map<string, SymTabEntry>(),
        };

        if (sourceFile_.cfFileType === CfFileType.cfc) {
            RootNode.containedScope.this = new Map<string, SymTabEntry>();
        }

        currentFlow = freshFlow([], FlowType.start);
        bindFlowToNode(currentFlow, RootNode);

        if (sourceFile_.cfFileType === CfFileType.cfc) {
            RootNode.containedScope.this = new Map<string, SymTabEntry>();
        }

        currentContainer = RootNode;
        bindListFunctionsLast(sourceFile_.content, sourceFile_);
    }

    function bindFlowToNode(flow: Flow, node: Node) {
        flow.node = node;
        node.flow = flow;
    }

    function mergeFlowsToLabel(...flows: (Flow | undefined)[]) {
        const joinableFlows : Flow[] = [];
        let unreachableCount = 0;
        for (const flow of flows) {
            if (!flow) continue;
            if (flow === UnreachableFlow) unreachableCount++;
            joinableFlows.push(flow);
        }
        if (unreachableCount === joinableFlows.length) return UnreachableFlow;
        return freshFlow(joinableFlows, FlowType.label);
    }

    function bindNode(node: Node | null | undefined, parent: Node) {
        if (!node) return;

        nodeMap.set(node.nodeId, node);
        bindDirectTerminals(node);
        node.parent = parent;
        node.flow = currentFlow;

        switch (node.kind) {
            case NodeKind.sourceFile:
                throw "Bind source files by binding its content";
            case NodeKind.comment:
                if (node.typedefs) {
                    bindList(node.typedefs, node);
                }
                return;
            case NodeKind.textSpan:
                return;
            case NodeKind.terminal:
                bindList(node.trivia, node);
                return;
            case NodeKind.hashWrappedExpr: // fallthrough
            case NodeKind.parenthetical:   // fallthrough
            case NodeKind.tagAttribute:
                bindNode(node.expr, node);
                return;
            case NodeKind.tag:
                bindTag(node);
                return;
            case NodeKind.callExpression:
                bindCallExpression(node);
                return;
            case NodeKind.callArgument:
                bindCallArgument(node);
                return;
            case NodeKind.unaryOperator:
                bindUnaryOperator(node);
                return;
            case NodeKind.binaryOperator:
                bindBinaryOperator(node);
                return;
            case NodeKind.conditional:
                bindConditional(node);
                return;
            case NodeKind.variableDeclaration:
                bindVariableDeclaration(node);
                return;
            case NodeKind.statement:
                bindStatement(node);
                return;
            case NodeKind.returnStatement:
                bindReturnStatement(node);
                return;
            case NodeKind.breakStatement:
                bindBreakStatement(node);
                return;
            case NodeKind.continueStatement:
                bindContinueStatement(node);
                return;
            case NodeKind.block:
                bindBlock(node);
                return;
            case NodeKind.simpleStringLiteral:
                bindSimpleStringLiteral(node);
                return;
            case NodeKind.interpolatedStringLiteral:
                bindInterpolatedStringLiteral(node);
                return;
            case NodeKind.numericLiteral: // fallthrough
            case NodeKind.booleanLiteral:
                // no-op, just a terminal
                return;
            case NodeKind.identifier:
                bindIdentifier(node);
                return;
            case NodeKind.indexedAccess:
                bindIndexedAccess(node);
                return;
            case NodeKind.indexedAccessChainElement:
                bindIndexedAccessChainElement(node);
                return;
            case NodeKind.sliceExpression:
                bindSliceExpression(node);
                return;
            case NodeKind.functionParameter:
                bindFunctionParameter(node);
                return;
            case NodeKind.functionDefinition: // fallthrough
            case NodeKind.arrowFunctionDefinition:
                bindFunctionDefinition(node);
                return;
            case NodeKind.dottedPath:
                // ?
                return;
            case NodeKind.dottedPathRest:
                // no-op, taken care of by dottedpath
                return;
            case NodeKind.switch:
                bindSwitch(node);
                return;
            case NodeKind.switchCase:
                bindSwitchCase(node);
                return;
            case NodeKind.do:
                bindDo(node);
                return;
            case NodeKind.while:
                bindWhile(node);
                return;
            case NodeKind.ternary:
                bindTernary(node);
                return;
            case NodeKind.for:
                bindFor(node);
                return;
            case NodeKind.structLiteral:
                bindStructLiteral(node);
                return;
            case NodeKind.structLiteralInitializerMember:
                bindStructLiteralInitializerMember(node);
                return;
            case NodeKind.arrayLiteral:
                bindArrayLiteral(node);
                return;
            case NodeKind.arrayLiteralInitializerMember:
                bindArrayLiteralInitializerMember(node);
                return;
            case NodeKind.try:
                bindTry(node);
                return;
            case NodeKind.catch:
                bindCatch(node);
                return;
            case NodeKind.finally:
                bindFinally(node);
                return;
            case NodeKind.importStatement:
                bindImportStatement(node);
                return;
            case NodeKind.new:
                bindNew(node);
                return;
            case NodeKind.typeShim:
                bindTypeShim(node);
                return;
            default:
                ((_:never) => { throw "Non-exhaustive case or unintentional fallthrough." })(node);
        }
    }

    function bindDirectTerminals(node: Node) {
        visit(node, function(visitedNode: Node | null | undefined) {
            if (visitedNode?.kind === NodeKind.terminal) {
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

    function bindListFunctionsLast(nodes: Node[], parent: Node) {
        bindList(nodes.filter(node => node.kind !== NodeKind.functionDefinition), parent);
        bindList(nodes.filter(node => node.kind === NodeKind.functionDefinition), parent);
    }

    function bindTypeShim(node: TypeShim) {
        // types always have names here? -- yes, typedefs do, but we need to explicitly indicate that inside the typesystem
        // we get them from `@type x = ` so presumably always...
        // also `@declare function foo` and possibly `@declare global <identifier-name> : type`
        currentContainer.containedScope.typedefs.set(node.type.name!, node.type);
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

        bindNode(node.left, node);
        bindNode(node.right, node);
    }

    function bindConditional(node: Conditional) {
        if (node.subType === ConditionalSubtype.if || node.subType === ConditionalSubtype.elseif) {
            const savedFlow = currentFlow;
            let expr : Node;
            if (node.fromTag) {
                expr = (node.tagOrigin.startTag as CfTag.ScriptLike).expr!;
            }
            else {
                expr = node.expr!;
            }

            bindNode(expr, node);

            currentFlow = freshFlow([savedFlow], FlowType.condition);
            bindNode(node.consequent, node);

            const trueBranchEnd = currentFlow;
            let falseBranchEnd : Flow | undefined = undefined;

            if (node.alternative) {
                currentFlow = freshFlow([savedFlow], FlowType.condition);
                bindNode(node.alternative, node);
                falseBranchEnd = currentFlow;
            }
            else {
                // the "end" of the non-existent false branch is where we started -- i.e., no flow changes
                falseBranchEnd = savedFlow;
            }

            currentFlow = mergeFlowsToLabel(trueBranchEnd, falseBranchEnd);
        }
        else {
            bindNode(node.consequent, node);
        }
    }

    function addSymbolToTable(symTab: SymTab, uiName: string, declaringNode: Node, type: _Type | null = null, declaredType?: _Type | null) : SymTabEntry {
        const canonicalName = uiName.toLowerCase();
        let symTabEntry : SymTabEntry;

        // fixme: if there are type annotations on multiple declarations of the same var, they should match
        if (symTab.has(canonicalName)) {
            symTabEntry = symTab.get(canonicalName)!;
            if (Array.isArray(symTabEntry.declarations)) {
                symTabEntry.declarations.push(declaringNode);
            }
            else {
                symTabEntry.declarations = symTabEntry.declarations ? [symTabEntry.declarations, declaringNode] : declaringNode;
            }
        }
        else {
            symTabEntry = {
                uiName,
                canonicalName,
                declarations: declaringNode,
                type: type ?? SyntheticType.any,
                declaredType: declaredType ?? null
            };

            symTab.set(canonicalName, symTabEntry);
        }

        return symTabEntry;
    }

    function bindForInInit(node: VariableDeclaration) : void {
        let targetScope = RootNode.containedScope.variables!;
        if (node.varModifier) {
            const containingFunction = getContainingFunction(node);
            if (!containingFunction) {
                errorAtRange(node.expr.range, "Local variables may not be declared at top-level scope.");
                return;
            }
            targetScope = containingFunction.containedScope?.local!;
        }

        if (node.expr.kind === NodeKind.identifier) {
            const name = getTriviallyComputableString(node.expr);
            if (!name) return;
            getNodeLinks(node).symTabEntry = addSymbolToTable(targetScope, name, node);
        }
        else if (node.expr.kind === NodeKind.indexedAccess) {
            // need a dot/bracket "path creation" mechanism, e.g., for (local.foo in bar) {}
        }
    }

    function bindVariableDeclaration(node: VariableDeclaration) {
        if (node.parent?.kind === NodeKind.for && node.parent.subType === ForSubType.forIn && node.parent.init === node) {
            bindForInInit(node);
            return;
        }

        if (node.expr.kind === NodeKind.binaryOperator) {
            bindNode(node.expr.left, node);

            bindNode(node.expr.right, node);
            currentFlow = freshFlow([currentFlow], FlowType.assignment);
            bindFlowToNode(currentFlow, node);
        }

        let identifierBaseName : ReturnType<typeof stringifyLValue> | undefined = undefined;
        
        if (node.expr.kind === NodeKind.binaryOperator && node.expr.optype === BinaryOpType.assign &&
            (node.expr.left.kind === NodeKind.indexedAccess || node.expr.left.kind === NodeKind.identifier)) {
            identifierBaseName = stringifyLValue(node.expr.left);
        }

        // make sure we got a useable name
        if (!identifierBaseName) {
            return;
        }

        const [uiPath, canonicalPath] = [identifierBaseName.ui.split("."), identifierBaseName.canonical.split(".")];

        if (isStaticallyKnownScopeName(canonicalPath[0]) && canonicalPath.length === 1 && node.varModifier) {
            errorAtRange(mergeRanges(node.finalModifier, node.varModifier, (<BinaryOperator>node.expr).left), "Variable declaration shadows built-in scope `" + canonicalPath[0] + "`");
        }
        else if (isStaticallyKnownScopeName(canonicalPath[0]) && canonicalPath.length === 2) {
            let targetScope : SymTab | undefined = undefined;
            if (canonicalPath[0] === "local") {
                targetScope = currentContainer.containedScope.local;
            }
            else {
                targetScope = RootNode.containedScope[canonicalPath[0]];
            }

            if (targetScope) {
                addSymbolToTable(targetScope, uiPath[1], node, null, node.typeAnnotation);
            }

            return;
        }

        if (node.finalModifier || node.varModifier) {
            if (getContainingFunction(node)) {
                addSymbolToTable(currentContainer.containedScope.local!, uiPath[0], node, null, node.typeAnnotation);
            }
            else {
                // we're not in a function, so we must be at top-level scope
                errorAtRange(mergeRanges(node.finalModifier, node.varModifier, (<BinaryOperator>node.expr)?.left), "Local variables may not be declared at top-level scope.");
            }
        
            const enclosingFunction = getEnclosingFunction(node);
            if (enclosingFunction) {
                // if name is same as a parameter definition's name, emit an error,
                // e.g, 
                // function foo(bar) { var bar = 42; }
                // is an error: "bar is already defined in argument scope"
                if (enclosingFunction.containedScope.arguments.has(canonicalPath[0])) {
                    errorAtRange(mergeRanges(node.finalModifier, node.varModifier, node.expr), `'${uiPath[0]}' is already defined in argument scope.`);
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
                maybeBindTagResult(node.tagOrigin.startTag);
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
            const expr = (node.tagOrigin.startTag as CfTag.ScriptLike).expr;
            if (expr) {
                bindNode(expr, node);
            }
        }
        else {
            if (node.expr) {
                bindNode(node.expr, node);
            }
        }

        currentFlow = UnreachableFlow;
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
                bindNode(node.tagOrigin.startTag, node);
                bindListFunctionsLast(node.stmtList, node);
                bindNode(node.tagOrigin.endTag, node);
                break;
            case BlockType.scriptSugaredTagCallBlock:
                // check against cf tag meta
                bindList(node.sugaredCallStatementAttrs!, node);
                bindListFunctionsLast(node.stmtList, node);
                break;
            case BlockType.scriptTagCallBlock:
                // check against cf tag meta
                // maybe push context to make sure children are correct
                bindList(node.tagCallStatementArgs!.args, node);
                bindListFunctionsLast(node.stmtList, node);
                break;
            case BlockType.cLike:
                bindListFunctionsLast(node.stmtList, node);
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

        function getReturnValueIdentifier(attrName: string) : string[] | undefined {
            const string = getTriviallyComputableString(getAttributeValue((<CfTag.Common>tag).attrs, attrName));
            if (string !== undefined && STUB_RELOCATEME_isValidIdentifier(string)) {
               return string.split(".");
            }
            return undefined;
        }

        let attrName : string | undefined = undefined;

        switch (tag.canonicalName) {
            case "directory":
            case "param":
            case "query": {
                attrName = "name";
                break;
            }
            case "savecontent": {
                attrName = "variable";
                break;
            }
            case "http": {
                attrName = "result";
                break;
            }
            case "loop": {
                const enum TagLoopKind { condition, query, struct, array, ranged };

                function determineTagLoopKind(tag: CfTag.Common) : TagLoopKind | undefined {
                    // <cfloop condition="expr">
                    if (getAttributeValue(tag.attrs, "condition")) return TagLoopKind.condition;
                    // <cfloop query=#q#>
                    if (getAttributeValue(tag.attrs, "query")) return TagLoopKind.query;
                    // <cfloop collection=#c# item="keyname">
                    if (getAttributeValue(tag.attrs, "collection")) return TagLoopKind.struct;
                    // <cfloop array=#a# item="elementname">
                    if (getAttributeValue(tag.attrs, "array")) return TagLoopKind.array;
                    // <cfloop from=#1# to=#n# index="indexname">
                    if (getAttributeValue(tag.attrs, "from")) return TagLoopKind.ranged;
                    return undefined;
                }

                switch (determineTagLoopKind(tag)) {
                    case TagLoopKind.condition:
                    case TagLoopKind.query: {
                        return;
                    }
                    case TagLoopKind.struct:
                    case TagLoopKind.array: {
                        attrName = "item";
                        break;
                    }
                    case TagLoopKind.ranged: {
                        attrName = "index";
                        break;
                    }
                    default: return;
                }
            }
        }

        if (!attrName) return;

        const name = getReturnValueIdentifier(attrName);

        if (!name || name.length > 2) {
            return;
        }

        let targetScope : SymTab = RootNode.containedScope.variables!;
        let targetName = name.length === 1 ? name[0] : name[1];

        if (name.length === 2) {
            switch (name[0].toLowerCase()) {
                case "local": {
                    if (currentContainer.containedScope.local) {
                        targetScope = currentContainer.containedScope.local;
                    }
                    else {
                        const attrVal = getAttributeValue((<CfTag.Common>tag).attrs, attrName);
                        // this is not really an error; it will simply write into `local.foo` in the global context
                        // but perhaps is a code smell to be warned about
                        if (attrVal) errorAtRange(attrVal.range, `Tag binds result name '${name[1]}' to a local scope in a non-local context.`);
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

        addSymbolToTable(targetScope, targetName, tag, tag.typeAnnotation);
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

    // @todo - extract non-canonical name for uiName
    function bindFunctionParameter(node: FunctionParameter) {
        if (node.fromTag) {
            bindNode(node.tagOrigin.startTag, node);

            if (node.canonicalName !== undefined) {
                addSymbolToTable(currentContainer.containedScope.arguments!, node.uiName, node);
            }

            return;
        }

        addSymbolToTable(currentContainer.containedScope.arguments!, node.uiName, node);
        bindNode(node.javaLikeTypename, node);
        bindNode(node.identifier, node);
        bindNode(node.defaultValue, node);
    }

    function getEnclosingFunction(node: Node | null) : NodeWithScope<FunctionDefinition | ArrowFunctionDefinition, "arguments"> | undefined {
        while (node) {
            if (node.kind === NodeKind.functionDefinition || node.kind === NodeKind.arrowFunctionDefinition) {
                return node as NodeWithScope<FunctionDefinition | ArrowFunctionDefinition, "arguments">;
            }
            node = node.parent;
        }
        return undefined;
    }

    function getAncestorOfType(node: Node | null, nodeType: NodeKind) : Node | undefined {
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

    // assignment and declaration should share more code
    function bindAssignment(node: BinaryOperator) {
        bindNode(node.left, node);

        const savedFlow = currentFlow;
        currentFlow = node.right.flow!;
        bindNode(node.right, node);
        currentFlow = savedFlow;

        const target = node.left;

        if (target.kind === NodeKind.indexedAccess) {
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
                        addSymbolToTable(currentContainer.containedScope.local, firstAccessAsString, node);
                    }
                    else {
                        // assigning to `local.x` in a non-local scope just binds the name `local` to the root variables scope
                        addSymbolToTable(RootNode.containedScope.variables!, "local", node);
                    }
                }
                else {
                    if (targetBaseName in RootNode.containedScope) {
                        addSymbolToTable(RootNode.containedScope[targetBaseName]!, firstAccessAsString, node);
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
            const targetBaseName = getTriviallyComputableString(target);
            if (targetBaseName) {
                const targetBaseCanonicalName = targetBaseName.toLowerCase();
                const targetScope = currentContainer.containedScope.local
                    ? currentContainer.containedScope.local
                    : RootNode.containedScope.variables!;

                if (targetScope.has(targetBaseCanonicalName)) {
                    return;
                }

                addSymbolToTable(targetScope, targetBaseName, node);
            }
        }
    }

    function bindFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition) {
        const signature = extractCfFunctionSignature(node);
        if (isHoistableFunctionDefinition(node) && typeof node.canonicalName === "string") {
            // lucee appears to not err on the following, but acf does
            // we need to model that hoistable functions are always hoisted into the root scope,
            // but are only visible within their declaration container:
            // function foo() { 
            //      function bar() {}
            // }
            // function bar() {} -- error, functions may only be defined once
            // bar(); -- error, bar is not visible

            const existingFunction = RootNode.containedScope.variables!.get(node.canonicalName) || currentContainer.containedScope.this?.get(node.canonicalName);
            if (existingFunction && isFunctionSignature((existingFunction as SymTabEntry).type)) {
                // if acf { ...
                //      errorAtRange(getFunctionNameRange(node.range), "Redefinition of hoistable function.");
                // }
            }

            let scopeTarget : SymTab;

            if (currentContainer.containedScope.local) { // it's only callable from local, but in ACF the name is taken globally
                scopeTarget = currentContainer.containedScope.local;
            }
            else if (RootNode.cfFileType === CfFileType.cfc && isInCfcPsuedoConstructor(node)) {
                scopeTarget = currentContainer.containedScope.this!;
            }
            else {
                scopeTarget = RootNode.containedScope.variables!;
            }
            
            addSymbolToTable(
                scopeTarget,
                node.uiName!,
                node,
                signature,
                node.typeAnnotation);
        }

        node.containedScope = {
            container: currentContainer,
            typedefs: new Map<string, _Type>(),
            local: new Map<string, SymTabEntry>(),
            arguments: new Map<string, SymTabEntry>(),
        };

        const typeAnnotatedParams = node.typeAnnotation && isFunctionSignature(node.typeAnnotation) ? node.typeAnnotation.params : null;

        for (let i = 0; i < node.params.length; i++) {
            const param = node.params[i];
            const type = signature.params[i]?.type || null;
            const annotatedType = typeAnnotatedParams?.[i].type || null;
            // we also need the annotation-provided type if it exists; for now it is implicitly null
            addSymbolToTable(node.containedScope.arguments!, param.uiName, param, type, annotatedType);
        }

        if (!node.fromTag && node.kind === NodeKind.functionDefinition) {
            // access modifier is a terminal which gets autobound in bindDirectTerminals, otherwise we would bind that here, too
            bindNode(node.returnType, node);
            bindNode(node.nameToken, node);
        }

        const startFlow = freshFlow([], FlowType.start);
        bindFlowToNode(startFlow, node);

        const savedFlow = currentFlow;
        const savedContainer = currentContainer;
        const savedDetachedClosureFlows = detachedClosureFlows;
        
        currentFlow = startFlow;
        currentContainer = node as NodeWithScope;
        detachedClosureFlows = [];

        bindList(node.params, node);

        if (node.kind === NodeKind.functionDefinition && node.fromTag) {
            bindList(node.body, node);
        }
        else {
            bindNode(node.body, node);
        }

        // if the end of a function is reachable, mark it as such
        // maybe want a node.endFlow or something
        if (currentFlow !== UnreachableFlow) {
            node.flags |= NodeFlags.flowWithNoReturn;
        }

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
            bindNode(node.init, node);
            bindNode(node.inToken, node);
            bindNode(node.expr, node);
            bindNode(node.body, node);
            return;
        }
        
        bindNode(node.initExpr, node);
        bindNode(node.semi1, node);
        bindNode(node.conditionExpr, node);
        bindNode(node.semi2, node);
        bindNode(node.incrementExpr, node);
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
            if (!getAncestorOfType(node, NodeKind.try)) {
                if (node.tagOrigin.startTag) {
                    errorAtRange(node.tagOrigin.startTag.range, "A catch tag must be contained within a try tag-block.");
                }
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
            if (!getAncestorOfType(node, NodeKind.try)) {
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

    function bindDeclarationFile(sourceFile: SourceFile) {
        sourceFile.containedScope = {
            container: null,
            typedefs: new Map<string, _Type>(),
            global: new Map<string, SymTabEntry>()
        };

        for (const node of sourceFile.content) {
            if (node.kind === NodeKind.typeShim) {
                if (isFunctionSignature(node.type)) {
                    addSymbolToTable(sourceFile.containedScope!.global!, node.type.uiName, node);
                }
            }
            else {
                errorAtRange(node.range, "Illegal non-declaration in declaration file.");
                continue;
            }
        }
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

export type Binder = ReturnType<typeof Binder>;