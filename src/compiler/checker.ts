import { SourceFile, Node, NodeType, BlockType, IndexedAccess, StatementType, CallExpression, IndexedAccessType, NodeId, CallArgument, BinaryOperator, BinaryOpType, FunctionDefinition, ArrowFunctionDefinition, FunctionParameter, copyFunctionParameterForTypePurposes, IndexedAccessChainElement, NodeFlags, BinaryOpTypeUiString, VariableDeclaration, Identifier, FlowId, Flow, ScopeDisplay, StaticallyKnownScopeName, isStaticallyKnownScopeName, For, ForSubType } from "./node";
import { Scanner } from "./scanner";
import { Diagnostic } from "./parser";
import { cfFunctionSignature, cfIntersection, Type, TypeKind, cfCachedTypeConstructorInvocation, cfTypeConstructor, cfNever, cfStruct, cfUnion, SyntheticType } from "./types";
import { getTriviallyComputableString } from "./utils";

export function Checker() {
    let sourceFile!: SourceFile;
    let scanner!: Scanner;
    scanner;
    let diagnostics!: Diagnostic[];
    let stdLib : cfStruct | undefined = undefined;
    let rootScope: ScopeDisplay;

    function check(sourceFile_: SourceFile, scanner_: Scanner, diagnostics_: Diagnostic[]) {
        sourceFile = sourceFile_;
        scanner = scanner_;
        diagnostics = diagnostics_;

        stdLib = findStdLib(sourceFile);

        rootScope = sourceFile.containedScope!;

        checkList(sourceFile.content);
        runDeferredFrames();
    }

    function typeErrorAtNode(node: Node, msg: string) {
        const freshDiagnostic : Diagnostic = {
            fromInclusive: node.range.fromInclusive,
            toExclusive: node.range.toExclusive,
            msg: msg,
        }

        const debugFrom = scanner.getAnnotatedChar(freshDiagnostic.fromInclusive);
        const debugTo = scanner.getAnnotatedChar(freshDiagnostic.toExclusive);
        // bump 0-offsetted info to editor-centric 1-offset
        freshDiagnostic.__debug_from_line = debugFrom.line+1;
        freshDiagnostic.__debug_from_col = debugFrom.col+1;
        freshDiagnostic.__debug_to_line = debugTo.line+1;
        freshDiagnostic.__debug_to_col = debugTo.col+1;

        diagnostics.push(freshDiagnostic);
    }

    function checkList(nodes: Node[]) {
        for (const node of nodes) {
            checkNode(node);
        }
    }

    function checkNode(node: Node | null) {
        if (!node) return;

        switch (node.kind) {
            case NodeType.sourceFile:
                throw "Check source files by binding its content";
            case NodeType.comment:
                return;
            case NodeType.type:
                return;
            case NodeType.textSpan:
                return;
            case NodeType.terminal:
                return;
            case NodeType.hashWrappedExpr: // fallthrough
            case NodeType.parenthetical:   // fallthrough
            case NodeType.tagAttribute:
                return;
            case NodeType.tag:
                return;
            case NodeType.callExpression:
                checkCallExpression(node);
                return;
            case NodeType.callArgument:
                checkCallArgument(node);
                return;
            case NodeType.unaryOperator:
                return;
            case NodeType.binaryOperator:
                checkBinaryOperator(node);
                return;
            case NodeType.conditional:
                return;
            case NodeType.variableDeclaration:
                checkVariableDeclaration(node);
                return;
            case NodeType.statement:
                switch (node.subType) {
                    case StatementType.expressionWrapper: {
                        checkNode(node.expr);
                    }
                }
                return;
            case NodeType.returnStatement:
                return;
            case NodeType.breakStatement:
                return;
            case NodeType.continueStatement:
                return;
            case NodeType.block:
                switch (node.subType) {
                    case BlockType.fromTag:
                        // check attrs and etc...
                        checkList(node.stmtList);
                        return;
                    case BlockType.cLike:
                        checkList(node.stmtList);
                        return;
                    case BlockType.scriptSugaredTagCallBlock:
                        checkList(node.stmtList);
                        return;
                    case BlockType.scriptTagCallBlock:
                        checkList(node.stmtList);
                        return;
                }
                return;
            case NodeType.simpleStringLiteral:
                setCachedTermEvaluatedType(node, SyntheticType.string);
                return;
            case NodeType.interpolatedStringLiteral:
                setCachedTermEvaluatedType(node, SyntheticType.string);
                return;
            case NodeType.numericLiteral:
                setCachedTermEvaluatedType(node, SyntheticType.number);
                return;
            case NodeType.booleanLiteral:
                setCachedTermEvaluatedType(node, SyntheticType.boolean);
                return;
            case NodeType.identifier:
                // klude/fixme!: identifier.source can be an indexed access
                // this was to support `a.b.c = 42`
                // what we need to do is parse that as a "dotted path";
                // if it turns out that the expression is an assignment, keep it that way
                // otherwise, transform it into an indexed-access expression
                // and if during "dotted-path" parsing, if we see that it became
                // a.b.c[1], or similar, transform it into an indexed-access
                checkIdentifier(node);
                return;
            case NodeType.indexedAccess:
                checkIndexedAccess(node);
                return;
            case NodeType.indexedAccessChainElement:
                checkIndexedAccessChainElement(node);
                return;
            case NodeType.sliceExpression:
                return;
            case NodeType.functionParameter:
                return;
            case NodeType.functionDefinition: // fallthrough
            case NodeType.arrowFunctionDefinition:
                checkFunctionDefinition(node);
                return;
            case NodeType.dottedPath:
                return;
            case NodeType.switch:
                return;
            case NodeType.switchCase:
                return;
            case NodeType.do:
                return;
            case NodeType.while:
                return;
            case NodeType.ternary:
                return;
            case NodeType.for:
                checkFor(node);
                return;
            case NodeType.structLiteral:
                return;
            case NodeType.structLiteralInitializerMember:
                return;
            case NodeType.arrayLiteral:
                return;
            case NodeType.arrayLiteralInitializerMember:
                return;
            case NodeType.try:
                return;
            case NodeType.catch:
                return;
            case NodeType.finally:
                return;
            case NodeType.importStatement:
                return;
            case NodeType.new:
                return;
            case NodeType.type:
                return;
            default:
                ((_:never) => { throw "Non-exhaustive case or unintentional fallthrough." })(node);
        }
    }
/*
    function getNearestScopeByName(node: Node, scopeName: StaticallyKnownScopeName) : cfStruct | undefined {
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
    }*/

    //https://helpx.adobe.com/coldfusion/developing-applications/the-cfml-programming-language/using-coldfusion-variables/about-scopes.html
    const scopeLookupOrder : readonly StaticallyKnownScopeName[] = [
        "local",
        "arguments",
        "query", // magic inaccessible scope inside a <cfloop query=#q#>...</cfquery> body
        "thread",
        "variables",
        "cgi",
        "file",
        "url",
        "form",
        "cookie",
        "client"
    ];

    interface ScopeDisplayMemberResolution {
        scopeName: StaticallyKnownScopeName,
        type: Type
    }
    function getScopeDisplayMember(scope: ScopeDisplay, canonicalName: string) : ScopeDisplayMemberResolution | undefined {
        for (const scopeName of scopeLookupOrder) {
            if (scope.hasOwnProperty(scopeName)) {
                const entry = scope[scopeName]!.membersMap.get(canonicalName) || scope[scopeName]!.caselessMembersMap.get(canonicalName);
                if (entry) {
                    return {scopeName: scopeName, type: entry};
                }
            }
        }
        return undefined;
    }

    function walkUpScopesToFindIdentifier(base: Node, canonicalName: string) : ScopeDisplayMemberResolution | undefined {
        let node : Node | null = base;
        while (node) {
            if (node.containedScope) {
                const varEntry = getScopeDisplayMember(node.containedScope, canonicalName);
                if (varEntry) { return varEntry; }

                if (node.kind === NodeType.sourceFile) {
                    // if we got to root and didn't find it, see if we can find it in stdlib (if stdlib was available)
                    if (stdLib) {
                        const type = lookupTypeStructMember(stdLib, canonicalName);
                        if (type) {
                            const scopeName = "variables";
                            return {scopeName, type};
                        }
                    }
                    return undefined;
                }

                else { node = node.containedScope.container; }
            }
            else {
                node = node.parent;
            }
        }

        return undefined;
    }

    function findStdLib(sourceFile: SourceFile) : cfStruct | undefined {
        for (const libFile of sourceFile.libRefs) {
            for (const typedef of libFile.content) {
                if (typedef.kind === NodeType.type && typedef.typeKind === TypeKind.struct && typedef.name === "std") {
                    return typedef;
                }
            }
        }
        return undefined;
    }

    /**
     * lookup a name in a typestruct; cased takes precedence over uncased
     * @param struct 
     * @param name 
     */
    function lookupTypeStructMember(struct: cfStruct, name: string) {
        return struct.membersMap.get(name) ?? struct.caselessMembersMap.get(name);
    }

    function getTypeAtFlow(base: Node, name: string, deferOnFunctionContainer = true) : Type | "defer" | undefined {
        const gen = worker();
        const capturedFrame = currentFrame;
        capturedFrame.push(gen);
        return gen.next().value;

        function* worker() : Generator<"defer", Type | undefined, never> {
        let node: Node | null = base;
        let flow: Flow | null = null;
        while (node) {
            if (node.flow) {
                flow = node.flow;
                break;
            }
            else {
                node = node.parent;
            }
        }

        if (!flow) { // we'll be defensive, but we should always get a Flow (a "root flow") from SourceFile
            return undefined;
        }

        let closestContainer : Node | null = null;

        while (flow) {
            const type = getCachedEvaluatedTypeOfIdentifierAtFlow(flow, name);
            // we got a type at this flow
            if (type) {
                capturedFrame.pop();
                return type;
            }
            else if (flow.node?.containedScope) {
                if (!closestContainer) closestContainer = flow.node;

                const scopeMember = getScopeDisplayMember(flow.node.containedScope, name);
                if (scopeMember) {
                    // no type at this flow, but the name is defined on this scope;
                    // stop walking upwards, because the current flow's node is the top-most container for this name,
                    // even if there is a predecessor flow
                    capturedFrame.pop();

                    if (scopeMember.type.typeKind === TypeKind.functionSignature) { // a function definition is always defined
                        return scopeMember.type;
                    }

                    if (flow.node === closestContainer) {
                        return undefined; // it's defined on the closest containing scope, but hasn't been assigned yet
                    }
                    else {
                        return scopeMember.type; // it's defined in an outer scope
                    }
                }
                if (deferOnFunctionContainer && flow.node.kind === NodeType.functionDefinition) {
                    // we hit a containing function, and have not yet figured out the type
                    // check the rest of the grand-parent's body 
                    // but before we defer, check all parent containers to see if they even contain this name
                    if (walkUpScopesToFindIdentifier(flow.node, name)) {
                        yield "defer";
                        return rerunAssertingNoDefer(getTypeAtFlow, flow.predecessor[0].node!, name, /*deferOnFunctionContainer*/ false);
                    }
                    else {
                        capturedFrame.pop();
                        return undefined;
                    }
                }
            }

            // if we got to root and didn't find it, see if we can find it in stdlib (if stdlib was available)
            if (flow.node?.kind === NodeType.sourceFile) {
                capturedFrame.pop();
                if (stdLib) return lookupTypeStructMember(stdLib, name);
            }

            // default case: move to predecessor flow, if one exists
            // todo: support multiple predecessors for if/else
            flow = flow.predecessor[0];
        }

        capturedFrame.pop();
        return undefined;
        }
    }

    function isCallable(type: Type) : boolean {
        return type.typeKind === TypeKind.any
            || type.typeKind === TypeKind.functionSignature
            || (type.typeKind === TypeKind.intersection && (isCallable(type.left) || isCallable(type.right)))
            || (type.typeKind === TypeKind.union && isCallable(type.left) && isCallable(type.right));
    }

    function unsafeAssertTypeKind<T extends Type>(_type: Type) : asserts _type is T {}

    /**
     * is the following legal:
     * <to> = <assignThis>
     * @param assignThis 
     * @param to 
     */
    function isAssignable(assignThis: Type, to: Type) : boolean {
        if (assignThis.typeKind === TypeKind.any || to.typeKind === TypeKind.any) {
            return true;
        }
        if (assignThis.typeKind !== to.typeKind) {
            return false;
        }
        if (assignThis.typeKind === TypeKind.struct && to.typeKind === TypeKind.struct) {
            if (assignThis.membersMap.size < to.membersMap.size) {
                return false;
            }

            for (const key of to.membersMap.keys()) {
                if (!assignThis.membersMap.has(key)) {
                    return false;
                }
                if (!isAssignable(assignThis.membersMap.get(key)!, to.membersMap.get(key)!)) {
                    return false;
                }
            }

            return true;
        }

        // ah no cool handling of function assignability or etc.
        return true;
    }

    function pushTypesIntoInlineFunctionDefinition(context: Node, signature: cfFunctionSignature, functionDef: (FunctionDefinition | ArrowFunctionDefinition)) {
        const existingArgumentsScope = functionDef.containedScope!.arguments!;
        for (let i = 0; i < signature.params.length; i++) {
            if (i === functionDef.params.length) {
                break;
            }
            if (existingArgumentsScope.membersMap.has(signature.params[i].canonicalName)) {
                existingArgumentsScope.membersMap.set(signature.params[i].canonicalName, evaluateType(context, signature.params[i].type));
            }
        }
    }

    function checkCallExpression(node: CallExpression) {
        checkNode(node.left);
        checkList(node.args);
        const type = getCachedTermEvaluatedType(node.left);
        if (type) {
            if (isCallable(type)) {
                if (type.typeKind === TypeKind.any) {
                    setCachedTermEvaluatedType(node, type);
                    return;
                }

                unsafeAssertTypeKind<cfFunctionSignature>(type);
                setCachedTermEvaluatedType(node, type.returns);

                //const requiredParams = type.params.filter(param => param.required === true);
                const namedArgCount = node.args.filter(arg => !!arg.equals).length;
                if (namedArgCount !== node.args.length && namedArgCount !== 0) {
                    // error, named args must be all or none
                }
                if (namedArgCount === node.args.length) {
                    // reorder args to match params ?
                }

                for (let i = 0; i < node.args.length; i++) {
                    const paramType = type.params[i]?.type ?? null;
                    if (!paramType) break;
                    const arg = node.args[i];
                    const argType = getCachedTermEvaluatedType(arg);
                    if (!isAssignable(/*assignThis*/ argType, /*to*/ paramType)) {
                        // error
                    }
                    if (paramType.typeKind === TypeKind.functionSignature && (arg.expr.kind === NodeType.functionDefinition || arg.expr.kind === NodeType.arrowFunctionDefinition)) {
                        pushTypesIntoInlineFunctionDefinition(node, paramType, arg.expr);
                        checkNode(arg.expr.body as Node /*fixme: we know this is a script function definition but can't prove it here; anyway, all function defs should have Node as a body, not Node|Node[] ?*/);
                    }
                }
            }
            else {
                // error
            }
        }
        

    }

    function checkCallArgument(node: CallArgument) {
        switch (node.expr.kind) {
            case NodeType.indexedAccess: {
                const x = getCachedTermEvaluatedType(node.expr.root);
                x;
            }
        }
    }

    type Deferrable<T> = (...args: any[]) => "defer" | T;

    function rerunAssertingNoDefer<T, F extends Deferrable<T>>(f: F, ...args: Parameters<F>) : Exclude<T, "defer"> {
        const result = f(...args);
        if (result === "defer") throw "unexpected defer";
        return result as Exclude<T, "defer">;
    }

    function checkBinaryOperator(node: BinaryOperator) {
        const capturedFrame = currentFrame;
        const gen = worker();
        capturedFrame.push(gen);
        return gen.next();

        function* worker() : Generator<"defer", void, never> {
            checkNode(node.left);
            checkNode(node.right);

            switch (node.optype) {
                case BinaryOpType.assign: {
                    // an assignment, even fv-unqualified, will always be bound to a scope
                    // `x = y` is effectively `variables.x = y`
                    if (node.left.kind === NodeType.identifier && node.left.canonicalName) {
                        let lhsType = getTypeAtFlow(node.left, node.left.canonicalName);
                        if (lhsType === "defer") {
                            yield "defer";
                            lhsType = rerunAssertingNoDefer(getTypeAtFlow, node.left, node.left.canonicalName);
                        }
                        /*if (lhsType === "defer") {
                            yield "defer";
                            lhsType = g_lhsType.next().value;
                            if (lhsType === "defer") throw "unexpected defer";
                        }*/
                        const rhsType = getCachedTermEvaluatedType(node.right);
                        if (!lhsType) {
                            // there is no type at the current flow; so, this is the first assignment for this var in this scope
                            if (node.typeAnnotation) {
                                const evaluatedTypeAnnotation = evaluateType(node, node.typeAnnotation);
                                if (!isAssignable(rhsType, node.typeAnnotation)) {
                                    typeErrorAtNode(node.right, "RHS is not assignable to LHS.");
                                }
                                setCachedEvaluatedTypeOfIdentifierAtFlow(node.left.flow!, node.left.canonicalName, evaluatedTypeAnnotation);
                            }
                            else {
                                setCachedEvaluatedTypeOfIdentifierAtFlow(node.left.flow!, node.left.canonicalName, SyntheticType.any);
                            }
                        }
                        else {
                            if (node.typeAnnotation) {
                                typeErrorAtNode(node, "Type annotations can only be bound to an identifier's first assignment.");
                            }
                            if (!isAssignable(rhsType, lhsType)) {
                                typeErrorAtNode(node.right, "RHS is not assignable to LHS.");
                            }
                        }
                    }

                    break;
                }
                case BinaryOpType.assign_cat:
                case BinaryOpType.contains:
                case BinaryOpType.does_not_contain:
                case BinaryOpType.cat: {
                    const leftType = getCachedTermEvaluatedType(node.left);
                    const rightType = getCachedTermEvaluatedType(node.right);
                    if (leftType.typeKind !== TypeKind.any && leftType.typeKind !== TypeKind.string) {
                        typeErrorAtNode(node.left, `Left operand to '${BinaryOpTypeUiString[node.optype]}' operator must be a string.`);
                    }
                    if (rightType.typeKind !== TypeKind.any && rightType.typeKind !== TypeKind.string) {
                        typeErrorAtNode(node.right, `Right operand to '${BinaryOpTypeUiString[node.optype]}' operator must be a string.`);
                    }
                    break;
                }
                // all other operators are (number op number)
                default: {
                    const leftType = getCachedTermEvaluatedType(node.left);
                    const rightType = getCachedTermEvaluatedType(node.right);
                    // acf allows (bool) + (bool), but maybe we don't want to support that
                    if (leftType.typeKind !== TypeKind.any && leftType.typeKind !== TypeKind.number) {
                        typeErrorAtNode(node.left, `Left operand to '${BinaryOpTypeUiString[node.optype]}' operator must be a number.`);
                    }
                    if (rightType.typeKind !== TypeKind.any && rightType.typeKind !== TypeKind.number) {
                        typeErrorAtNode(node.right, `Right operand to '${BinaryOpTypeUiString[node.optype]}' operator must be a number.`);
                    }
                }
            }

            capturedFrame.pop();
        }
    }

    const evaluatedTypeMap = new Map<FlowId, Map<string, Type>>();
    function setCachedEvaluatedTypeOfIdentifierAtFlow(flow: Flow, name: string, type: Type) : void {
        if (!evaluatedTypeMap.has(flow.flowId)) {
            evaluatedTypeMap.set(flow.flowId, new Map());
        }
        evaluatedTypeMap.get(flow.flowId)!.set(name, type);
    }

    function getCachedEvaluatedTypeOfIdentifierAtFlow(flow: Flow, name: string) : Type | undefined {
        return evaluatedTypeMap.get(flow.flowId)?.get(name);
    }

    function checkVariableDeclaration(node: VariableDeclaration) : void {
        // check for re-defined finals in current scope...or maybe during binding phase
        if (node.expr.kind === NodeType.binaryOperator) {
            //checkNode(node.expr.left)
            checkNode(node.expr.right);

            const name = getTriviallyComputableString(node.expr.left);
            if (!name) return;
            let evaluatedType : Type;
            if (node.typeAnnotation) {
                evaluatedType = evaluateType(node, node.typeAnnotation);
            }
            else {
                evaluatedType = SyntheticType.any;
            }

            const rhsType = getCachedTermEvaluatedType(node.expr.right);
            if (!isAssignable(rhsType, evaluatedType)) {
                typeErrorAtNode(node.expr.left, "Leftside type is not assignable to rightside type.");
                evaluatedType = SyntheticType.any;
            }
            else /* is assignable */ {
                if (!node.typeAnnotation) {
                    evaluatedType = rhsType;
                }
            }
            if (node.expr.left.kind === NodeType.identifier && node.expr.left.canonicalName !== undefined && node.expr.left.flow?.successor) {
                setCachedEvaluatedTypeOfIdentifierAtFlow(node.expr.left.flow, node.expr.left.canonicalName, evaluatedType);
            }
        }
    }

    // fixme: only good per instance...
    // is this a singleton or what
    const termEvaluatedTypeCache = new Map<NodeId, Type>();

    function getCachedTermEvaluatedType(node: Node | null) {
        if (!node) {
            return SyntheticType.any;
        }

        let targetId = node.nodeId;

        /*if (node.kind === NodeType.indexedAccess) {
            targetId = node.accessElements[node.accessElements.length-1].nodeId;
        }*/

        if (termEvaluatedTypeCache.has(targetId)) {
            return termEvaluatedTypeCache.get(targetId)!;
        }
        else {
            return SyntheticType.any;
        }
    }

    function setCachedTermEvaluatedType(node: Node, type: Type) {
        if (type.typeKind === TypeKind.typeConstructor
            || type.typeKind === TypeKind.typeConstructorInvocation
            || type.typeKind === TypeKind.cachedTypeConstructorInvocation) {
            typeErrorAtNode(node, "Type is not concrete.");
            termEvaluatedTypeCache.set(node.nodeId, SyntheticType.any);
        }
        else {
            termEvaluatedTypeCache.set(node.nodeId, type);
        }
    }

    function isStructOrArray(type: Type) : boolean {
        return type.typeKind === TypeKind.struct
            || type.typeKind === TypeKind.array
            || (type.typeKind === TypeKind.intersection && (isStructOrArray(type.left) || isStructOrArray(type.right)))
            || (type.typeKind === TypeKind.union && isStructOrArray(type.left) && isStructOrArray(type.right));
    }

    function findAncestor(node: Node, predicate: (node: Node | null) => true | false | "bail") : Node | undefined {
        let current : Node | null = node;
        while (current) {
            const result = predicate(current);
            if (result === true) {
                return current;
            }
            else if (result === false) {
                current = current.parent;
            }
            else if (result === "bail") {
                break;
            }
        }
        return undefined;
    }

    function getContainingFunction(node: Node) {
        return findAncestor(node, (node) => node?.kind === NodeType.functionDefinition);
    }

    function checkIdentifier(node: Identifier) {
        const gen = worker();
        //frameRoots.push(node.nodeId);
        const capturedFrame = currentFrame;
        capturedFrame.push(gen);
        return gen.next().value;

        function* worker() : Generator<"defer", void, never> {
            // if we're on the lefthand side of a non-fv qualified assignment, we're done
            // an fv-qualified assignment is handled by checkVariableDeclaration
            // assignment should alter the type of the variable for this flow in checkBinaryOperator
            if (node.parent?.kind === NodeType.binaryOperator && node.parent.optype === BinaryOpType.assign && node === node.parent.left) {
                capturedFrame.pop();
                return;
            }

            const name = node.canonicalName;
            if (name !== undefined) {
                if (isStaticallyKnownScopeName(name)) {
                    if (name === "local" || name === "arguments") {
                        const containingFunction = getContainingFunction(node);
                        if (containingFunction) setCachedTermEvaluatedType(node, containingFunction.containedScope![name]!);
                    }
                    else {
                        setCachedTermEvaluatedType(node, rootScope[name] ?? SyntheticType.any);
                    }
                    return;
                }

                let type = getTypeAtFlow(node, name);
                if (type === "defer") {
                    yield "defer";
                    type = rerunAssertingNoDefer(getTypeAtFlow, node, name, /*deferOnFunctionContainer*/ false);
                }

                if (type) {
                    setCachedTermEvaluatedType(node, evaluateType(node, type));
                    capturedFrame.pop();
                    return;
                }

                const symbolTableEntry = walkUpScopesToFindIdentifier(node, name); // it's possible we did this already in getTypeAtFlow, can we reuse the value instead of doing it again
                if (symbolTableEntry) {
                    // there is a symbol table entry, but we could not find a type on the flow graph
                    // this is a use-before-defintion
                    // be nice to say "local" scope or "variables" scope or etc.
                    typeErrorAtNode(node, `Identifier '${name}' is in '${symbolTableEntry.scopeName}' scope, but is used before being assigned.`);
                }
                else {
                    typeErrorAtNode(node, `Cannot find name '${name}'.`);
                }
            }
        }
    }

    function checkIndexedAccess(node: IndexedAccess) {
        checkNode(node.root);

        let type = getCachedTermEvaluatedType(node.root);
        if (!type || type.typeKind === TypeKind.any) {
            return;
        }

        // we set cached types on 'root' elements,
        // that is, the indexed-access root node itself, and the subsequent elements
        // not on the component identifiers, dots, brackets, etc.
        if (isStructOrArray(type)) {
            setCachedTermEvaluatedType(node.root, type);
            for (let i = 0; i < node.accessElements.length; i++) {
                const element = node.accessElements[i];
                if (element.accessType === IndexedAccessType.dot) {
                    type = getMemberType(type, node, element.property.token.text);
                    if (type.typeKind === TypeKind.any) {
                        type = SyntheticType.any; // subsequent access elements will also be any
                        setCachedTermEvaluatedType(element, SyntheticType.any);
                    }
                    else {
                        setCachedTermEvaluatedType(element, type);
                    }
                }
            }
            setCachedTermEvaluatedType(node, type);
        }
        else {
            setCachedTermEvaluatedType(node.root, SyntheticType.any);
            for (const element of node.accessElements) {
                setCachedTermEvaluatedType(element, SyntheticType.any);
            }
            // error: some kind of indexed access error
        }

        checkList(node.accessElements);
    }

    function checkIndexedAccessChainElement(node: IndexedAccessChainElement) {
        if (node.parent!.flags & NodeFlags.checkerError) {
            return;
        }

        const parentType = getCachedTermEvaluatedType(node.parent);
        if (parentType.typeKind === TypeKind.any) {
            return;
        }

        if (node.accessType === IndexedAccessType.dot) {
            const name = node.property.token.text;
            if (!(<cfStruct>parentType).membersMap.has(name)) {
                // need a flag like "checked struct accesses"
                //typeErrorAtNode(node.property, `Property '${name}' does not exist on parent type.`);
                node.flags |= NodeFlags.checkerError;
            }
        }
    }

    const frames : any[] = [];
    let currentFrame = frames;
    //const frameRoots : NodeId[] = [];
    function runDeferredFrames() {
        runFrameWorker(frames);
        function runFrameWorker(frame: any | any[]) {
            if (Array.isArray(frame)) {
                for (let i = frame.length-1; i >= 0; i--) {
                    runFrameWorker(frame[i]);
                }
            }
            else {
                frame.next();
            }
        }
    }

    function checkFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition) {
        for (const param of node.params) {
            setCachedEvaluatedTypeOfIdentifierAtFlow(node.flow!, param.canonicalName, param.type || SyntheticType.any);
        }

        const savedFrame = currentFrame;
        const thisFrame : any[] = [];
        currentFrame.push(thisFrame);
        currentFrame = thisFrame;
        //frameRoots.push(node.nodeId);

        if (node.kind === NodeType.functionDefinition && node.fromTag) {
            checkList(node.body);
        }
        else {
            checkNode(node.body);
        }

        if (thisFrame.length === 0) {
            currentFrame.pop();
            //frameRoots.pop();
        }

        currentFrame = savedFrame;
    }

    //
    // type lookup
    //
    function walkUpContainersToFindType(context: Node, type: Type) : Type | undefined {
        if (type.typeKind !== TypeKind.typeId) {
            return type;
        }

        let node : Node | null = context;
        const typeName = type.name;

        while (node) {
            if (node.containedScope) {
                if (node.containedScope.typedefs) {
                    if (node.containedScope.typedefs.has(typeName)) {
                        return node.containedScope.typedefs.get(typeName)!;
                    }
                }
                if (node.kind === NodeType.sourceFile) {
                    break;
                }
                else {
                    node = node.containedScope.container;
                }
            }
            else {
                node = node.parent;
            }
        }

        if (node) { // should always be true (we hit the top, SourceFile, and broke out of the above loop)
            // find the `std` type in the libfiles, which we're just assuming will be either present or not, and if so, it is the only libfile
            for (const libFile of node.libRefs) {
                for (const typedef of libFile.content) {
                    if (typedef.kind === NodeType.type && typedef.typeKind === TypeKind.struct && typedef.name === "std") {
                        return typedef.membersMap.get(type.name);
                    }
                }
            }
        }

        if (!type.synthetic) {
            typeErrorAtNode(type, `Cannot find name '${type.name}'.`);
        }
        return undefined;
    }

    function checkFor(node: For) {
        if (node.subType === ForSubType.forIn) {
            checkNode(node.forIn!.expr);
        }
        checkNode(node.body);
    }

    function getMemberType(type: Type, context: Node, name: string) : Type {
        if (type.typeKind === TypeKind.intersection) {
            const left = getMemberType(type.left, context, name);
            const right = getMemberType(type.right, context, name);
            return cfIntersection(left, right);
        }
        else if (type.typeKind === TypeKind.struct) {
            const memberType = type.membersMap.get(name);
            return memberType ? evaluateType(context, memberType) : SyntheticType.any;
        }

        return SyntheticType.any;
    }

    //
    // type evaluation
    //
    const evaluateType = (function() {
        type NodeTrie = Map<NodeId, NodeTrie> & Map<null, Type | "PENDING"> ;
        const typeConstructorInvocationCacheTrie : NodeTrie = new Map();
        
        const enum TypeCache_Status { resolved, resolving, noCache };
        type TypeCache_Resolution = TypeCache_Cached | TypeCache_NoCache;
        interface TypeCache_Cached {
            status: TypeCache_Status.resolved,
            value: Type
        }
        interface TypeCache_NoCache {
            status: TypeCache_Status.resolving | TypeCache_Status.noCache
        }
        
        function setCachedTypeConstructorInvocation(typeFunction: cfTypeConstructor, args: Type[], val: "PENDING" | Type) : void {
            function getChildTrieMapOrNull(thisLevel: NodeTrie, nodeId: NodeId) : NodeTrie | null {
                const result = thisLevel.get(nodeId);
                if (result && typeof result === "object") {
                    return result;
                }
                else {
                    return null;
                }
            }
        
            if (args.length === 0) {
                const bottom : NodeTrie = new Map();
                bottom.set(null, val);
                typeConstructorInvocationCacheTrie.set(typeFunction.nodeId, bottom);
                return;
            }
        
            let workingMap = getChildTrieMapOrNull(typeConstructorInvocationCacheTrie, typeFunction.nodeId);
        
            for (let i = 0; i < args.length; i++) { // is args ever 0 in a type call ?
                if (i === args.length - 1) {
                    if (workingMap === null) {
                        workingMap = new Map() as NodeTrie;
                        typeConstructorInvocationCacheTrie.set(typeFunction.nodeId, workingMap);
                    }
                    const existingNextLevel = getChildTrieMapOrNull(workingMap, args[i].nodeId);
                    if (existingNextLevel) {
                        existingNextLevel.set(null, val);
                    }
                    else {
                        const bottom = new Map([[null, val]]) as NodeTrie;
                        workingMap.set(args[i].nodeId, bottom)
                    }
                }
                else {
                    if (workingMap === null) {
                        workingMap = new Map() as NodeTrie;
                        typeConstructorInvocationCacheTrie.set(typeFunction.nodeId, workingMap);
                    }
        
                    const existingNextLevel = getChildTrieMapOrNull(workingMap, args[i].nodeId);
                    if (existingNextLevel) {
                        workingMap = existingNextLevel;
                    }
                    else {
                        const generatedNextLevel = new Map();
                        workingMap.set(args[i].nodeId, generatedNextLevel);
                        workingMap = generatedNextLevel;
                    }
                }
            }
        }

        function getCachedTypeConstructorInvocation(typeFunction: cfTypeConstructor, args: Type[]) : TypeCache_Resolution {
            let trieDescender = typeConstructorInvocationCacheTrie.get(typeFunction.nodeId);
            for (let i = 0; i < args.length; i++) {
                if (!trieDescender) {
                    return {status: TypeCache_Status.noCache};
                }
                trieDescender = trieDescender.get(args[i].nodeId);
            }
        
            if (!trieDescender) {
                return {status: TypeCache_Status.noCache}
            }
        
            const cachedResult = trieDescender.get(null);
        
            if (!cachedResult) {
                // this should never happen; log an error or something
                return {status: TypeCache_Status.noCache};
            }
            if (cachedResult === "PENDING") {
                return {status: TypeCache_Status.resolving};
            }
            else {
                return {status: TypeCache_Status.resolved, value: cachedResult}
            }
        }

        function evaluateType(context: Node, type: Type | null, typeParamMap: Map<string, Type> = new Map(), depth = 0) : Type {
            return typeWorker(type);

            // here args[n] should already have been evaluated
            function invokeTypeConstructor(typeFunction: cfTypeConstructor, args: Type[]) : Type {
                if (args.findIndex(type => type.typeKind === TypeKind.never) !== -1) {
                    return cfNever();
                }

                depth++;
                const result = (function() {
                    if (typeFunction.params.length !== args.length) {
                        // hit this once by writing @type U<T>
                        // when only @type T<U> was valid;
                        // need some type checking of the type system
                        throw "args.length !== typeFunction.params.length"
                    }
                
                    const cached = getCachedTypeConstructorInvocation(typeFunction, args);
                    if (cached.status === TypeCache_Status.resolved) {
                        return cached.value;
                    }
                
                    const typeParamMap = new Map(typeFunction.capturedParams.entries());
                    // extend constructor's captured environment with the argument list
                    for (let i = 0; i < typeFunction.params.length; i++) {
                        typeParamMap.set(typeFunction.params[i].name, args[i]);
                    }
                
                    // say our current evaluation is for `T<U>`
                    // set `T<U>` to "PENDING" so that we have something to check for to not recurse infinitely on something like `T<U> = {foo: T<U>}`
                    setCachedTypeConstructorInvocation(typeFunction, args, "PENDING");
                    const result = evaluateType(context, typeFunction.body, typeParamMap, depth+1);
                    setCachedTypeConstructorInvocation(typeFunction, args, result);
            
                    if (result.typeKind === TypeKind.typeConstructor) {
                        // if a type constructor returned a type constructor, extend the new type constructor's environment
                        // with the parent's environment + the args to the parent's invocation
                        // inner most names shadows outer names if there are name conflicts
                        result.capturedParams = typeParamMap;
                    }
                    return result;
                })();
                depth--;
                return result;
            }
            
            function evaluateIntersection(left: Type, right: Type) : Type {
                if (left.typeKind === TypeKind.never || right.typeKind === TypeKind.never) {
                    return cfNever();
                }

                depth++;
                const result = (function() {
                    if (left.typeKind === TypeKind.struct && right.typeKind === TypeKind.struct) {
                        let longest = left.membersMap.size > right.membersMap.size ? left.membersMap : right.membersMap;
                        let shortest = longest === left.membersMap ? right.membersMap : left.membersMap;
                
                        const remainingLongestKeys = new Set([...longest.keys()]);
                        const result = new Map<string, Type>();
                        for (const key of shortest.keys()) {
                            remainingLongestKeys.delete(key);
                            const evaluatedShortest = typeWorker(shortest.get(key)!);
                            const evaluatedLongest = longest.has(key) ? typeWorker(longest.get(key)!) : null;
                            if (!evaluatedLongest) {
                                result.set(key, evaluatedShortest);
                                continue;
                            }
                            const intersect = evaluateIntersection(evaluatedShortest, evaluatedLongest);
                            if (intersect.typeKind === TypeKind.never) {
                                return cfNever();
                            }
                            else {
                                result.set(key, intersect);
                            }
                        }
                
                        for (const key of remainingLongestKeys) {
                            result.set(key, longest.get(key)!);
                        }
                
                        return SyntheticType.struct(result);
                    }
                    else {
                        // only valid type operands to the "&" type operator are {}
                        // which is not the "empty interface" but just a shorthand for "struct"
                        return cfNever();
                    }
                })();
                depth--;
                return result;
            }

            function typeWorker(type: Type | null) : Type {
                depth++;
                const result = (function() {
                    if (!type) return SyntheticType.any;
                    
                    switch (type.typeKind) {
                        case TypeKind.intersection: {
                            const left = typeWorker(type.left);
                            const right = typeWorker(type.right,);
                            return evaluateIntersection(left, right);
                        }
                        case TypeKind.union: {
                            const left = typeWorker(type.left);
                            const right = typeWorker(type.right);
                            return cfUnion(left, right);
                        }
                        case TypeKind.struct: { // work on cacheability of this; it is concrete just return a cached copy or something like that
                            const evaluatedStructContents = new Map<string, Type>();
                            let concrete = true;
                            for (const key of type.membersMap.keys()) {
                                const preEvaluatedId = type.membersMap.get(key)!.nodeId;
                                const evaluatedType = typeWorker(type.membersMap.get(key)!);
                                const postEvaluatedId = evaluatedType.nodeId;
                                if (preEvaluatedId !== postEvaluatedId) {
                                    concrete = false;
                                }
                                evaluatedStructContents.set(key, evaluatedType);
                            }
                            if (concrete) {
                                return type;
                            }
                            return SyntheticType.struct(evaluatedStructContents);
                        }
                        case TypeKind.functionSignature:
                            // need a more lean version of functionParameter
                            const params : FunctionParameter[] = type.params.map(param => copyFunctionParameterForTypePurposes(param));
                            for (let i = 0; i < type.params.length; i++) {
                                if (!params[i].type) {
                                    throw "no type for parameter " + i; /// can we get here? parser / binder should convert this to any before we get here...
                                }
                                params[i].type = typeWorker(params[i].type!);
                            }
                            const returns = typeWorker(type.returns);
                            return cfFunctionSignature(type.name, params, returns);
                        case TypeKind.typeConstructorInvocation: {
                            const typeConstructor = typeWorker(type.left);
                            if (typeConstructor.typeKind === TypeKind.never) {
                                return typeConstructor;
                            }

                            unsafeAssertTypeKind<cfTypeConstructor>(typeConstructor);

                            if (type.args.length === 0) {
                                typeErrorAtNode(type.left, `Type argument list cannot be empty.`);
                                return SyntheticType.never;
                            }
                            if (typeConstructor.params.length != type.args.length) {
                                typeErrorAtNode(type.left, `Type requires ${typeConstructor.params.length} arguments.`);
                                return SyntheticType.never;
                            }

                            const args : Type[] = [];
                            for (const arg of type.args) {
                                if (arg.typeKind === TypeKind.typeId) {
                                    args.push(typeWorker(typeParamMap.get(arg.name) || walkUpContainersToFindType(context, arg) || SyntheticType.any));
                                }
                                else {
                                    args.push(typeWorker(arg));
                                }
                            }
                
                            const cachedTypeCall = getCachedTypeConstructorInvocation(typeConstructor as cfTypeConstructor, args);
                            if (cachedTypeCall.status === TypeCache_Status.resolved) {
                                return cachedTypeCall.value;
                            }
                            else if (cachedTypeCall.status === TypeCache_Status.resolving) {
                                return cfCachedTypeConstructorInvocation(typeConstructor as cfTypeConstructor, args);
                            }
                            else {
                                return invokeTypeConstructor(typeConstructor as cfTypeConstructor, args);
                            }
                        }
                        case TypeKind.cachedTypeConstructorInvocation: {
                            const cachedTypeCall = getCachedTypeConstructorInvocation(type.left, type.args);
                            if (cachedTypeCall.status === TypeCache_Status.resolved) {
                                return cachedTypeCall.value;
                            }
                            else if (cachedTypeCall.status === TypeCache_Status.resolving) {
                                return type;
                            }
                            else {
                                throw "expected resolved or resolving cache result but got none";
                            }
                        }
                        case TypeKind.typeId: {
                            // @fixme: should error if we can't find it; and return never ?
                            const result = typeParamMap.get(type.name) || walkUpContainersToFindType(context, type) || null;
                            if (!result) return cfNever();
                            return typeWorker(result);
                        }
                        default:
                            return type;
                    }
                })();
                depth--;
                return result;
            }
        }

        return evaluateType;
    })();

    return {
        check,
        getCachedTermEvaluatedType
    }
}
