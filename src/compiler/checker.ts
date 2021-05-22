import { SourceFile, Node, NodeType, BlockType, IndexedAccess, isStaticallyKnownScopeName, Scope, StaticallyKnownScopeName, ScopeDisplay, Term, StatementType, CallExpression, IndexedAccessType, NodeId, CallArgument, BinaryOperator, BinaryOpType, FunctionDefinition, ArrowFunctionDefinition, FunctionParameter, copyFunctionParameterForTypePurposes } from "./node";
import { Scanner } from "./scanner";
import { Diagnostic } from "./parser";
import { cfAny, cfFunctionSignature, cfIntersection, Type, TypeKind, cfCachedTypeConstructorInvocation, cfTypeConstructor, cfNever, cfStruct, cfUnion } from "./types";

export function Checker() {
    let sourceFile!: SourceFile;
    let scanner!: Scanner;
    scanner;
    let diagnostics!: Diagnostic[];
    diagnostics;

    function check(sourceFile_: SourceFile, scanner_: Scanner, diagnostics_: Diagnostic[]) {
        sourceFile = sourceFile_;
        scanner = scanner_;
        diagnostics = diagnostics_;

        checkList(sourceFile.content);
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
                return;
            case NodeType.interpolatedStringLiteral:
                return;
            case NodeType.numericLiteral: // fallthrough
            case NodeType.booleanLiteral:
                return;
            case NodeType.identifier:
                // klude/fixme!: identifier.source can be an indexed access
                // this was to support `a.b.c = 42`
                // what we need to do is parse that as a "dotted path";
                // if it turns out that the expression is an assignment, keep it that way
                // otherwise, transform it into an indexed-access expression
                // and if during "dotted-path" parsing, if we see that it became
                // a.b.c[1], or similar, transform it into an indexed-access
                checkNode(node.source);
                return;
            case NodeType.indexedAccess:
                checkIndexedAccess(node);
                return;
            case NodeType.indexedAccessChainElement:
                return;
            case NodeType.sliceExpression:
                return;
            case NodeType.functionParameter:
                return;
            case NodeType.functionDefinition: // fallthrough
            case NodeType.arrowFunctionDefinition:
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

    function getNearestScopeByName(node: Node, scopeName: StaticallyKnownScopeName) : Scope | undefined {
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
    }

    //https://helpx.adobe.com/coldfusion/developing-applications/the-cfml-programming-language/using-coldfusion-variables/about-scopes.html
    const scopeLookupOrder = [
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
    ] as const;

    function getContainerVariable(scope: ScopeDisplay, canonicalName: string) : Term | undefined {
        for (const scopeName of scopeLookupOrder) {
            if (scope.hasOwnProperty(scopeName)) {
                const entry = (<Scope>scope[scopeName as keyof ScopeDisplay]).get(canonicalName);
                if (entry) {
                    return entry;
                }
            }
        }
        return undefined;
    }

    function walkUpContainersToFindSymtabEntry(base: Node, canonicalName: string) : Term | undefined {
        let node : Node | null = base;
        while (node) {
            if (node.containedScope) {
                const varEntry = getContainerVariable(node.containedScope, canonicalName);
                if (varEntry) { return varEntry; }
                else { node = node.containedScope.container; }
            }
            else {
                node = node.parent;
            }
        }

        return undefined;
    }

    function isCallable(type: Type) : boolean {
        return type.typeKind === TypeKind.any
            || type.typeKind === TypeKind.functionSignature
            || (type.typeKind === TypeKind.intersection && (isCallable(type.left) || isCallable(type.right)))
            || (type.typeKind === TypeKind.union && isCallable(type.left) && isCallable(type.right));
    }

    // fixme: shim to support Scopes not being Types but rather a Map<string, Variable>
    // a Scope sould just be a type level struct with (mostly?) string keys
    function isType(type: Type | Scope) : type is Type {
        return type.hasOwnProperty("kind") && (<any>type).kind === NodeType.type;
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
            if (assignThis.members.size < to.members.size) {
                return false;
            }

            for (const key of to.members.keys()) {
                if (!assignThis.members.has(key)) {
                    return false;
                }
                if (!isAssignable(assignThis.members.get(key)!, to.members.get(key)!)) {
                    return false;
                }
            }

            return true;
        }

        // ah no cool handling of function assignability or etc.
        return true;
    }

    function pushTypesIntoInlineFunctionDefinition(context: Node, signature: cfFunctionSignature, functionDef: (FunctionDefinition | ArrowFunctionDefinition)) {
        const existingArgumentsScope = functionDef.containedScope?.arguments!;
        for (let i = 0; i < signature.params.length; i++) {
            if (i === functionDef.params.length) {
                break;
            }
            if (existingArgumentsScope.has(signature.params[i].canonicalName)) {
                existingArgumentsScope.set(signature.params[i].canonicalName, {
                    type: evaluateType(context, signature.params[i].type),
                    name: signature.params[i].canonicalName,
                    final: false,
                    var: false,
                    target: functionDef.params[i]
                });
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
                    return;
                }
                unsafeAssertTypeKind<cfFunctionSignature>(type);
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

    function checkBinaryOperator(node: BinaryOperator) {
        switch (node.optype) {
            case BinaryOpType.assign: {
                if (node.left.kind === NodeType.identifier) {
                    //const type = getTypeFromNearestContainingScope(node.left);
                    /*if (type && isType(type) && type.typeKind === TypeKind.deferred) {
                        const inferredType = inferExpressionType(node.right);
                    }*/
                }
            }
        }
    }

    // fixme: only good per instance...
    // is this a singleton or what
    const termEvaluatedTypeCache = new Map<NodeId, Type>();

    function getCachedTermEvaluatedType(node: Node | null) {
        if (!node) {
            return cfAny();
        }

        let targetId = node.nodeId;

        /*if (node.kind === NodeType.indexedAccess) {
            targetId = node.accessElements[node.accessElements.length-1].nodeId;
        }*/

        if (termEvaluatedTypeCache.has(targetId)) {
            return termEvaluatedTypeCache.get(targetId)!;
        }
        else {
            return cfAny();
        }
    }

    function setCachedTermEvaluatedType(node: Node, type: Type) {
        termEvaluatedTypeCache.set(node.nodeId, type);
    }

    function isStructOrArray(type: Type) : boolean {
        return type.typeKind === TypeKind.struct
            || type.typeKind === TypeKind.array
            || (type.typeKind === TypeKind.intersection && (isStructOrArray(type.left) || isStructOrArray(type.right)))
            || (type.typeKind === TypeKind.union && isStructOrArray(type.left) && isStructOrArray(type.right));
    }

    function checkIndexedAccess(node: IndexedAccess) {
        let type = getTypeFromNearestContainingScope(node.root);
        if (!type || !isType(type) || type.typeKind === TypeKind.any) {
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
                        type = cfAny(); // subsequent access elements will also be any
                        setCachedTermEvaluatedType(element, cfAny());
                    }
                    else {
                        setCachedTermEvaluatedType(element, type);
                    }
                }
            }
            setCachedTermEvaluatedType(node, type);
        }
        else {
            setCachedTermEvaluatedType(node.root, cfAny());
            for (const element of node.accessElements) {
                setCachedTermEvaluatedType(element, cfAny());
            }
            // error: some kind of indexed access error
        }
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
                node = node.containedScope.container;
            }
            else {
                node = node.parent;
            }
        }

        return undefined;
    }

    /**
     * given an identifier, find it's symbol table entry and retreive its typeinfo
     */
    function getTypeFromNearestContainingScope(base: Node) : Type | Scope | undefined {
        let type : Type | undefined;
        switch (base.kind) {
            case NodeType.identifier: {
                if (!base.canonicalName) {
                    return undefined;
                }
                if (isStaticallyKnownScopeName(base.canonicalName)) {
                    return getNearestScopeByName(base, base.canonicalName);
                }
                else {
                    const symtabEntry = walkUpContainersToFindSymtabEntry(base, base.canonicalName);
                    if (!symtabEntry) {
                        return undefined;
                    }
                    type = symtabEntry.type;
                }
                break;
            }
            default:
                return undefined;
        }

        if (!type) {
            return undefined;
        }
        else if (!isType(type)) {
            return type as Scope;
        }
        else {
            const context = base;
            return evaluateType(context, type);
        }
    }

    function getMemberType(type: Type, context: Node, name: string) : Type {
        if (type.typeKind === TypeKind.intersection) {
            const left = getMemberType(type.left, context, name);
            const right = getMemberType(type.right, context, name);
            return cfIntersection(left, right);
        }
        else if (type.typeKind === TypeKind.struct) {
            const memberType = type.members.get(name);
            return memberType ? evaluateType(context, memberType) : cfAny();
        }

        return cfAny();
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

            function invokeTypeConstructor(typeFunction: cfTypeConstructor, args: Type[]) : Type {
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
                depth++;
                const result = (function() {
                    if (left.typeKind === TypeKind.struct && right.typeKind === TypeKind.struct) {
                        let longest = left.members.size > right.members.size ? left.members : right.members;
                        let shortest = longest === left.members ? right.members : left.members;
                
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
                
                        return cfStruct(result);
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
                    if (!type) return cfAny();
                    
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
                        case TypeKind.struct: {
                            const evaluatedStructContents = new Map<string, Type>();
                            for (const key of type.members.keys()) {
                                evaluatedStructContents.set(key, typeWorker(type.members.get(key)!));
                            }
                            return cfStruct(evaluatedStructContents, type.stringIndex);
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
                            const args : Type[] = [];
                            for (const arg of type.args) {
                                if (arg.typeKind === TypeKind.typeId) {
                                    args.push(typeParamMap.get(arg.name)!);
                                }
                                else {
                                    args.push(arg);
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
                            const result = typeParamMap.get(type.name) || walkUpContainersToFindType(context, type) || cfAny();
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