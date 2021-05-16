import { SourceFile, Node, NodeType, BlockType, IndexedAccess, isStaticallyKnownScopeName, Scope, StaticallyKnownScopeName, ScopeDisplay, Term, StatementType, CallExpression, IndexedAccessType, NodeId, CallArgument, BinaryOperator, BinaryOpType, FunctionDefinition, ArrowFunctionDefinition } from "./node";
import { Scanner } from "./scanner";
import { Diagnostic } from "./parser";
import { cfAny, cfFunctionSignature, cfIntersection, cfNil, cfTypeId, invokeTypeConstructor, evaluateType, Type, TypeKind } from "./types";

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

    function walkUpContainersToFindName(base: Node, canonicalName: string) : Term | undefined {
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

    function walkUpContainersToFindType(base: Node, type: Type) : Type | undefined {
        if (type.typeKind !== TypeKind.typeId) {
            return type;
        }

        let node : Node | null = base;
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
    function getTypeFromNearestContainingScope(node: Node) : Type | Scope | undefined {
        let type : Type | undefined;
        switch (node.kind) {
            case NodeType.identifier: {
                if (!node.canonicalName) {
                    return undefined;
                }
                if (isStaticallyKnownScopeName(node.canonicalName)) {
                    return getNearestScopeByName(node, node.canonicalName);
                }
                else {
                    type = walkUpContainersToFindName(node, node.canonicalName)?.type;
                }
                break;
            }
            default:
                return undefined;
        }

        if (type && type.typeKind === TypeKind.typeCall) {
            const typeFunction = walkUpContainersToFindType(node, <cfTypeId>type.left);
            if (!typeFunction) {
                // error, "cannot find typename 'foo'"
                return undefined;
            }
            if (typeFunction.typeKind !== TypeKind.typeConstructor) {
                // error, "type 'foo' is not generic"
                return undefined;
            }

            const typeArgs : Type[] = [];
            for (const typeArg of type.args) {
                const foundType = walkUpContainersToFindType(node, typeArg);
                if (!foundType) {
                    typeArgs.push(cfNil());
                }
                else {
                    typeArgs.push(foundType);
                }
            }

            return invokeTypeConstructor(typeFunction, typeArgs);
        }

        return type;
    }

    function getMemberType(type: Type, name: string) : Type {
        if (type.typeKind === TypeKind.intersection) {
            const left = getMemberType(type.left, name);
            const right = getMemberType(type.right, name);
            return cfIntersection(left, right);
        }
        else if (type.typeKind === TypeKind.struct) {
            return type.members.get(name) || cfAny();
        }

        return cfAny();
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

    function pushTypesIntoInlineFunctionDefinition(signature: cfFunctionSignature, functionDef: (FunctionDefinition | ArrowFunctionDefinition)) {
        const existingArgumentsScope = functionDef.containedScope?.arguments!;
        for (let i = 0; i < signature.params.length; i++) {
            if (i === functionDef.params.length) {
                break;
            }
            if (existingArgumentsScope.has(signature.params[i].canonicalName)) {
                existingArgumentsScope.set(signature.params[i].canonicalName, {
                    type: evaluateType(signature.params[i].type),
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
                        pushTypesIntoInlineFunctionDefinition(paramType, arg.expr);
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
                    type = getMemberType(type, element.property.token.text);
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

    return {
        check,
        getCachedTermEvaluatedType
    }
}