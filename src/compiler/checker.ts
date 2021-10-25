import { Diagnostic, SourceFile, Node, NodeKind, BlockType, IndexedAccess, StatementType, CallExpression, IndexedAccessType, CallArgument, BinaryOperator, BinaryOpType, FunctionDefinition, ArrowFunctionDefinition, IndexedAccessChainElement, NodeFlags, VariableDeclaration, Identifier, Flow, isStaticallyKnownScopeName, For, ForSubType, UnaryOperator, Do, While, Ternary, StructLiteral, StructLiteralInitializerMemberSubtype, StructLiteralInitializerMember, ArrayLiteral, ArrayLiteralInitializerMember, Catch, Try, Finally, New, Switch, CfTag, SwitchCase, SwitchCaseType, Conditional, ConditionalSubtype, SymTabEntry, mergeRanges, ReturnStatement, SymbolResolution, SymbolTable, Property } from "./node";
import { CfcResolver, EngineSymbolResolver } from "./project";
import { Scanner, CfFileType, SourceRange } from "./scanner";
import { cfFunctionSignature, cfIntersection, cfCachedTypeConstructorInvocation, cfTypeConstructor, Struct, cfUnion, SyntheticType, TypeFlags, cfArray, extractCfFunctionSignature, _Type, isTypeId, isIntersection, isStructLike, isUnion, isFunctionSignature, isTypeConstructorInvocation, isCachedTypeConstructorInvocation, isArray, isTypeConstructor, getCanonicalType, stringifyType, cfFunctionSignatureParam, unsafe__cfFunctionSignatureWithFreshReturnType, isFunctionOverloadSet, cfFunctionOverloadSet, createLiteralType, isLiteralType, cfTypeId, SymbolTableTypeWrapper, CfcTypeWrapper, Interface, StructKind, isCfcLookupType } from "./types";
import { exhaustiveCaseGuard, findAncestor, getAttributeValue, getContainingFunction, getFunctionDefinitionAccessLiteral, getFunctionDefinitionReturnsLiteral, getSourceFile, getTriviallyComputableString, isCfcMemberFunctionDefinition, isSimpleOrInterpolatedStringLiteral, Mutable, stringifyDottedPath, stringifyLValue, stringifyStringAsLValue } from "./utils";
import { walkupScopesToResolveSymbol as externWalkupScopesToResolveSymbol } from "./utils";
import { Engine, EngineVersion, supports } from "./engines";

const structViewCache = WeakPairMap<SymbolTable, Interface, Struct>(); // map a SymbolTable -> Interface -> Struct, used to check prior existence of a wrapping of a symbol table into a struct with a possible interface extension

export function Checker() {
    let sourceFile!: SourceFile;
    let scanner!: Scanner;
    let diagnostics!: Diagnostic[];
    let noUndefinedVars = false;
    let engineVersion!: EngineVersion;

    function check(sourceFile_: SourceFile) {
        // we're using the Checker as a singleton, and checking one source file might trigger checking another
        // so, we have to save state before descending into the next check
        const savedSourceFile = sourceFile;
        
        sourceFile = sourceFile_;
        scanner = sourceFile.scanner;
        diagnostics = sourceFile.diagnostics;

        if (sourceFile.cfFileType === CfFileType.cfc && sourceFile.cfc?.extends && sourceFile.cfc?.extends.containedScope.this) {
            sourceFile.containedScope.super = sourceFile.cfc.extends.containedScope.this;
        }

        instantiateInterfaces(sourceFile);
        checkListFunctionsLast(sourceFile.content);

        for (const decorator of sourceFile.containedScope.typedefs.decorators) {
            if (decorator.name === "QuickInstance") {
                QuickInstance(sourceFile);
            }
        }

        sourceFile = savedSourceFile;
        scanner = savedSourceFile?.scanner;
        diagnostics = savedSourceFile?.diagnostics;
    }

    function instantiateInterfaces(node: Node) {
        if (node.containedScope?.typedefs.mergedInterfaces) {
            for (const [ifaceName, iface] of node.containedScope.typedefs.mergedInterfaces) {
                // fixme: preserve order; all spreads happend first right now
                const freshMembers = new Map<string, SymTabEntry>();
                if (iface.instantiableSpreads) {
                    for (const spread of iface.instantiableSpreads) {
                        const type = evaluateType(node, spread);
                        if (isStructLike(type)) {
                            for (const [k,v] of type.members) {
                                freshMembers.set(k,v);                                
                            }
                        }
                    }
                }
                for (const [memberName, member] of iface.members) {
                    if (isCfcLookupType(member.type)) {
                        (member as Mutable<SymTabEntry>).type = evaluateType(node, member.type);
                    }
                    freshMembers.set(memberName, member);
                }
                const freshType = Interface(ifaceName, freshMembers);
                (freshType as Mutable<Interface>).underlyingType = iface;
                node.containedScope.typedefs.mergedInterfaces.set(ifaceName, freshType);
            }
        }
    }

    /*
    rmme
    function installHeritage() {
        if (!sourceFile.containedScope.this) return; // n.b. CFCs should always have a `this` scope installed during binding
        let ancestor = sourceFile.cfc?.extends;

        sourceFile.containedScope.super = new Map();

        while (ancestor) {
            if (ancestor.containedScope.this) {
                for (const [k,v] of ancestor.containedScope.this) {
                    if (!sourceFile.containedScope.this.has(k)) { // this has this bindings already intalled, we just add "super" symbols to it
                        sourceFile.containedScope.this.set(k,v);
                    }
                    if (!sourceFile.containedScope.super.has(k)) { // super starts life in this method, and only gets "super" symbols
                        sourceFile.containedScope.super.set(k,v);
                    }
                }
            }
            ancestor = ancestor.cfc?.extends;
        }
    }
    */

    function setNoUndefinedVars(newVal: boolean) : void {
        noUndefinedVars = newVal;
    }

    function typeErrorAtRange(range: SourceRange, msg: string) : void {
        const freshDiagnostic : Diagnostic = {
            fromInclusive: range.fromInclusive,
            toExclusive: range.toExclusive,
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

    function typeErrorAtNode(node: Node, msg: string) {
        typeErrorAtRange(node.range, msg);
    }

    function checkList(nodes: Node[]) {
        for (const node of nodes) {
            checkNode(node);
        }
    }

    function forEach(nodes: Node[], cb: (node: Node) => void) {
        for (let i = 0; i < nodes.length; i++) {
            cb(nodes[i]);
        }
    }

    function checkListFunctionsLast(nodes: Node[]) {
        forEach(nodes.filter(node => node.kind !== NodeKind.functionDefinition), checkNode);
        forEach(nodes.filter(node => node.kind === NodeKind.functionDefinition), checkNode);
    }

    function checkNode(node: Node | null) {
        if (!node) return;

        switch (node.kind) {
            case NodeKind.sourceFile:
                throw "Check source files by binding its content";
            case NodeKind.comment:
                return;
            case NodeKind.textSpan:
                return;
            case NodeKind.terminal:
                return;
            case NodeKind.hashWrappedExpr: // fallthrough
            case NodeKind.parenthetical:   // fallthrough
            case NodeKind.tagAttribute:
                checkNode(node.expr);
                return;
            case NodeKind.tag:
                return;
            case NodeKind.callExpression:
                checkCallExpression(node);
                return;
            case NodeKind.callArgument:
                checkCallArgument(node);
                return;
            case NodeKind.unaryOperator:
                checkUnaryOperator(node);
                return;
            case NodeKind.binaryOperator:
                checkBinaryOperator(node);
                return;
            case NodeKind.conditional:
                checkConditional(node);
                return;
            case NodeKind.variableDeclaration:
                checkVariableDeclaration(node);
                return;
            case NodeKind.statement:
                switch (node.subType) {
                    case StatementType.expressionWrapper: {
                        checkNode(node.expr);
                    }
                }
                return;
            case NodeKind.returnStatement:
                checkReturnStatement(node);
                return;
            case NodeKind.breakStatement:
                return;
            case NodeKind.continueStatement:
                return;
            case NodeKind.block:
                switch (node.subType) {
                    case BlockType.fromTag:
                        // check attrs and etc...
                        checkListFunctionsLast(node.stmtList);
                        return;
                    case BlockType.cLike:
                        checkListFunctionsLast(node.stmtList);
                        return;
                    case BlockType.scriptSugaredTagCallBlock:
                        checkList(node.stmtList);
                        return;
                    case BlockType.scriptTagCallBlock:
                        checkList(node.stmtList);
                        return;
                }
                return;
            case NodeKind.simpleStringLiteral:
                setCachedEvaluatedNodeType(node, createLiteralType(node.textSpan.text));
                return;
            case NodeKind.interpolatedStringLiteral:
                checkList(node.elements);
                setCachedEvaluatedNodeType(node, SyntheticType.string);
                return;
            case NodeKind.numericLiteral:
                setCachedEvaluatedNodeType(node, SyntheticType.numeric);
                return;
            case NodeKind.booleanLiteral:
                setCachedEvaluatedNodeType(node, SyntheticType.boolean);
                return;
            case NodeKind.identifier:
                // klude/fixme!: identifier.source can be an indexed access
                // this was to support `a.b.c = 42`
                // what we need to do is parse that as a "dotted path";
                // if it turns out that the expression is an assignment, keep it that way
                // otherwise, transform it into an indexed-access expression
                // and if during "dotted-path" parsing, if we see that it became
                // a.b.c[1], or similar, transform it into an indexed-access
                checkIdentifier(node);
                return;
            case NodeKind.indexedAccess:
                checkIndexedAccess(node);
                return;
            case NodeKind.indexedAccessChainElement:
                checkIndexedAccessChainElement(node);
                return;
            case NodeKind.sliceExpression:
                if (node.from) checkNode(node.from);
                if (node.to) checkNode(node.to);
                if (node.stride) checkNode(node.stride);
                return;
            case NodeKind.functionParameter:
                if (!node.fromTag && node.defaultValue) checkNode(node.defaultValue);
                return;
            case NodeKind.functionDefinition: // fallthrough
            case NodeKind.arrowFunctionDefinition:
                checkFunctionDefinition(node);
                return;
            case NodeKind.dottedPath:
                return;
            case NodeKind.dottedPathRest:
                // no-op, taken care of by dottedPath
                return;
            case NodeKind.switch:
                checkSwitch(node);
                return;
            case NodeKind.switchCase:
                checkSwitchCase(node);
                return;
            case NodeKind.do:
                checkDo(node);
                return;
            case NodeKind.while:
                checkWhile(node);
                return;
            case NodeKind.ternary:
                checkTernary(node);
                return;
            case NodeKind.for:
                checkFor(node);
                return;
            case NodeKind.structLiteral:
                checkStructLiteral(node);
                return;
            case NodeKind.structLiteralInitializerMember:
                checkStructLiteralInitializerMember(node);
                return;
            case NodeKind.arrayLiteral:
                checkArrayLiteral(node);
                return;
            case NodeKind.arrayLiteralInitializerMember:
                checkArrayLiteralInitializerMember(node);
                return;
            case NodeKind.try:
                checkTry(node);
                return;
            case NodeKind.catch:
                checkCatch(node);
                return;
            case NodeKind.finally:
                checkFinally(node);
                return;
            case NodeKind.importStatement:
                return;
            case NodeKind.new:
                checkNew(node);
                return;
            case NodeKind.typeShim:
                return;
            case NodeKind.property: {
                // if we're in Wirebox mode, and the property has an inject attribute that we can resolve, add the type to the property value
                if (sourceFile.libRefs.has("<<magic/wirebox>>")) {
                    const nameAttrVal = getAttributeValue(node.attrs, "name");
                    const nameStringVal = getTriviallyComputableString(nameAttrVal)?.toLowerCase();
                    const injectAttrVal = getAttributeValue(node.attrs, "inject");
                    const injectStringVal = getTriviallyComputableString(injectAttrVal)?.toLowerCase();
                    if (!injectStringVal || !nameStringVal) return;

                    const mappings = sourceFile.libRefs.get("<<magic/wirebox>>")!.containedScope.typedefs.mergedInterfaces.get("Wirebox")?.members.get("mappings");
                    const targetSymbol = sourceFile.containedScope.variables!.get(nameStringVal);
                    if (!mappings || !targetSymbol || !isStructLike(mappings.type)) return;

                    const cfc = mappings.type.members.get(injectStringVal);
                    if (!cfc) return;

                    targetSymbol.declaredType = cfc.type;
                }
                return;
            }
            case NodeKind.paramStatement:
                return;
            default:
                exhaustiveCaseGuard(node);
        }
    }
/*
    function getNearestScopeByName(node: Node, scopeName: StaticallyKnownScopeName) : Struct | undefined {
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

    /* rmme
    //https://helpx.adobe.com/coldfusion/developing-applications/the-cfml-programming-language/using-coldfusion-variables/about-scopes.html
    const scopeLookupOrder : readonly StaticallyKnownScopeName[] = [
        "local",
        "arguments",
        "__query", // magic inaccessible scope inside a <cfloop query=#q#>...</cfquery> body
        "__transient", // found a quasi-declaration (raw assignment) which we can possibly use; keep climbing for an actual decl, but use this if we find no other
        "thread",
        "variables",
        "cgi",
        "file",
        "url",
        "form",
        "cookie",
        "client"
    ];

    
    function getScopeDisplayMember(scope: ScopeDisplay, canonicalName: string) : SymTabResolution | undefined {
        // could we not do this, if we stored a link to the symTab for nested things ?
        // how would that work for arrays? there wouldn't be a symTab for those...
        const path = canonicalName.split(".");
        if (path.length > 1) {
            if (!isStaticallyKnownScopeName(path[0])) return undefined;
            const scopeName = path[0];
            if (!scope.hasOwnProperty(scopeName) || !scope[scopeName as StaticallyKnownScopeName]!.get(path[1])) return undefined;
            let current = scope[scopeName as StaticallyKnownScopeName]!.get(path[1])!;
            for (let i = 2; i < path.length; i++) {
                if (!isStructLike(current.type)) return undefined;
                let maybeNext = current.type.members.get(path[i]);
                if (!maybeNext) return undefined;
                current = maybeNext;
            }
            return {scopeName: path[0], symTabEntry: current};
        }

        for (const scopeName of scopeLookupOrder) {
            if (scope.hasOwnProperty(scopeName)) {
                const entry = scope[scopeName]!.get(canonicalName);
                if (entry) {
                    return {scopeName: scopeName, symTabEntry: entry};
                }
            }
        }
        return undefined;
    }

    
    function walkupScopesToResolveSymbol(base: Node, canonicalName: string) : SymbolResolution | undefined {
        const engineSymbol = engineSymbolResolver(canonicalName);
        let transientLocal : SymbolResolution | undefined = undefined;
        let node : Node | null = base;

        while (node) {
            if (node.containedScope) {
                const varEntry = getScopeDisplayMember(node.containedScope, canonicalName);
                if (varEntry) {
                    (varEntry as SymbolResolution).container = node;
                    if (engineSymbol) varEntry.alwaysVisibleEngineSymbol = engineSymbol;

                    if (varEntry.scopeName === "__transient") {
                        transientLocal = varEntry as SymbolResolution; // keep climbing for an actual declaration; if we don't find one, we'll use this
                    }
                    else {
                        return varEntry as SymbolResolution;
                    }
                }

                if (node.kind === NodeKind.sourceFile) {
                    return engineSymbol
                        ? {scopeName: "__cfEngine", symTabEntry: engineSymbol, container: null}
                        : transientLocal;
                }

                else {
                    node = node.containedScope.container;
                }
            }
            else {
                node = node.parent;
            }
        }

        return transientLocal;
    }
    */

    // function determineFlowType(base: Node, canonicalName: string) : _Type | undefined {
    //     base;
    //     canonicalName;
    //     getCachedEvaluatedFlowType;

    //     return SyntheticType.any;

    //     // let node: Node | null = base;
    //     // let flow: Flow | null = null;
    //     // while (node) {
    //     //     if (node.flow) {
    //     //         flow = node.flow;
    //     //         break;
    //     //     }
    //     //     else {
    //     //         node = node.parent;
    //     //     }
    //     // }

    //     // if (!flow) { // we'll be defensive, but we should always get a Flow (a "root flow") from SourceFile
    //     //     return undefined;
    //     // }

    //     // const usageContainer : Node = findAncestor(base, (node) => node?.kind === NodeType.functionDefinition || node?.kind === NodeType.sourceFile)!;
    //     // const usageContainerSymbol = getScopeDisplayMember(usageContainer.containedScope!, canonicalName);
    //     // // if the usage container does not contain a symbol table entry for this name, then it is an "outer" variable, defined in some outer scope
    //     // // walk up until we find it, and use it's non-flow based type
    //     // if (!usageContainerSymbol) {
    //     //     let node : Node | null = usageContainer.containedScope!.container; 
    //     //     while (node) {
    //     //         const symbol = getScopeDisplayMember(node.containedScope!, canonicalName);
    //     //         if (symbol) return symbol.symTabEntry.type;
    //     //         node = node.containedScope!.container;
    //     //     }
    //     //     return undefined;
    //     // }

    //     // return work(flow);

    //     // function work(flow: Flow) : _Type | undefined {
    //     //     const type = getCachedEvaluatedFlowType(flow, canonicalName);

    //     //     // we got a type at this flow
    //     //     if (type) {
    //     //         return type;
    //     //     }
    //     //     else if (flow.node?.containedScope) {
    //     //         const scopeMember = getScopeDisplayMember(flow.node.containedScope, canonicalName);
    //     //         if (scopeMember) {
    //     //             // no type at this flow, but the name is defined on this scope;
    //     //             // stop walking upwards, because the current flow's node is the top-most container for this name,
    //     //             // even if there is a predecessor flow

    //     //             if (isFunctionSignature(scopeMember.symTabEntry.type)) { // a function definition is always defined
    //     //                 return scopeMember.symTabEntry.type;
    //     //             }

    //     //             if (flow.node === usageContainer) {
    //     //                 return undefined; // it's defined on the closest containing scope, but hasn't been definitely assigned yet
    //     //             }
    //     //             else {
    //     //                 return scopeMember.symTabEntry.type; // it's defined in an outer scope
    //     //             }
    //     //         }
    //     //         if (flow.node.kind === NodeType.functionDefinition) {
    //     //             // we hit a containing function, and have not yet figured out the type
    //     //             // check the rest of the grand-parent's body 
    //     //             // but before we defer, check all parent containers to see if they even contain this name
    //     //             if (walkupScopesToResolveSymbol(flow.node, canonicalName)) {
    //     //                 return undefined;
    //     //             }
    //     //         }
    //     //     }

    //     //     // if we got to root and didn't find it, see if we can find it in stdlib (if stdlib was available)
    //     //     if (flow.node?.kind === NodeType.sourceFile) {
    //     //         return checkLibRefsForName(canonicalName)?.type;
    //     //     }

    //     //     if (flow.predecessor.length === 1) {
    //     //         return work(flow.predecessor[0]);
    //     //     }
    //     //     else {
    //     //         let containsUndefined = false;
    //     //         const flowTypes : _Type[] = [];
    //     //         for (let i = 0; i < flow.predecessor.length; i++) {
    //     //             const type = work(flow.predecessor[i]);
    //     //             if (type) flowTypes.push(type);
    //     //             else containsUndefined = true;
    //     //         }
    //     //         return flowTypes.length === 0 ? undefined : unionify(flowTypes, containsUndefined);
    //     //     }
    //     // }
    // }

    // needs to merge 2+ unions, dedup, etc.
    // union type needs to be a Set<_Type>
    // "instantiated type": {flags: TypeFlag, type: _Type} so we can share "any" and "void" and etc.
    function unionify(types: _Type[], definitelyContainsUndefined = false) : _Type {
        let flags : TypeFlags = definitelyContainsUndefined ? TypeFlags.containsUndefined : TypeFlags.none;
        const membersBuilder : _Type[] = [];
        for (const type of types) {
            if (type.flags & TypeFlags.containsUndefined) {
                flags |= TypeFlags.containsUndefined;
            }
            else if (type.flags & TypeFlags.any || type.flags & TypeFlags.never) {
                return type;
            }
            else {
                membersBuilder.push(type);
            }
        }

        if (membersBuilder.length === 0) return SyntheticType.any;

        return cfUnion(membersBuilder, flags);
    }

    function unsafeAssertTypeKind<T extends _Type>(_type: _Type) : asserts _type is T {}

    /**
     * `to = assignThis` is a valid assignment if `assignThis` is a subtype of `to`
     * @param assignThis 
     * @param to 
     */
    function isAssignable(assignThis: _Type, to: _Type) : boolean {
        return isLeftSubtypeOfRight(assignThis, to);
    }

    //
    // for an annotated function definition, the user will generally want to refine existing types
    // e.g for something like
    // struct function foo(struct arg) {}
    // we would probably like to specify with an annotation that we are accepting/returning some subtype of struct (which by itself is just {}, the empty interface)
    // so we just check every param and the return type in a covariant way (i.e that the annotated type params and return type are covariant with respect to the cf types)
    // also, because a user is forced by the CF syntax to specify the param names in the actual function definition, we do not check annotated param names
    // (otherwise we would have to write them twice for no benefit)
    //
    function isAnnotatedSigCompatibleWithCfFunctionSig(annotatedSig: cfFunctionSignature, cfSig: cfFunctionSignature) {
        // covariant in return type
        if (!isLeftSubtypeOfRight(annotatedSig.returns, cfSig.returns)) return false;
        
        if (cfSig.params.length != annotatedSig.params.length) return false;

        for (let i = 0; i < cfSig.params.length; i++) {
            // covariant param check
            if (!isAssignable(annotatedSig.params[i].type, cfSig.params[i].type)) return false;
        }
        return true;
    }

    //
    // `l <: r` means "left is a subtype of right"
    // `l !<: r` means "left is NOT a subtype of right"
    // subtype has the common meaning; however it is maybe helpful to note that "sub" also means "substitutable" in addition to "a descendant in a heirarchy"
    // i.e. `l <: r` means l is substitutable for r (you can safely use an l in r's place)
    //
    function isLeftSubtypeOfRight(l: _Type, r: _Type) : boolean {
        let depth = 0;
        let tooDeep = false;
        tooDeep ? 0 : 1; // yes compiler this is used

        return worker(l, r);

        function worker(l: _Type, r: _Type) : boolean {
            if (depth > 32) {
                //debugger;
                tooDeep = true;
                return false;
            }
            try {
                depth++;

                // a type is a subtype of itself
                if (l === r) return true;

                // any is a subtype of every type; every type is a subtype of any
                if (l.flags & TypeFlags.any || r.flags & TypeFlags.any) return true;

                // void is not a subtype of anything except itself and any
                if (l.flags & TypeFlags.void || r.flags & TypeFlags.void) return false;

                // it would be nice to error on this, but plenty of legacy code relies on
                // number being assignable to boolean
                if (l.flags & TypeFlags.numeric && r.flags & TypeFlags.boolean) return true;

                if (isLiteralType(l) && isLiteralType(r)) return l.literalValue === r.literalValue;
                if (!isLiteralType(l) && isLiteralType(r)) return false; // number is not a subtype of `0`

                // numeric is a subtype of string
                // however, string is not a subtype of numeric
                if (l === SyntheticType.numeric && r === SyntheticType.string) {
                    return true;
                }

                //
                // {x: number, y: number} <: {x: number}, because L has AT LEAST all the properties of R
                // {x: number} !<: {x: number, y: number}
                //
                function isLeftStructSubtypeOfRightStruct(l: Struct, r: Struct) {
                    // the outer types may be different wrappers for the same underlying struct
                    // e.g. a SymbolTableWrapper around `this` from within a CFC, and a CFC wrapper wrapping the same `this` but for a reference from another file
                    if (l.members === r.members) return true;

                    if (l.members.size < r.members.size) return false;

                    // if both types are CFCs, we compare by nominal inheritance
                    // we probably need to fix that this is valid CF:
                    /*
                        // Parent.cfc
                        component {
                            Parent function init() { return this; }
                        }
                        // Child.cfc
                        component {
                            Child function init() { return super.init(); } // super.init : Parent, but Parent :> Child, we could probably treat "super.init as this.init"
                        }
                    */
                    if (l.structKind === StructKind.cfcTypeWrapper && r.structKind === StructKind.cfcTypeWrapper) {
                        return !!findAncestor(l.cfc, (node) => node === r.cfc, /*followCfcInheritance*/ true);
                    }

                    for (const [propName, rightVal] of r.members) {
                        const leftVal = l.members.get(propName);
                        if (!leftVal || !worker(leftVal.type, rightVal.type)) return false;
                    }
                    return true;
                }

                if (l.flags & TypeFlags.numeric && r.flags & TypeFlags.numeric) {
                    return true;
                }
                if (l.flags & TypeFlags.string && r.flags & TypeFlags.string) {
                    return true;
                }
                if (l.flags & TypeFlags.boolean && r.flags & TypeFlags.boolean) {
                    return true;
                }
                if (isStructLike(l) && isStructLike(r)) {
                    return isLeftStructSubtypeOfRightStruct(l, r);
                }
                if (isArray(l) && isArray(r)) {
                    return worker(l.memberType, r.memberType);
                }
                if (isFunctionSignature(l) && isFunctionSignature(r)) {
                    // covariant in return type
                    if (!worker(l.returns, r.returns)) return false;

                    // contravariant in parameter types
                    // also every parameter needs the same names...?

                    const hasSpread = !!(r.params.length > 0 && r.params[r.params.length-1].flags & TypeFlags.spread);
                    if (!hasSpread && l.params.length != r.params.length) return false;

                    for (let i = 0; i < l.params.length; i++) {
                        // if the arg in the right side is a spread, just check remaining left types against the (un-array-wrapped) spread type
                        if (r.params[i].flags & TypeFlags.spread) {
                            const rType = r.params[i].type;
                            const spreadType = isArray(rType) ? rType.memberType : null;
                            if (!spreadType) return false; // all spread types must be arrays, we should have caught this before getting here
                            for (let j = i; j < l.params.length; j++) {
                                const lpt = l.params[i].type;
                                // contravariant, flip left/right
                                if (!worker(spreadType, lpt)) return false;
                            }

                            break;
                        }

                        const lpt = l.params[i].type;
                        const rpt = r.params[i].type;
                        // contravariant, flip left/right
                        if (!worker(rpt, lpt)) return false;
                    }
                    return true;
                }
                //
                // S|T <: U     iff S <: U && T <: U   (e.g `U = S|T` is valid only if both S and T are subtypes of U)
                // U   <: S|T   iff U <: S || U <: T   (e.g `S|T = U` is valid if U is either an S or a T)
                //
                // S&T <: U     iff S <: U || T <: U   (e.g `U = S&T` is valid if either S or T is a subtype of U)
                // U   <: S&T   iff U <: S && U <: T   (e.g `S&T = U` is valid only if U is a subtype of both S and T)
                //
                if (isUnion(l)) {
                    for (const leftConstituent of l.types) {
                        if (!worker(leftConstituent, r)) return false;
                    }
                    return true;
                }
                if (isUnion(r)) {
                    for (const rightConstituent of r.types) {
                        if (worker(l, rightConstituent)) return true;
                    }
                    return false;
                }
                if (isIntersection(l)) {
                    for (const leftConstituent of l.types) {
                        if (worker(leftConstituent, r)) return true;
                    }
                    return false;
                }
                if (isIntersection(r)) {
                    for (const rightConstituent of r.types) {
                        if (!worker(l, rightConstituent)) return false;
                    }
                    return true;
                }

                return false;
            }
            finally {
                depth--;
            }
        }
    }

    /*
    function pushTypesIntoInlineFunctionDefinition(context: Node, signature: cfFunctionSignature, functionDef: (FunctionDefinition | ArrowFunctionDefinition)) {
        const existingArgumentsScope = functionDef.containedScope!.arguments!;
        for (let i = 0; i < signature.params.length; i++) {
            if (i === functionDef.params.length) {
                break;
            }
            if (existingArgumentsScope.has(signature.params[i].canonicalName)) {
                existingArgumentsScope.get(signature.params[i].canonicalName)!.type = evaluateType(context, signature.params[i].type);
            }
        }
    }
    */

    function chooseOverload(overloadSet: cfFunctionOverloadSet, args: CallArgument[]) {
        const availableOverloads = new Set(overloadSet.overloads);
        for (let i = 0; i < args.length; i++) {
            const paramNum = i+1;
            const arg = args[i];
            const argType = getCachedEvaluatedNodeType(arg.expr);

            const deleteableOverloads = new Set<cfFunctionOverloadSet["overloads"][number]>();
            for (const overload of availableOverloads) {
                if (overload.params.length < paramNum) {
                    deleteableOverloads.add(overload);
                    continue;
                }
                const paramType = overload.params[i].type;
                if (!isAssignable(/*assignThis*/argType, /*to*/paramType)) {
                    deleteableOverloads.add(overload);
                    continue;
                }
            }
            for (const deleteable of deleteableOverloads) {
                availableOverloads.delete(deleteable);
            }
        }

        return [...availableOverloads];
    }

    function checkCallExpression(node: CallExpression) {
        checkNode(node.left);
        checkList(node.args);

        // the resolved symbol may shadow a built-in always-visible engine symbol
        // function call identifiers always call the built-in if it names a built-in
        // e.g function `foo(string encodeForHtml) { return encodeForHTML(encodeForHTML); }` is valid,
        // invoking the builtin function 'encodeForHTML' on the string argument 'encodeForHTML'
        // if the above is true, checking has cached the non-engine type, and we need to override that decision
        const symbol = getResolvedSymbol(node.left);
        if (symbol?.alwaysVisibleEngineSymbol || symbol?.scopeName === "__cfEngine") { // are we shadowing an alwaysVisible symbol, or this symbol is directly a built-in type?
            const sig = symbol.alwaysVisibleEngineSymbol?.type ?? symbol.symTabEntry.type;
            if (isFunctionSignature(sig)) { // what else could it be here?
                setCachedEvaluatedNodeType(node.left, sig);
                setCachedEvaluatedNodeType(node, sig.returns);
            }
            // for now we won't check that the args to built-ins are correct (have the right names / types etc.), because we don't have great definitions for them
            return;
        }

        const sig = getCachedEvaluatedNodeType(node.left);

        if (isFunctionOverloadSet(sig)) {
            const overloads = chooseOverload(sig, node.args);
            if (overloads.length !== 1) {
                setCachedEvaluatedNodeType(node, SyntheticType.any);
            }
            else {
                setCachedEvaluatedNodeType(node, overloads[0].returns);
            }
        }
        else if (isFunctionSignature(sig)) {
            checkCallLikeArguments(sig, node);
            setCachedEvaluatedNodeType(node, sig.returns);
        }
        else if (sig.flags & TypeFlags.any) {
            return;
        }
        else {
            typeErrorAtNode(node.left, `Type '${stringifyType(sig)}' is not callable.`);
        }
    }

    function checkCallLikeArguments(sig: _Type, node: CallExpression) : void {
        if (sig.flags & TypeFlags.any) {
            return;
        }
        if (isFunctionSignature(sig)) {
            const isNewExpr = node.parent?.kind === NodeKind.new;

            const minRequiredParams = sig.params.filter(param => !(param.flags & TypeFlags.optional)).length;
            // maxParams is undefined if there was a spread param, since it accepts any number of trailing args
            const maxParams = sig.params.length > 0 && sig.params[sig.params.length - 1].flags & TypeFlags.spread
                ? undefined
                : sig.params.length;

            const namedArgCount = node.args.filter(arg => !!arg.equals).length;
            let hasArgumentCollectionArg = false;

            if (namedArgCount > 0) {
                if (namedArgCount !== node.args.length) {
                    typeErrorAtRange(mergeRanges(node.leftParen, node.args, node.rightParen), "All arguments must be named, if any are named.");
                }

                const paramNameMap = new Map<string, {uiName: string, param: cfFunctionSignatureParam}>();
                for (const param of sig.params) {
                    paramNameMap.set(param.canonicalName, {uiName: param.uiName, param});
                }
                const seenArgs = new Set<string>();
                for (const arg of node.args) {
                    const argName = arg.name?.kind === NodeKind.identifier
                        ? stringifyLValue(arg.name)
                        : isSimpleOrInterpolatedStringLiteral(arg.name)
                        ? stringifyStringAsLValue(arg.name)
                        : undefined;
                    if (arg.name && argName) {
                        hasArgumentCollectionArg = hasArgumentCollectionArg || (argName.canonical === "argumentcollection");
                        if (seenArgs.has(argName?.canonical)) {
                            const uiName = argName.ui || argName.canonical;
                            typeErrorAtNode(arg.name, `Duplicate argument '${uiName}'`);
                        }

                        const paramPair = paramNameMap.get(argName.canonical);
                        if (!paramPair) {
                            if (argName.canonical !== "argumentcollection") {
                                const uiName = argName.ui || argName.canonical;
                                typeErrorAtNode(arg.name, `'${uiName}' is not a recognized parameter for this ${isNewExpr ? "constructor" : "function"}.`);
                            }
                        }
                        else {
                            const argType = getCachedEvaluatedNodeType(arg);
                            const paramType = paramPair.param.type;
                            if (!isAssignable(argType, paramType)) {
                                typeErrorAtNode(arg, `Argument of type '${stringifyType(argType)}' is not assignable to parameter of type '${stringifyType(paramType)}'.`);
                            }
                        }
                        
                        seenArgs.add(argName.canonical);
                    }
                }

                // make sure that all required named parameters have been provided
                // this is skipped if there was an argumentCollection argument
                // it is valid to provide both an argumentCollection argument and OTHER named arguments, but the runtime behavior will be non-sensical
                // because it is valid to do so, we unfortunately cannot error on it; a linter could though (and also flag if a function defintion has "argumentCollection" as a parameter name)
                if (!hasArgumentCollectionArg) {
                    const requiredNamedParams = new Map(
                        sig.params.filter(param => !(param.flags & TypeFlags.optional) && param.canonicalName)
                        .map(param => [param.canonicalName, param.uiName]));

                    for (const seenArg of seenArgs) {
                        if (requiredNamedParams.has(seenArg)) requiredNamedParams.delete(seenArg)
                    }
                    if (requiredNamedParams.size > 0) {
                        const missingNamedParams = [...requiredNamedParams.values()].map(uiName => "'" + uiName + "'").join(", ");
                        typeErrorAtNode(node.left, `Required named parameters are missing: ${missingNamedParams}`);
                    }
                }
            }
            else {
                const spreadAdjust = maxParams === undefined ? -1 : 0; // if there was a trailing spread arg, drop the spread count by 1
                const minToCheck = Math.min(node.args.length, sig.params.length + spreadAdjust);
                for (let i = 0; i < minToCheck; i++) {
                    const argType = getCachedEvaluatedNodeType(node.args[i]);
                    const paramType = sig.params[i].type;
                    if (!isAssignable(argType, paramType)) {
                        typeErrorAtNode(node.args[i], `Argument of type '${stringifyType(argType)}' is not assignable to parameter of type '${stringifyType(paramType)}'.`);
                    }
                }
            }

            if (!hasArgumentCollectionArg && (node.args.length < minRequiredParams || (maxParams !== undefined && node.args.length > maxParams))) {
                let msg;
                if (minRequiredParams !== maxParams) {
                    if (maxParams === undefined) msg = `Expected at least ${minRequiredParams} arguments, but got ${node.args.length}`;
                    else msg = `Expected between ${minRequiredParams} and ${maxParams} arguments, but got ${node.args.length}`;
                }
                else msg = `Expected ${maxParams} arguments, but got ${node.args.length}`;
                typeErrorAtRange(mergeRanges(node.leftParen, node.args, node.rightParen), msg);
            }

            for (let i = 0; i < node.args.length; i++) {
                const paramType = sig.params[i]?.type ?? null;
                if (!paramType) break;
                const arg = node.args[i];
                const argType = getCachedEvaluatedNodeType(arg);
                if (!isAssignable(/*assignThis*/ argType, /*to*/ paramType)) {
                    // error
                }
                if (isFunctionSignature(paramType) && (arg.expr.kind === NodeKind.functionDefinition || arg.expr.kind === NodeKind.arrowFunctionDefinition)) {
                    //pushTypesIntoInlineFunctionDefinition(node, paramType, arg.expr);
                    //checkNode(arg.expr.body as Node /*fixme: we know this is a script function definition but can't prove it here; anyway, all function defs should have Node as a body, not Node|Node[] ?*/);
                }
            }
        }
        else {
            // error
        }
    }

    function checkCallArgument(node: CallArgument) {
        checkNode(node.expr);
        setCachedEvaluatedNodeType(node, getCachedEvaluatedNodeType(node.expr));
    }

    function checkUnaryOperator(node: UnaryOperator) {
        checkNode(node.expr);
        const type = getCachedEvaluatedNodeType(node.expr);
        if (!(type.flags & TypeFlags.any) && !(type.flags & TypeFlags.numeric)) {
            // true for ++/--
            //typeErrorAtNode(node.expr, "Unary operator requires a numeric operand.");
            // need "coercible to bool" for "!"
        }
    }

    function checkBinaryOperator(node: BinaryOperator) {
        checkNode(node.left);
        checkNode(node.right);

        switch (node.optype) {
            case BinaryOpType.assign: {
                // an assignment, even fv-unqualified, will always be bound to a scope
                // `x = y` is effectively `variables.x = y`
                if (node.left.kind === NodeKind.identifier || node.left.kind === NodeKind.indexedAccess) {
                    const lValIdent = stringifyLValue(node.left);
                    if (!lValIdent) return;

                    const lhsType = walkupScopesToResolveSymbol(node, lValIdent.canonical)?.symTabEntry.declaredType; //determineFlowType(node.left, lValIdent.canonical);
                    const rhsType = getCachedEvaluatedNodeType(node.right);
                    if (!lhsType) {
                        /*
                        // there is no type in the current flow; so, this is the first assignment for this var in this scope
                        if (node.typeAnnotation) {
                            const evaluatedTypeAnnotation = evaluateType(node, node.typeAnnotation);
                            if (!isAssignable(rhsType, node.typeAnnotation)) {
                                //typeErrorAtNode(node.right, "RHS is not assignable to LHS.");
                            }
                            setCachedEvaluatedFlowType(node.left.flow!, lValIdent.canonical, evaluatedTypeAnnotation);
                        }
                        else {
                            setCachedEvaluatedFlowType(node.left.flow!, lValIdent.canonical, rhsType);
                        }*/
                    }
                    else {
                        if (node.typeAnnotation) {
                            typeErrorAtNode(node, "_Type annotations can only be bound to an identifier's first assignment.");
                        }
                        if (isAssignable(/*assignThis*/rhsType, /*to*/lhsType)) {
                            setCachedEvaluatedFlowType(node.left.flow!, lValIdent.canonical, rhsType);
                        }
                        else {
                            const l = stringifyType(lhsType);
                            const r = stringifyType(rhsType);
                            typeErrorAtNode(node.right, `Type '${r}' is not assignable to type '${l}'`);
                        }
                    }
                }

                break;
            }
            case BinaryOpType.assign_cat:
            case BinaryOpType.contains:
            case BinaryOpType.does_not_contain:
            case BinaryOpType.cat: {
                /*
                const leftType = getCachedEvaluatedNodeType(node.left);
                const rightType = getCachedEvaluatedNodeType(node.right);
                if (leftType.typeKind !== TypeKind.any && leftType.typeKind !== TypeKind.string) {
                    //typeErrorAtNode(node.left, `Left operand to '${BinaryOpTypeUiString[node.optype]}' operator must be a string.`);
                }
                if (rightType.typeKind !== TypeKind.any && rightType.typeKind !== TypeKind.string) {
                    //typeErrorAtNode(node.right, `Right operand to '${BinaryOpTypeUiString[node.optype]}' operator must be a string.`);
                }
                */
                break;
            }
            case BinaryOpType.eq:
            case BinaryOpType.neq:
            case BinaryOpType.equivalent:
            case BinaryOpType.implies: {
                break;
            }
            default: { // all other operators are (number op number)
                /*
                const leftType = getCachedEvaluatedNodeType(node.left);
                const rightType = getCachedEvaluatedNodeType(node.right);
                // acf allows (bool) + (bool), but maybe we don't want to support that
                if (leftType.typeKind !== TypeKind.any && leftType.typeKind !== TypeKind.number) {
                    //typeErrorAtNode(node.left, `Left operand to '${BinaryOpTypeUiString[node.optype]}' operator must be a number.`);
                }
                if (rightType.typeKind !== TypeKind.any && rightType.typeKind !== TypeKind.number) {
                    //typeErrorAtNode(node.right, `Right operand to '${BinaryOpTypeUiString[node.optype]}' operator must be a number.`);
                }
                */
            }
        }
    }

    function checkConditional(node: Conditional) {
        if (node.subType === ConditionalSubtype.if || node.subType === ConditionalSubtype.elseif) {
            if (node.fromTag) checkNode((node.tagOrigin.startTag as CfTag.ScriptLike).expr);
            else checkNode(node.expr);
            checkNode(node.consequent);
            checkNode(node.alternative);
        }
        else {
            checkNode(node.consequent);
        }
    }

    // unused but called
    function setCachedEvaluatedFlowType(flow: Flow, name: string, type: _Type) : void {
        flow; name; type;
        return;
        // if (!sourceFile.cachedFlowTypes.has(flow.flowId)) {
        //     sourceFile.cachedFlowTypes.set(flow.flowId, new Map());
        // }
        // sourceFile.cachedFlowTypes.get(flow.flowId)!.set(name, type);
    }

    // function getCachedEvaluatedFlowType(flow: Flow, name: string) : _Type | undefined {
    //     return sourceFile.cachedFlowTypes.get(flow.flowId)?.get(name);
    // }

    function checkReturnStatement(node: ReturnStatement) {
        checkNode(node.expr);
        const func = getContainingFunction(node);
        if (!func) {
            const errNode = node.fromTag ? node.tagOrigin.startTag! : node.returnToken!;
            typeErrorAtNode(errNode, "A return statement must be contained inside a function body.");
            return;
        }

        const sig = getCachedEvaluatedNodeType(func);
        if (!sig || !isFunctionSignature(sig) || sig.returns.flags & TypeFlags.any) {
            return;
        }

        const exprType = node.expr ? getCachedEvaluatedNodeType(node.expr) : SyntheticType.void_;

        if (!isAssignable(exprType, sig.returns)) {
            // if we got an exprType, we got an expr or a just return token; if this is from tag, we definitely got a tag
            const errNode = node.fromTag ? node.tagOrigin.startTag! : (node.expr || node.returnToken!);
            typeErrorAtNode(errNode, `Type '${stringifyType(exprType)}' is not compatible with expected return type '${stringifyType(sig.returns)}'`);
        }
    }

    // `var x` without an initializer is valid in `for (var x in y)`;
    // otherwise it should always be `var x = ...`
    // both are variable declarations
    // we would like to extract `x` out of the above in both cases
    // note that `for (var x.y in z)` is valid code, and there was recently a complaint on the ACF bug tracker that `for (var x['y'] in z)` should also be valid
    // these would effectively declare `var x = {y: ?}` 
    function getVariableDeclarationLValue(node: VariableDeclaration) : Identifier | IndexedAccess | undefined {
        let workingNode : Node = node.expr;
        if (workingNode.kind === NodeKind.binaryOperator) workingNode = workingNode.left;
        if (workingNode.kind === NodeKind.identifier || workingNode.kind === NodeKind.indexedAccess) return workingNode;
        else return undefined;
    }    

    function checkVariableDeclaration(node: VariableDeclaration) : void {
        const lValue = getVariableDeclarationLValue(node);
        if (!lValue) return;
        const name = stringifyLValue(lValue);
        if (!name) return;
        const canonicalPath = name.canonical.split(".");
        if (canonicalPath.length > 2) return;

        const isForInit = node.parent?.kind === NodeKind.for &&
            ((node.parent.subType === ForSubType.forIn && node === node.parent.init) ||
             (node.parent.subType === ForSubType.for && node === node.parent.initExpr));

        if (isForInit && node.finalModifier) {
            typeErrorAtRange(mergeRanges(node.finalModifier, node.expr), `final-qualified declaration in a for initializer will fail at runtime.`);
        }

        if (engineVersion.engine === Engine.Adobe && canonicalPath.length === 1) {
            const enclosingFunction = getContainingFunction(node);
            if (enclosingFunction) {
                for (const param of enclosingFunction.params) {
                    if (param.canonicalName === name.canonical) {
                        typeErrorAtNode(node, `Identifier '${name.ui}' is already declared in arguments scope.`)
                    }
                }
            }
        }

        let rhsType : _Type | undefined = undefined;
        let assignabilityErrorNode : Node;

        if (node.expr.kind === NodeKind.binaryOperator) {
            checkNode(node.expr.right);
            rhsType = getCachedEvaluatedNodeType(node.expr.right);
            assignabilityErrorNode = node.expr.right;
        }
        else if (isForInit && node.parent?.kind === NodeKind.for && node.parent.subType === ForSubType.forIn && node === node.parent.init) {
            rhsType = getCachedEvaluatedNodeType(node.parent.expr); // `for (x in y)`, x gets its type from `y`
            assignabilityErrorNode = node;
        }
        else {
            return; // should be unreachable; a variable delcaration is either initialized (var x = y) or a for-in expr (for x in y)
        }

        // check for rebinding of final vars
        // let hasOtherFinalDecl = false;

        const symbol = walkupScopesToResolveSymbol(node, name.canonical)?.symTabEntry;
        // if (symbol && symbol.declarations) {
        //     hasOtherFinalDecl = filterNodeList(symbol.declarations, (decl) => decl.kind === NodeType.variableDeclaration && !!decl.finalModifier && decl !== node).length > 0;
        // }

        // if (hasOtherFinalDecl) {
        //     // this would maybe work in a block scoped context but not function scoped
        //     // we need flow analysis to get it right
        //     //typeErrorAtNode(lValue, `Cannot rebind identifier '${name.ui}', which was declared final.`);
        //     return;
        // }

        //if (node.finalModifier) rhsType.flags |= TypeFlags.final; // need to clone the type
        
        if (symbol && symbol.declaredType) {
            if (!isAssignable(rhsType, symbol.declaredType)) {
                const l = stringifyType(symbol.declaredType);
                const r = stringifyType(rhsType);
                typeErrorAtNode(assignabilityErrorNode, `Type '${r}' is not assignable to type '${l}'`);
            }
        }

        setCachedEvaluatedFlowType(lValue.flow!, name.canonical, rhsType);
    }

    // fixme: needs to take a SourceFile arg or that defaults to our current or ...
    function getCachedEvaluatedNodeTypeImpl(node: Node | null, workingSourceFile: SourceFile) : _Type {
        if (!node) {
            return SyntheticType.any;
        }

        let targetId = node.nodeId;

        /*if (node.kind === NodeType.indexedAccess) {
            targetId = node.accessElements[node.accessElements.length-1].nodeId;
        }*/

        if (workingSourceFile.cachedNodeTypes.has(targetId)) {
            return workingSourceFile.cachedNodeTypes.get(targetId)!;
        }
        else {
            return SyntheticType.any;
        }
    }

    function getCachedEvaluatedNodeType(node: Node | null) {
        return getCachedEvaluatedNodeTypeImpl(node, sourceFile);
    }

    function setCachedEvaluatedNodeType(node: Node, type: _Type) {
        if (isTypeConstructor(type) 
            || isTypeConstructorInvocation(type)
            || isCachedTypeConstructorInvocation(type)) {
            typeErrorAtNode(node, "_Type is not concrete.");
            sourceFile.cachedNodeTypes.set(node.nodeId, SyntheticType.any);
            return SyntheticType.any;
        }
        else {
            sourceFile.cachedNodeTypes.set(node.nodeId, type);
            return type;
        }
    }

    function getSymbolImpl(node: Node | null, workingSourceFile: SourceFile) : SymbolResolution | undefined {
        if (!node) return undefined;
        return workingSourceFile.nodeToSymbol.get(node.nodeId);
    }

    function getResolvedSymbol(node: Node | null) {
        return getSymbolImpl(node, sourceFile);
    }

    function setResolvedSymbol(node: Node, resolvedSymbol: SymbolResolution) : void {
        sourceFile.nodeToSymbol.set(node.nodeId, resolvedSymbol);
    }

    function isStructOrArray(type: _Type) : boolean {
        return isStructLike(type)
            || isArray(type)
            || (isIntersection(type) && type.types.some(type => isStructOrArray(type)))
            || (isUnion(type) && type.types.every(type => isStructOrArray(type)))
    }

    // take a SymbolTable and wrap it in a Struct, so we can bridge the gap between those worlds, mostly for the 
    // sake of completions; a "scope" in CF is essentially a struct, but not quite; and vice versa; so the abstraction is not perfect but it's close
    // we cache and reuse already-wrapped sybmol tables so that we can compare types by identity
    function structViewOfScope(scopeContents: SymbolTable, interfaceExtension?: Interface) : Struct {
        if (!interfaceExtension) interfaceExtension = SyntheticType.EmptyInterface;
        if (structViewCache.has(scopeContents, interfaceExtension)) {
            return structViewCache.get(scopeContents, interfaceExtension)!;
        }
        else {
            const type = SymbolTableTypeWrapper(scopeContents, interfaceExtension);
            structViewCache.set(scopeContents, interfaceExtension, type);
            return type;
        }
        
    }

    // fixme: consider libraries? walk into parent cfcs?
    function walkupToFindInterfaceDefinition(name: string, node: Node) : Interface | undefined {
        let working : Node | null = node;
        while (working) {
            if (working.containedScope) {
                if (working.containedScope.typedefs.mergedInterfaces.has(name)) {
                    return working.containedScope.typedefs.mergedInterfaces.get(name)!;
                }
                working = working.containedScope.parentContainer;
            }
            else {
                working = working.parent;
            }
        }
        return undefined;
    }

    function checkIdentifier(node: Identifier) {
        // if we're on the lefthand side of a non-fv qualified assignment, we're done
        // an fv-qualified assignment is handled by checkVariableDeclaration
        // assignment should alter the type of the variable for this flow in checkBinaryOperator
        if (node.parent?.kind === NodeKind.binaryOperator && node.parent.optype === BinaryOpType.assign && node === node.parent.left) {
            return;
        }

        const name = node.canonicalName;

        if (name !== undefined) {
            //const useContainer = getContainer(node);

            if (isStaticallyKnownScopeName(name)) {
                switch (name) {
                    case "local":
                    case "arguments": {
                        const containingFunction = getContainingFunction(node);
                        if (!containingFunction) {
                            // warn about local/arguments use outside of function
                            return;
                        }
                        const possiblyEmptyInterfaceExtension = walkupToFindInterfaceDefinition(name, containingFunction);
                        setCachedEvaluatedNodeType(node, structViewOfScope(containingFunction.containedScope![name]!, possiblyEmptyInterfaceExtension));
                        break;
                    }
                    case "variables":
                    case "this":
                    case "super": {
                        const sourceFile = getSourceFile(node)!;
                        if (sourceFile.cfFileType !== CfFileType.cfc) {
                            return;
                        }
                        const possiblyEmptyInterfaceExtension = walkupToFindInterfaceDefinition(name, node);
                        setCachedEvaluatedNodeType(node, structViewOfScope(sourceFile.containedScope[name]!, possiblyEmptyInterfaceExtension));
                        break;
                    }
                    case "application": {
                        const possiblyEmptyInterfaceExtension = walkupToFindInterfaceDefinition(name, node);
                        setCachedEvaluatedNodeType(node, structViewOfScope(sourceFile.containedScope.application!, possiblyEmptyInterfaceExtension));
                        break;
                    }
                }
            }

            // there is no "symbol" for a built-in scope like `variables` or etc.
            // we could probably skip this if we know that's the case
            // the type of `this` and `variables` are built from variable declarations,
            // but most builtin scopes support being "extended" via interface declarations;
            // so while they don't get symbols, they will have types
            const resolvedSymbol = walkupScopesToResolveSymbol(node, name); // really we want the flow type

            if (resolvedSymbol) {
                const evaluatedType = evaluateType(node, resolvedSymbol.symTabEntry.declaredType ?? resolvedSymbol.symTabEntry.type);
                setCachedEvaluatedNodeType(node, evaluatedType);
                setResolvedSymbol(node, resolvedSymbol);
            }
            // let flowType : _Type | undefined = undefined; determineFlowType(node, name);

            // // we know the symbol, but it has no flow type yet
            // // it's possible we don't know the symbol (inherited from parent cfc or cfinclude'd), in which case we don't want to report this
            // if (resolvedSymbol) {
            //     if (flowType && (flowType.flags & TypeFlags.containsUndefined)) {
            //         //typeErrorAtNode(node, `'${name}' is possibly undefined.`);
            //     }
            //     else if (flowType === undefined) {
            //         //typeErrorAtNode(node, `'${name}' is used before its first definition.`)
            //     }
            // }
            
            // // if we got a flow type, use that
            // if (flowType) {
            //     const evaluatedType = evaluateType(node, flowType);
            //     // is it necessary to cache the type on both the flow node (if it exists) *and* the node?
            //     if (node.flow) setCachedEvaluatedFlowType(node.flow, name, evaluatedType);
            //     setCachedEvaluatedNodeType(node, evaluatedType);
            //     return;
            // }

            // otherwise, fallback to the symbol type
            // if (resolvedSymbol) {
            //     if (useContainer === resolvedSymbol.container) {
            //         // there is a symbol table entry, but we could not find a type on the flow graph
            //         // if we're toplevel 
            //         /*if (noUndefinedVars) {
            //             typeErrorAtNode(node, `Identifier '${name}' is used before its declaration.`);
            //         }*/
            //         setCachedEvaluatedNodeType(node, SyntheticType.any);
            //         return;
            //     }
            //     else {
            //         // identifer is declared in some outer scope, we have to assume it is ok
            //         setCachedEvaluatedNodeType(node, resolvedSymbol.symTabEntry.type);
            //         return;
            //     }
            // }

            // if (noUndefinedVars) {
            //     //typeErrorAtNode(node, `Cannot find name '${name}'.`);
            // }
        }
    }

    function checkIndexedAccess(node: IndexedAccess) {
        checkNode(node.root);

        let type : _Type | undefined = getCachedEvaluatedNodeType(node.root);
        if (!type || type.flags & TypeFlags.any) {
            return;
        }

        // we set cached types on 'root' elements,
        // that is, the indexed-access root node itself, and the subsequent elements
        // not on the component identifiers, dots, brackets, etc.
        if (isStructOrArray(type)) {
            for (let i = 0; i < node.accessElements.length; i++) {
                const element = node.accessElements[i];
                if ((element.accessType === IndexedAccessType.dot || element.accessType === IndexedAccessType.bracket) && isStructLike(type)) {
                    const propertyName = element.accessType === IndexedAccessType.dot
                        ? element.property.token.text.toLowerCase()
                        : getTriviallyComputableString(element.expr);

                    const walkupInheritance = sourceFile.cfFileType === CfFileType.cfc
                        && i === 0
                        && node.root.kind === NodeKind.identifier
                        && (node.root.canonicalName === "this" || node.root.canonicalName === "super");

                    if (!propertyName) {
                        type = undefined;
                    }
                    else if (walkupInheritance) {
                        const symbol = (node.root as Identifier).canonicalName === "this"
                            ? walkupThisToResolveSymbol(propertyName)
                            : walkupSuperToResolveSymbol(propertyName);
                        if (symbol) {
                            type = evaluateType(node, symbol.symTabEntry.type);
                            setResolvedSymbol(element, symbol);
                        }
                        else {
                            type = undefined;
                        }
                    }
                    else {
                        const symbolResolution = getStructMember(element, type, propertyName);
                        if (symbolResolution) {
                            setResolvedSymbol(element, symbolResolution)
                            type = evaluateType(node, symbolResolution.symTabEntry.type);
                        }
                        else {
                            type = undefined;
                        }
                    }

                    if (!type || (type.flags & TypeFlags.any)) {
                        type = SyntheticType.any; // subsequent access elements will also be any
                        if (noUndefinedVars && propertyName) {
                            const errNode = element.accessType === IndexedAccessType.dot
                                ? element.property
                                : element.expr;
                            typeErrorAtNode(errNode, `Property '${propertyName}' does not exist on type`);
                        }
                    }
                }
                else if (element.accessType === IndexedAccessType.bracket && type && isArray(type)) {
                    type = getArrayMemberType(type);
                    if (!type) type = SyntheticType.any;
                }
                else {
                    // todo - support arrays being also dot-indexable, since they have member functions
                    type = SyntheticType.any;
                }

                setCachedEvaluatedNodeType(element, type);
            }

            setCachedEvaluatedNodeType(node, type); // in `a.b.c`, the whole indexedAccess expression type is typeof c
        }
        else {
            setCachedEvaluatedNodeType(node, SyntheticType.any);
            for (const element of node.accessElements) {
                setCachedEvaluatedNodeType(element, SyntheticType.any);
            }
            // typeErrorAtNode(node.root, `Type '${stringifyType(type)}' is not indexable.`)
        }

        checkList(node.accessElements);
    }

    function checkIndexedAccessChainElement(node: IndexedAccessChainElement) {
        if (node.parent!.flags & NodeFlags.checkerError) {
            //setCachedEvaluatedFlowType
            return;
        }

        const parentType = getCachedEvaluatedNodeType(node.parent);
        if (parentType.flags & TypeFlags.any) {
            return;
        }

        if (node.accessType === IndexedAccessType.dot) {
            if (isStructLike(parentType)) {
                const name = node.property.token.text;
                if (isStructLike(parentType)) {
                    if (!parentType.members.has(name) && noUndefinedVars) {
                        //typeErrorAtNode(node.property, `Property '${name}' does not exist on parent type.`);
                    }
                    node.flags |= NodeFlags.checkerError;
                }
            }
            else {
                // error like "type 'foo' has no member 'bar'"
            }
        }
    }

    

    function checkFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition) {
        // for cfc member functions, some work was already done in the binder to extract the signature, but we didn't have visibility into CFC resolution there;
        // so here we can try to resolve CFC return types / param types

        const isMemberFunction = isCfcMemberFunctionDefinition(node);
        let memberFunctionSignature : cfFunctionSignature | undefined;
        
        if (isMemberFunction) {
            if (node.kind === NodeKind.functionDefinition && node.canonicalName) {
                const symbol = walkupScopesToResolveSymbol(sourceFile, (node as any).canonicalName);
                if (symbol?.symTabEntry.type && isFunctionSignature(symbol.symTabEntry.type)) {
                    memberFunctionSignature = symbol.symTabEntry.type;
                    if (memberFunctionSignature.returns === SyntheticType.any) { // we got any as a returntype; user may have explicitly said "any", or we may have gotten something we didn't understand in the binder
                        const returnTypeLiteral = getFunctionDefinitionReturnsLiteral(node);
                        if (returnTypeLiteral && returnTypeLiteral.canonical !== "any") { // fixme: if (returnTypeLiteral && !isBuiltinType) so we don't try to lookup "string" as a cfc or etc.
                            const cfc = cfcResolver({resolveFrom: sourceFile.absPath, cfcName: returnTypeLiteral.ui});
                            if (cfc) {
                                memberFunctionSignature = unsafe__cfFunctionSignatureWithFreshReturnType(memberFunctionSignature, CfcTypeWrapper(cfc.sourceFile));
                                const variablesSymbol = sourceFile.containedScope.variables?.get(node.canonicalName);
                                // keep both `variables` and `this` in sync with member functions
                                if (variablesSymbol) {
                                    variablesSymbol.type = memberFunctionSignature;
                                    sourceFile.containedScope.variables?.set(node.canonicalName, variablesSymbol);
                                    sourceFile.containedScope.this?.set(node.canonicalName, variablesSymbol);
                                }
                            }
                        }
                    }
                }
            }
        }

        const cfSyntaxDirectedTypeSig = memberFunctionSignature ?? extractCfFunctionSignature(node);
        let finalType = cfSyntaxDirectedTypeSig;
        
        // if (node.kind === NodeKind.functionDefinition && node.canonicalName) {
        //     const engineSymbol = engineSymbolResolver(node.canonicalName);
        //     if (engineSymbol && isFunctionSignature(engineSymbol.type)) {
        //         // we pull engine function information from cfdocs;
        //         // it includes things like "onSessionStart", which are methods meant to be overridden by users inside application.cfm
        //         // probably we need to "auto-implements" application.cfc and have an interface definition file for the expected shape;
        //         // but! you don't *need* to implement some methods; so it's more of an auto-extends; can user-code have application.cfc extend anything?
        //         // typeErrorAtRange(node.range, `Named functions cannot shadow built-in functions; name '${node.uiName}' is reserved by a built-in function.`)
        //     }
        // }

        if (node.typeAnnotation) {
            const evaluatedType = evaluateType(node, node.typeAnnotation);
            if (isFunctionSignature(evaluatedType)) {
                if (!isAnnotatedSigCompatibleWithCfFunctionSig(evaluatedType, cfSyntaxDirectedTypeSig)) {
                    typeErrorAtNode(node, `Type '${stringifyType(cfSyntaxDirectedTypeSig)}' is not assignable to the annotated type '${stringifyType(node.typeAnnotation)}'.`)
                }
                else {
                    // copy cf-sig param names into annotated-type param names
                    for (let i = 0; i < cfSyntaxDirectedTypeSig.params.length; i++) {
                        evaluatedType.params[i].canonicalName = cfSyntaxDirectedTypeSig.params[i].canonicalName;
                        evaluatedType.params[i].uiName = cfSyntaxDirectedTypeSig.params[i].uiName;
                    }
                }

                finalType = evaluatedType;
            }
            else if (!(evaluatedType.flags & TypeFlags.any)) {
                typeErrorAtNode(node, `Expected a function signature as an annotated type, but got type '${stringifyType(evaluatedType)}'.`)
            }
        }

        // put access modifiers on the type signature for cfc member functions
        if (node.kind === NodeKind.functionDefinition && isMemberFunction) {
            const accessModifier = getFunctionDefinitionAccessLiteral(node);
            let accessModifierFlag : TypeFlags = 0;
            switch (accessModifier) {
                case "remote":    accessModifierFlag = TypeFlags.remote;    break;
                case "public":    accessModifierFlag = TypeFlags.public;    break;
                case "protected": accessModifierFlag = TypeFlags.protected; break;
                case "private":   accessModifierFlag = TypeFlags.private;   break;
                default: exhaustiveCaseGuard(accessModifier);
            }
            (finalType as Mutable<_Type>).flags |= accessModifierFlag;
        }

        setCachedEvaluatedNodeType(node, finalType);
        
        // for (const param of node.params) {
        //     setCachedEvaluatedFlowType(node.flow!, param.canonicalName, param.type || SyntheticType.any);
        // }

        if (node.kind === NodeKind.functionDefinition && node.fromTag) {
            checkListFunctionsLast(node.body);
        }
        else {
            checkNode(node.body);
        }
    }

    function checkSwitch(node: Switch) {
        if (node.fromTag) {
            checkNode(getAttributeValue((node.tagOrigin.startTag as CfTag.Common).attrs, "expression") ?? null)
        }
        else {
            checkNode(node.expr);
        }
        for (const case_ of node.cases) {
            checkNode(case_);
        }
    }

    function checkSwitchCase(node: SwitchCase) {
        if (node.fromTag) {
            if (node.tagOrigin.startTag?.canonicalName === "cfcase") {
                const attr = getAttributeValue((node.tagOrigin.startTag as CfTag.Common).attrs, "value") ?? null;
                // pre-cf2021 it has to be a string or numeric literal
                checkNode(attr);
            }
            checkList(node.body);
            return;
        }

        if (node.caseType === SwitchCaseType.case) {
            // pre cf-2021 it has to be a string or numeric literal
            checkNode(node.expr);
        }
        checkList(node.body);
    }

    function checkDo(node: Do) {
        checkNode(node.body);
        checkNode(node.expr);
    }

    function checkWhile(node: While) {
        checkNode(node.expr);
        checkNode(node.body);
    }

    function checkTernary(node: Ternary) {
        checkNode(node.expr);
        checkNode(node.ifTrue);
        checkNode(node.ifFalse);
    }

    //
    // type lookup
    //
    function walkUpContainersToFindType(context: Node, type: cfTypeId) : _Type | undefined {
        let node : Node | null = context;
        const typeName = type.name;

        while (node) {
            if (node.containedScope) {
                if (node.containedScope.typedefs) {
                    if (node.containedScope.typedefs.aliases.has(typeName)) {
                        return node.containedScope.typedefs.aliases.get(typeName)!;
                    }
                }
                if (node.kind === NodeKind.sourceFile) {
                    for (const lib of node.libRefs.values()) {
                        if (lib.containedScope.typedefs.mergedInterfaces.has(type.name)) {
                            return lib.containedScope.typedefs.mergedInterfaces.get(type.name)!;
                        }
                    }
                    break;
                }
                else {
                    node = node.containedScope.parentContainer;
                }
            }
            else {
                node = node.parent;
            }
        }

        return undefined;

        // if (node) { // should always be true (we hit the top, SourceFile, and broke out of the above loop)
        //     // find the `std` type in the libfiles, which we're just assuming will be either present or not, and if so, it is the only libfile
        //     /*for (const libFile of node.libRefs) {
        //         for (const typedef of libFile.content) {
        //             if (typedef.kind === NodeType.type && typedef.typeKind === TypeKind.struct && typedef.uiName === "std") {
        //                 return typedef.membersMap.get(type.uiName)?.type;
        //             }
        //         }
        //     }*/
        // }

        // if (!(type.flags & TypeFlags.synthetic)) {
        //     //typeErrorAtNode(type, `Cannot find name '${type.uiName}'.`);
        // }
        // return undefined;
    }

    function checkFor(node: For) {
        if (node.subType === ForSubType.forIn) {
            checkNode(node.expr);
            checkNode(node.init);
            checkNode(node.body);
            return;
        }

        checkNode(node.initExpr);
        checkNode(node.conditionExpr);
        checkNode(node.incrementExpr);
        checkNode(node.body);
    }

    function checkStructLiteral(node: StructLiteral) {
        // got `[:]`, there is nothing to check
        if (node.emptyOrderedStructColon) {
            return;
        }
        checkList(node.members);
        const memberTypes = new Map<string, SymTabEntry>();
        for (const member of node.members) {
            switch (member.subType) {
                case StructLiteralInitializerMemberSubtype.keyed: {
                    const memberType = getCachedEvaluatedNodeType(member);
                    if (memberType === SyntheticType.never) continue; // spreads or shorthand in unsupported engines may produce this
                    const key = getTriviallyComputableString(member.key);
                    if (!key) continue;
                    const canonicalName = key.toLowerCase();
                    memberTypes.set(canonicalName, {
                        uiName: key,
                        canonicalName,
                        declarations: [member],
                        type: memberType
                    });
                    break;
                }
                case StructLiteralInitializerMemberSubtype.spread: {
                    break;
                }
                default: exhaustiveCaseGuard(member);
            }
        }
        setCachedEvaluatedNodeType(node, SyntheticType.struct(memberTypes));
    }

    function checkStructLiteralInitializerMember(node: StructLiteralInitializerMember) {
        switch (node.subType) {
            case StructLiteralInitializerMemberSubtype.keyed: {
                if (node.shorthand) {
                    if (!supports.structLiteralShorthand(engineVersion)) {
                        typeErrorAtNode(node, `CF engine ${engineVersion.uiString} does not support struct literal shorthand notation.`);
                        setCachedEvaluatedNodeType(node, SyntheticType.never);
                        break;
                    }
                    if (node.key.kind !== NodeKind.identifier || !node.key.canonicalName) { // fixme: when is this undefined or empty?
                        typeErrorAtNode(node, "Shorthand struct literal initializers must be identifiers.");
                        setCachedEvaluatedNodeType(node, SyntheticType.never);
                        break;
                    }

                    checkNode(node.key);
                    const symbol = walkupScopesToResolveSymbol(node, node.key.canonicalName);
                    if (symbol) setCachedEvaluatedNodeType(node, symbol.symTabEntry.type);
                }
                else {
                    checkNode(node.key);
                    checkNode(node.expr);
                    setCachedEvaluatedNodeType(node, getCachedEvaluatedNodeType(node.expr));
                }
                break;
            }
            case StructLiteralInitializerMemberSubtype.spread: {
                if (!supports.structLiteralSpread(engineVersion)) {
                    typeErrorAtNode(node, `CF engine ${engineVersion} does not support struct literal spread syntax.`)
                    setCachedEvaluatedNodeType(node, SyntheticType.never);
                }
                checkNode(node.expr);
                break;
            }
            default: exhaustiveCaseGuard(node);
        }
    }

    function checkArrayLiteral(node: ArrayLiteral) {
        checkList(node.members);
        setCachedEvaluatedNodeType(node, cfArray(unionify(node.members.map(member => getCachedEvaluatedNodeType(member)))));
    }

    function checkArrayLiteralInitializerMember(node: ArrayLiteralInitializerMember) {
        checkNode(node.expr);
        setCachedEvaluatedNodeType(node, getCachedEvaluatedNodeType(node.expr));
    }

    function checkTry(node: Try) {
        checkList(node.body);
        checkList(node.catchBlocks);
        checkNode(node.finallyBlock);
    }

    function checkCatch(node: Catch) {
        if (node.fromTag) {
            // find exception binding via attributes?
            checkList(node.body);
            return;
        }
        // put node.exceptionBinding into flow, maybe do that in binder
        checkList(node.body)
    }

    function checkFinally(node: Finally) {
        checkList(node.body);
    }

    function checkNew(node: New) {
        if (node.callExpr.left.kind !== NodeKind.dottedPath) return;
        const cfcName = stringifyDottedPath(node.callExpr.left).ui;
        const cfc = cfcResolver({resolveFrom: sourceFile.absPath, cfcName: cfcName});
        if (!cfc) return;
        const cfcThis = CfcTypeWrapper(cfc.sourceFile);
        setCachedEvaluatedNodeType(node, cfcThis);
        
        const initSig = cfcThis.members.get("init");
        if (initSig && isFunctionSignature(initSig.type)) {
            setCachedEvaluatedNodeType(node.callExpr.left, initSig.type);
            checkCallLikeArguments(initSig.type, node.callExpr)
        }
    }

    function getArrayMemberType(type: _Type) : _Type | undefined {
        type = getCanonicalType(type);
        if (isArray(type)) {
            return type.memberType;
        }
        return undefined;
    }

    function getStructMember(element: Node, type: _Type, canonicalName: string) : SymbolResolution | undefined {
        if (!isStructLike(type)) {
            return undefined;
        }

        let member = type.members.get(canonicalName);

        if (!member && type.structKind === StructKind.symbolTableTypeWrapper && type.interfaceExtension) {
            member = type.interfaceExtension.members.get(canonicalName);
        }
        else if (type.structKind === StructKind.cfcTypeWrapper) {
            member = type.interfaceExtension?.members.get(canonicalName);
            if (!member) {
                let workingNode : SourceFile | null = type.cfc;
                while (workingNode) {
                    member = workingNode.containedScope.this!.get(canonicalName)
                        ?? workingNode.containedScope.typedefs.mergedInterfaces.get("this")?.members.get(canonicalName);
                    if (member) break;
                    workingNode = workingNode.cfc?.extends ?? null;
                }
            }
        }

        return member
            ? { container: element, scopeName: "__property", symTabEntry: member }
            : undefined;
    }

    // specialize `externWalkupScopesToResolveSymbol` to start at the top-level of our current source file, don't resolve engine symbols, and only consider `this` scopes
    function walkupThisToResolveSymbol(canonicalName: string) : SymbolResolution | undefined {
        return externWalkupScopesToResolveSymbol(sourceFile, canonicalName, /*engineSymbolResolver*/undefined, ["this"]);
    }

    // same as `walkupThisToResolveSymbol` but we start in the parent component
    function walkupSuperToResolveSymbol(canonicalName: string) : SymbolResolution | undefined {
        if (sourceFile.cfc?.extends) {
            return externWalkupScopesToResolveSymbol(sourceFile.cfc.extends, canonicalName, /*engineSymbolResolver*/undefined, ["this"]);
        }
        return undefined;
    }

    let cfcResolver! : CfcResolver;
    let walkupScopesToResolveSymbol! : (base: Node, canonicalName: string) => SymbolResolution | undefined;
    function install(installables: Partial<CheckerInstallable>) {
        for (const key of Object.keys(installables) as (keyof CheckerInstallable)[]) {
            switch (key) {
                case "CfcResolver":
                    cfcResolver = installables[key]!;
                    break;
                case "EngineSymbolResolver":
                    walkupScopesToResolveSymbol = (baseNode: Node, canonicalName: string) => externWalkupScopesToResolveSymbol(baseNode, canonicalName, installables[key]!);
                    break;
                default: exhaustiveCaseGuard(key);
            }
        }
    }

    //
    // type evaluation
    //
    const evaluateType = (function() {
        type NodeTrie = Map<_Type, NodeTrie> & Map<null, _Type | "PENDING"> ;
        const typeConstructorInvocationCacheTrie : NodeTrie = new Map();
        
        const enum TypeCache_Status { resolved, resolving, noCache };
        type TypeCache_Resolution = TypeCache_Cached | TypeCache_NoCache;
        interface TypeCache_Cached {
            status: TypeCache_Status.resolved,
            value: _Type
        }
        interface TypeCache_NoCache {
            status: TypeCache_Status.resolving | TypeCache_Status.noCache
        }
        
        function setCachedTypeConstructorInvocation(typeFunction: cfTypeConstructor, args: readonly _Type[], val: "PENDING" | _Type) : void {
            function getChildTrieMapOrNull(thisLevel: NodeTrie, type: _Type) : NodeTrie | null {
                const result = thisLevel.get(type);
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
                typeConstructorInvocationCacheTrie.set(typeFunction, bottom);
                return;
            }
        
            let workingMap = getChildTrieMapOrNull(typeConstructorInvocationCacheTrie, typeFunction);
        
            for (let i = 0; i < args.length; i++) { // is args ever 0 in a type call ?
                if (i === args.length - 1) {
                    if (workingMap === null) {
                        workingMap = new Map() as NodeTrie;
                        typeConstructorInvocationCacheTrie.set(typeFunction, workingMap);
                    }
                    const existingNextLevel = getChildTrieMapOrNull(workingMap, args[i]);
                    if (existingNextLevel) {
                        existingNextLevel.set(null, val);
                    }
                    else {
                        const bottom = new Map([[null, val]]) as NodeTrie;
                        workingMap.set(args[i], bottom)
                    }
                }
                else {
                    if (workingMap === null) {
                        workingMap = new Map() as NodeTrie;
                        typeConstructorInvocationCacheTrie.set(typeFunction, workingMap);
                    }
        
                    const existingNextLevel = getChildTrieMapOrNull(workingMap, args[i]);
                    if (existingNextLevel) {
                        workingMap = existingNextLevel;
                    }
                    else {
                        const generatedNextLevel = new Map();
                        workingMap.set(args[i], generatedNextLevel);
                        workingMap = generatedNextLevel;
                    }
                }
            }
        }

        function getCachedTypeConstructorInvocation(typeFunction: cfTypeConstructor, args: readonly _Type[]) : TypeCache_Resolution {
            let trieDescender = typeConstructorInvocationCacheTrie.get(typeFunction);
            for (let i = 0; i < args.length; i++) {
                if (!trieDescender) {
                    return {status: TypeCache_Status.noCache};
                }
                trieDescender = trieDescender.get(args[i]);
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

        function evaluateType(context: Node, type: _Type | null, typeParamMap: Map<string, _Type> = new Map(), depth = 0) : _Type {
            return typeWorker(type);

            // here args[n] should already have been evaluated
            function invokeTypeConstructor(typeFunction: cfTypeConstructor, args: readonly _Type[]) : _Type {
                if (args.includes(SyntheticType.never)) {
                    return SyntheticType.never;
                }

                try {
                    depth++;

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
            
                    if (isTypeConstructor(result)) {
                        // if a type constructor returned a type constructor, extend the new type constructor's environment
                        // with the parent's environment + the args to the parent's invocation
                        // inner most names shadows outer names if there are name conflicts
                        // result.capturedParams = typeParamMap;
                    }
                    return result;
                }
                finally {
                    depth--;
                }
            }
            
            function evaluateIntersection(types: _Type[]) : _Type {
                if (types.includes(SyntheticType.never)) {
                    return SyntheticType.never;
                }

                try {
                    depth++;

                    /*
                    if (left.typeKind === TypeKind.struct && right.typeKind === TypeKind.struct) {
                        let longest = left.membersMap.size > right.membersMap.size ? left.membersMap : right.membersMap;
                        let shortest = longest === left.membersMap ? right.membersMap : left.membersMap;
                
                        const remainingLongestKeys = new Set([...longest.keys()]);
                        const result = new Map<string, SymTabEntry>();
                        for (const key of shortest.keys()) {
                            remainingLongestKeys.delete(key);
                            const evaluatedShortest = typeWorker(shortest.get(key)!.type);
                            const evaluatedLongest = longest.has(key) ? typeWorker(longest.get(key)!.type) : null;
                            if (!evaluatedLongest) {
                                result.set(key, {uiName: shortest.get(key)!.uiName, canonicalName: key, declarations: null, userType: null, inferredType: null, type: evaluatedShortest});
                                continue;
                            }
                            const intersect = evaluateIntersection(evaluatedShortest, evaluatedLongest);
                            if (intersect.typeKind === TypeKind.never) {
                                return cfNever();
                            }
                            else {
                                result.set(key, {uiName: longest.get(key)!.uiName, canonicalName: key, declarations: null, userType: null, inferredType: null, type: evaluatedLongest});
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
                        return SyntheticType.never;
                    }*/
                    return cfIntersection(...types);
                }
                finally {
                    depth--;
                }
            }

            function typeWorker(type: Readonly<_Type> | null) : _Type {
                try {
                    depth++;
                
                    if (!type) return SyntheticType.any;
                    
                    if (isIntersection(type)) {
                        return evaluateIntersection(type.types);
                    }
                    if (isUnion(type)) {
                        return cfUnion(type.types.map(type => typeWorker(type))); // need to dedupe and etc.
                    }
                    if (isStructLike(type)) {
                        return type;
                        /*const evaluatedStructContents = new Map<string, SymTabEntry>();
                        let concrete = true;
                        for (const key of type.members.keys()) {
                            //const preEvaluatedId = type.members.get(key)!.type.nodeId;
                            const evaluatedType = typeWorker(type.members.get(key)!.type);
                            //const postEvaluatedId = evaluatedType.nodeId;
                            if (type !== evaluatedType) {
                                concrete = false;
                            }
                            evaluatedStructContents.set(key, {
                                uiName: type.members.get(key)!.uiName,
                                declarations: null,
                                canonicalName: key,
                                type: evaluatedType
                            });
                        }
                        if (concrete) {
                            return type;
                        }
                        return SyntheticType.struct(evaluatedStructContents);*/
                    }
                    if (isFunctionSignature(type)) {
                        // need a more lean version of functionParameter
                        /*
                        const params : cfFunctionSignatureParam[] = [...type.params];
                        for (let i = 0; i < type.params.length; i++) {
                            if (!params[i].type) {
                                throw "no type for parameter " + i; /// can we get here? parser / binder should convert this to any before we get here...
                            }
                            params[i].type = typeWorker(params[i].type!);
                        }
                        const returns = typeWorker(type.returns);
                        return cfFunctionSignature(type.uiName, params, returns);*/
                        return type;
                    }
                    if (isTypeConstructorInvocation(type)) {
                        if (type.left === SyntheticType.cfcLookupType) {
                            const literalType = type.args[0];
                            if (literalType && isLiteralType(literalType) && literalType.flags & TypeFlags.string) {
                                const resolvedCfc = cfcResolver({resolveFrom: sourceFile.absPath, cfcName: literalType.literalValue as string})
                                if (resolvedCfc) {
                                    const freshType = CfcTypeWrapper(resolvedCfc.sourceFile);
                                    (freshType as Mutable<_Type>).underlyingType = type;
                                    return freshType;
                                }
                            }
                        }

                        const typeConstructor = typeWorker(type.left);
                        if (typeConstructor.flags & TypeFlags.never) {
                            return typeConstructor;
                        }

                        unsafeAssertTypeKind<cfTypeConstructor>(typeConstructor);

                        if (type.args.length === 0) {
                            //typeErrorAtNode(type.left, `_Type argument list cannot be empty.`);
                            return SyntheticType.never;
                        }
                        if (typeConstructor.params.length != type.args.length) {
                            //typeErrorAtNode(type.left, `_Type requires ${typeConstructor.params.length} arguments.`);
                            return SyntheticType.never;
                        }

                        const args : _Type[] = [];
                        for (const arg of type.args) {
                            if (isTypeId(arg)) {
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
                    if (isCachedTypeConstructorInvocation(type)) {
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
                    if (isTypeId(type)) {
                        // @fixme: should error if we can't find it; and return never ?
                        let result = typeParamMap.get(type.name) || walkUpContainersToFindType(context, type) || null;
                        if (!result) return SyntheticType.any;
                        if (type.indexChain) {
                            for (const name of type.indexChain) {
                                if (!isStructLike(result)) return SyntheticType.any; // should error with "not indexable" or etc.
                                const nextType = result.members.get(name)?.type;
                                if (nextType) result = typeWorker(nextType);
                            }
                        }
                        
                        return typeWorker(result);
                    }

                    // a type not requiring evaluation: number, string, boolean,
                    return type;
                }
                finally {
                    depth--;
                }
            }
        }

        return evaluateType;
    })();

    return {
        check,
        getCachedEvaluatedNodeType: getCachedEvaluatedNodeTypeImpl,
        getSymbol: getSymbolImpl,
        setNoUndefinedVars,
        install,
        setLang: (lv: EngineVersion) => engineVersion = lv,
    }
}

export interface CheckerInstallable {
    CfcResolver: CfcResolver,
    EngineSymbolResolver: EngineSymbolResolver
}

export type Checker = ReturnType<typeof Checker>;

function QuickInstance(sourceFile: SourceFile) : void {
    if (sourceFile.cfFileType !== CfFileType.cfc) return;

    const methodsToAdd : SymTabEntry[] = [];

    // method names matching /^scope(.*)/ get replaced with methods named /$1/
    // also has its first param removed
    for (const symTabEntry of sourceFile.containedScope.this!.values()) {
        if (isFunctionSignature(symTabEntry.type) && !isFunctionOverloadSet(symTabEntry.type)) {
            if (/^scope/i.test(symTabEntry.uiName)) {
                (symTabEntry.type as Mutable<_Type>).flags &= ~(TypeFlags.private | TypeFlags.public | TypeFlags.protected | TypeFlags.remote);
                
                let freshUiName = symTabEntry.uiName.replace(/^scope/i, "");
                freshUiName = freshUiName[0].toLowerCase() + freshUiName.slice(1);
                
                // fixme: can't set access flags with function sig constructor?
                
                const freshType = cfFunctionSignature(freshUiName, symTabEntry.type.params.slice(1), CfcTypeWrapper(sourceFile), symTabEntry.type.attrs);
                (symTabEntry.type as Mutable<_Type>).flags |= TypeFlags.private;
                (freshType as Mutable<_Type>).flags |= TypeFlags.public;

                methodsToAdd.push({
                    uiName: freshUiName,
                    canonicalName: freshUiName.toLowerCase(),
                    type: freshType,
                    declarations: symTabEntry.declarations
                });
            }
        }
    }

    // properties get "where getters",
    // i.e. property named "column" gets a public "getter" named "whereColumn"
    for (const symTabEntry of sourceFile.containedScope.variables!.values()) {
        const propertyDecl = symTabEntry.declarations?.find((node) : node is Property => node.kind === NodeKind.property);
        if (propertyDecl) {
            let propertyName = getTriviallyComputableString(getAttributeValue(propertyDecl.attrs, "name"));
            if (propertyName) {
                propertyName = propertyName[0].toUpperCase() + propertyName.slice(1);
                const param = cfFunctionSignatureParam(true, SyntheticType.string, "");
                methodsToAdd.push({
                    uiName: "where" + propertyName,
                    canonicalName: "where" + propertyName.toLowerCase(),
                    type: cfFunctionSignature("where" + propertyName, [param], CfcTypeWrapper(sourceFile), []),
                    declarations: [propertyDecl]
                })
            }
        }
    }

    for (const method of methodsToAdd) {
        sourceFile.containedScope.this!.set(method.canonicalName, method);
    }
}

function WeakPairMap<K0 extends Object, K1 extends Object, V>() {
    const map = new WeakMap<K0, WeakMap<K1, V>>();

    function get(k0: K0, k1: K1) {
        return map.get(k0)?.get(k1);
    }

    function has(k0: K0, k1: K1) {
        return map.has(k0) ? map.get(k0)!.has(k1) : false;
    }

    function set(k0: K0, k1: K1, v: V) {
        const subMap = map.get(k0);
        if (subMap) {
            subMap.set(k1, v);
        }
        else {
            map.set(k0, new WeakMap([[k1, v]]));
        }
    }

    function _delete(k0: K0, k1?: K1) {
        if (k1) {
            map.get(k0)?.delete(k1);
        }
        else {
            map.delete(k0);
        }
    }

    return {
        get,
        set,
        has,
        delete: _delete
    }
}
