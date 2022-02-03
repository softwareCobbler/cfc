import { Diagnostic, SourceFile, Node, NodeKind, BlockType, IndexedAccess, StatementType, CallExpression, IndexedAccessType, CallArgument, BinaryOperator, BinaryOpType, FunctionDefinition, ArrowFunctionDefinition, IndexedAccessChainElement, NodeFlags, VariableDeclaration, Identifier, Flow, isStaticallyKnownScopeName, For, ForSubType, UnaryOperator, Do, While, Ternary, StructLiteral, StructLiteralInitializerMemberSubtype, StructLiteralInitializerMember, ArrayLiteral, ArrayLiteralInitializerMember, Catch, Try, Finally, New, Switch, CfTag, SwitchCase, SwitchCaseType, Conditional, ConditionalSubtype, SymTabEntry, mergeRanges, ReturnStatement, SymbolResolution, SymbolTable, UnreachableFlow, DiagnosticKind, TagAttribute, SymbolId, TypeShimKind, NonCompositeFunctionTypeAnnotation, DUMMY_CONTAINER } from "./node";
import { CfcResolution, CfcResolver, ComponentResolutionArgs, EngineSymbolResolver, LibTypeResolver, ProjectOptions } from "./project";
import { Scanner, CfFileType, SourceRange } from "./scanner";
import { cfFunctionSignature, Struct, cfUnion, BuiltinType, TypeFlags, UninstantiatedArray, extractCfFunctionSignature, Type, stringifyType, cfFunctionSignatureParam, cfFunctionOverloadSet, cfTypeId, SymbolTableTypeWrapper, Cfc, Interface, createType, createLiteralType, typeFromJavaLikeTypename, structurallyCompareTypes, TypeKind, isStructLike, cfArray, cfStructLike, isStructLikeOrArray, cfGenericFunctionSignature } from "./types";
import { CanonicalizedName, exhaustiveCaseGuard, findAncestor, functionDefinitionHasUserSpecifiedReturnType, getAttributeValue, getCallExpressionNameErrorRange, getComponentAttrs, getContainingFunction, getFunctionDefinitionAccessLiteral, getFunctionDefinitionNameTerminalErrorNode, getSourceFile, getTriviallyComputableString, isCfcMemberFunctionDefinition, isInCfcPsuedoConstructor, isInEffectiveConstructorMethod, isLiteralExpr, isNamedFunction, isSimpleOrInterpolatedStringLiteral, Mutable, stringifyDottedPath, stringifyLValue, stringifyStringAsLValue, tryGetCfcMemberFunctionDefinition } from "./utils";
import { walkupScopesToResolveSymbol as externWalkupScopesToResolveSymbol, TupleKeyedWeakMap } from "./utils";
import { Engine, supports } from "./engines";

const structViewCache = TupleKeyedWeakMap<[SymbolTable, Interface], SymbolTableTypeWrapper>(); // map a SymbolTable -> Interface -> Struct, used to check prior existence of a wrapping of a symbol table into a struct with a possible interface extension
const EmptyInstantiationContext = SourceFile("", CfFileType.cfc, ""); // an empty type type instantiation context, no symbols are visible; there we rely entirely on captured or provided contexts attached the instantiable target

// the "debug" object should be moved into the Project object, and passed into the checker
// but this gets it going
function Debug() {
    function out(...args: any[]) {
        console.log(...args);
    }

    return {
        out
    }
}

export function Checker(options: ProjectOptions) {
    const engineVersion = options.engineVersion;

    // dev feature flags, generally enable-able via editor config options
    const GENERIC_FUNCTION_INFERENCE = options.genericFunctionInference;
    const CHECK_RETURN_TYPES = options.checkReturnTypes;
    const CHECK_FLOW_TYPES = options.checkFlowTypes;
    const debug = Debug();

    let sourceFile: SourceFile;
    let scanner: Scanner;
    let diagnostics: Diagnostic[];
    let returnTypes: Type[] = [];
    let flowBecameUnreachable = false;
    let warnOnUndefined: boolean;
    let checkerStack : Node[];
    let forcedReturnTypes : WeakMap<Node, Type>;

    function check(sourceFile_: SourceFile) {
        // we're using the Checker as a singleton, and checking one source file might trigger checking another
        // so, we have to save state before descending into the next check
        const savedSourceFile = sourceFile;
        const savedScanner = scanner;
        const savedDiagnostics = diagnostics;
        const savedReturnTypes = returnTypes;
        const savedFlowBecameUnreachable = flowBecameUnreachable;
        const savedWarnOnUndefined = warnOnUndefined;
        const savedCheckerStack = checkerStack;
        const savedForcedReturnTypes = forcedReturnTypes;

        sourceFile = sourceFile_;
        scanner = sourceFile_.scanner;
        diagnostics = sourceFile_.diagnostics;
        returnTypes = savedReturnTypes;
        flowBecameUnreachable = savedFlowBecameUnreachable;
        warnOnUndefined = savedWarnOnUndefined;
        checkerStack = savedCheckerStack;
        forcedReturnTypes = savedForcedReturnTypes;

        sourceFile = sourceFile_;
        scanner = sourceFile.scanner;
        diagnostics = sourceFile.diagnostics;
        flowBecameUnreachable = false;
        warnOnUndefined = false;
        checkerStack = [];
        forcedReturnTypes = new WeakMap();

        if (sourceFile.cfFileType === CfFileType.cfc) {
            warnOnUndefined = getAttributeValue(getComponentAttrs(sourceFile) ?? [], "warn-undefined") !== null;

            if (sourceFile.cfc?.extends && sourceFile.cfc?.extends.containedScope.this) {
                sourceFile.containedScope.super = sourceFile.cfc.extends.containedScope.this;
            }
        }

        instantiateInterfaces(sourceFile);
        checkList(sourceFile.content);

        // 1/22/21 -- not supporting decorators
        // for (const decorator of sourceFile.containedScope.typedefs.decorators) {
        //     if (decorator.name === "QuickInstance") {
        //         QuickInstance(sourceFile);
        //     }
        // }

        sourceFile = savedSourceFile;
        scanner = savedScanner;
        diagnostics = savedDiagnostics;
        returnTypes = savedReturnTypes;
        flowBecameUnreachable = savedFlowBecameUnreachable;
        warnOnUndefined = savedWarnOnUndefined;
        checkerStack = savedCheckerStack;
        forcedReturnTypes = savedForcedReturnTypes;
    }

    function checkerStackContains(node: Node) {
        return checkerStack.indexOf(node) !== -1;
    }

    // fixme: application and variables shouldn't ever be generic
    // and we should defer in general to needs at a use-site?
    // really this just merges ... instantiation is later ...
    function instantiateInterfaces(node: Node) {
        if (node.containedScope?.typeinfo.mergedInterfaces) {
            for (const [ifaceName, iface] of node.containedScope.typeinfo.mergedInterfaces) {
                if (iface.typeParams) {
                    // we can't instantiate a generic interface without type args
                    continue;
                }
                // fixme: preserve order; all spreads happend first right now
                const freshMembers = new Map<string, SymTabEntry>();
                // @instantiableSpreads
                // if (iface.instantiableSpreads) {
                //     for (const spread of iface.instantiableSpreads) {
                //         const type = evaluateType(node, spread);
                //         if (isStructLike(type)) {
                //             for (const [k,v] of type.members) {
                //                 freshMembers.set(k,v);                                
                //             }
                //         }
                //     }
                // }
                for (const [memberName, member] of iface.members) {
                    // if (member.firstLexicalType?.kind === TypeKind.cfcLookup) {
                    //     setEffectivelyDeclaredType(member)
                    //     (member as Mutable<SymTabEntry>).firstLexicalType = evaluateType(node, member.firstLexicalType);
                    // }
                    freshMembers.set(memberName, member);
                }
                const freshType = Interface(ifaceName, freshMembers);
                (freshType as Mutable<Interface>).underlyingType = iface;
                node.containedScope.typeinfo.mergedInterfaces.set(ifaceName, freshType);
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

    function issueDiagnosticAtRange(range: SourceRange, msg: string, kind = DiagnosticKind.error) : void {
        const freshDiagnostic : Diagnostic = {
            kind,
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

    function issueDiagnosticAtNode(node: Node, msg: string, kind = DiagnosticKind.error) {
        issueDiagnosticAtRange(node.range, msg, kind);
    }

    function checkList(nodes: Node[]) {
        for (const node of nodes) {
            checkNode(node);
        }
    }

    function checkNode(node: Node | null) {
        if (!node) return;

        if (flowBecameUnreachable) {
            node.flags |= NodeFlags.unreachable;
            if (sourceFile.endOfNodeFlowMap.has(node.nodeId)) {
                sourceFile.endOfNodeFlowMap.get(node.nodeId)!.becameUnreachable = true;
            }
        }

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
                    case StatementType.fromTag: {
                        checkTag(node.tagOrigin.startTag as CfTag.Common);
                        break;
                    }
                    case StatementType.expressionWrapper: {
                        checkNode(node.expr);
                        break;
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
            case NodeKind.simpleStringLiteral:
                setCachedEvaluatedNodeType(node, createLiteralType(node.textSpan.text));
                return;
            case NodeKind.interpolatedStringLiteral:
                checkList(node.elements);
                setCachedEvaluatedNodeType(node, BuiltinType.string);
                return;
            case NodeKind.numericLiteral: {
                const val = parseFloat(node.literal.token.text);
                if (!isNaN(val)) setCachedEvaluatedNodeType(node, createLiteralType(val));
                else setCachedEvaluatedNodeType(node, BuiltinType.numeric);
                return;
            }
            case NodeKind.booleanLiteral:
                setCachedEvaluatedNodeType(node, node.booleanValue ? BuiltinType.true : BuiltinType.false);
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

                    const mappings = sourceFile.libRefs.get("<<magic/wirebox>>")!.containedScope.typeinfo.mergedInterfaces.get("Wirebox")?.members.get("mappings");
                    const targetSymbol = sourceFile.containedScope.variables!.get(nameStringVal);
                    if (!mappings || !targetSymbol || mappings.firstLexicalType!.kind !== TypeKind.struct) return;

                    const cfcLookup = mappings.firstLexicalType.members.get(injectStringVal)?.firstLexicalType;
                    if (!cfcLookup) return;
                    const cfc = evaluateType(node, cfcLookup);

                    setEffectivelyDeclaredType(targetSymbol, cfc);
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

    function determineFlowType(base: Flow, symbolId: SymbolId) : Type | undefined {
        const seen = new Set<Flow>();
        return work(base) ?? undefined;

        function work(flow: Flow) : Type | undefined | null {
            const type = getCachedEvaluatedFlowType(flow, symbolId);

            if (type) {
                return type;
            }

            seen.add(flow);

            const flowTypes : Type[] = [];
            let seenCount = 0;

            for (let i = 0; i < flow.predecessors.length; i++) {
                if (seen.has(flow.predecessors[i])) {
                    seenCount++;
                    continue;
                }
                const type = work(flow.predecessors[i]);
                if (type) {
                    flowTypes.push(type);
                }
                else if (type === undefined) {
                    flowTypes.push(BuiltinType.undefined);
                }
                else if (type === null) {
                    seenCount++;
                }

            }

            if (seenCount === flow.predecessors.length) {
                return null;
            }
            else if (flowTypes.length === 0) {
                return undefined;
            }
            else {
                return unionify(flowTypes);
            }
        }
    }

    function mergeTypes(l: Type, r: Type) : Type {
        if (l === r) return l;

        if (l.kind === TypeKind.literal && r.kind === TypeKind.literal) {
            if (l.underlyingType === BuiltinType.numeric && r.underlyingType === BuiltinType.numeric) return BuiltinType.numeric;
            if (l.underlyingType === BuiltinType.string || r.underlyingType === BuiltinType.string) return BuiltinType.string;
        }

        if (isStructLike(l) && isStructLike(r)) {
            // merge arrays ... ?
            // merge cfc types ? ... probably not desireable?
            if (l.kind === TypeKind.struct && r.kind === TypeKind.struct) {
                const keys = [...l.members.keys(), ...r.members.keys()];
                const mergedMembers = new Map<string, SymTabEntry>();
                for (const key of keys) {
                    if (l.members.has(key) && r.members.has(key)) {
                        const mergedType = mergeTypes(l.members.get(key)!.firstLexicalType!, r.members.get(key)!.firstLexicalType!);
                        mergedMembers.set(key, {
                            canonicalName: key,
                            uiName: l.members.get(key)!.uiName,
                            declarations: [],
                            firstLexicalType: mergedType,
                            symbolId: -1,
                        })
                    }
                    else {
                        // would be good to mark it as optional
                        const oneDefinitelyExists = (l.members.get(key) || r.members.get(key))!
                        mergedMembers.set(key, oneDefinitelyExists)
                    }
                }
                return Struct(mergedMembers);
            }

        }

        return l; // ?!
    }

    // needs to merge 2+ unions, dedup, etc.
    // union type needs to be a Set<_Type>
    // "instantiated type": {flags: TypeFlag, type: _Type} so we can share "any" and "void" and etc.
    // the "undefined" typeflag needs, at a minimum, to be renamed "containsNull", and should be a BulitinType.null
    // unions could maybe have flags that indicate whether they contain primitives
    function unionify(types: Type[]) : Type {
        let flags : TypeFlags = TypeFlags.none;
        const membersBuilder = new Set<Type>();

        outer:
        for (const type of types) {
            if (type === BuiltinType.any || type === BuiltinType.never) {
                return type;
            }
            if (type.flags & TypeFlags.containsUndefined) {
                flags |= TypeFlags.containsUndefined;
            }

            // merge types that are subtypes of each others
            // the check is bivariant
            // l: {x: 1, y:1, z:1}, r: {x: 1, y:1} -> merge ok
            // l: {x: 1, y:1}, r: {x: 1, y:1, z: 1} -> merge ok
            // l: {x:1}, r: {z:1} -> no merge
            const deletables : Type[] = [];
            let addedViaMerge = false;
            for (const existingUnionMember of membersBuilder) {
                if (existingUnionMember === type) {
                    continue outer;
                }
                if (isLeftSubtypeOfRight(existingUnionMember, type, false, false, true) || isLeftSubtypeOfRight(type, existingUnionMember, false, false, true)) {
                    deletables.push(existingUnionMember);
                    const merged = mergeTypes(existingUnionMember, type);
                    membersBuilder.add(merged);
                    addedViaMerge = true;
                    break;
                }
            }
            if (!addedViaMerge) {
                membersBuilder.add(type);
            }
            for (const deleteable of deletables) {
                membersBuilder.delete(deleteable);
            }
        }

        if (membersBuilder.size === 0) return BuiltinType.any; // maybe never?

        return cfUnion(membersBuilder, flags);
    }


    /**
     * `to = assignThis` is a valid assignment if `assignThis` is a subtype of `to`
     * @param assignThis 
     * @param to 
     */
    function isAssignable(assignThis: Type, to: Type, sourceIsLiteralExpr = false, forReturnType = false) : boolean {
        return isLeftSubtypeOfRight(assignThis, to, sourceIsLiteralExpr, forReturnType);
    }

    // a zero-arity cf signature can be matched with the signature `(...arguments: any[]) => any`
    // like:
    //
    // // @type (...arguments: any[]) => any
    // <cfscript>
    //     function foo() { /* any number of args in arguments */ }
    // </cfscript>
    // 
    //
    function functionSignatureIsLegacyCfIndefiniteArityMarker(sig: cfFunctionSignature) : boolean {
        const exactlyOneParam = sig.params.length === 1;
        if (!exactlyOneParam) {
            return false;
        }

        const param = sig.params[0];
        const spread = !!(param.flags & TypeFlags.spread);
        const nameIsArguments = param.canonicalName === "arguments";
        const isArray = param.paramType.kind === TypeKind.array;
        const arrayMemberIsAny = isArray && param.paramType.memberType === BuiltinType.any;

        return spread
            && nameIsArguments
            && isArray
            && arrayMemberIsAny;
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

        // if cf signature has zero args, but the annotated sig explicitly indicates zero-or-more
        // note this is different than (atLeastOne: any, ...rest: any[])
        if (cfSig.params.length === 0 && functionSignatureIsLegacyCfIndefiniteArityMarker(annotatedSig)) {
            return true;
        }
        
        if (cfSig.params.length != annotatedSig.params.length) return false;

        for (let i = 0; i < cfSig.params.length; i++) {
            // covariant param check, not contravariant; we're trying to tighten the type
            if (!isAssignable(annotatedSig.params[i].paramType, cfSig.params[i].paramType)) return false;
        }
        return true;
    }

    function isAnnotatedOverloadCompatibleWithCfFunctionSig(lParams: readonly cfFunctionSignatureParam[], rParams: readonly cfFunctionSignatureParam[], lReturns: Type, rReturns: Type) {
        if (!isLeftSubtypeOfRight(lReturns, rReturns)) {
            return false;
        }

        // T <: U
        // L = @!type (a) => T
        // R = function foo(a, b) U
        const requiredLeft = lParams.filter((p) => !(p.flags & TypeFlags.optional)).length;
        const requiredRight = rParams.filter((p) => !(p.flags & TypeFlags.optional)).length;
        
        if (requiredLeft > requiredRight) {
            return false;
        }

        // hm, should probably enforce same order of names
        for (let i = 0; i <= Math.min(lParams.length, rParams.length); i++) {
            if (!isAssignable(lParams[i], rParams[i])) {
                return false;
            }
        }

        return true;
    }

    //
    // `l <: r` means "left is a subtype of right"
    // `l !<: r` means "left is NOT a subtype of right"
    // it is maybe helpful to note that "sub" means "substitutable" in addition to "a descendant in a heirarchy"
    // i.e. `l <: r` means l is substitutable for r (you can safely use an l in r's place)
    //
    function isLeftSubtypeOfRight(l: Type, r: Type, sourceIsLiteralExpr = false, forReturnType = false, widenLiterals = false) : boolean {
        let depth = 0;
        let tooDeep = false;
        tooDeep ? 0 : 1; // yes compiler this is used
        const runningComparisonMap = new Map<Type, Set<Type>>();

        return worker(l, r);

        function worker(l: Type, r: Type) : boolean {
            if (runningComparisonMap.get(l)?.has(r)) {
                // we're already comparing these types
                return true;
            }
            if (depth > 32) {
                //debugger;
                tooDeep = true;
                return false;
            }
            try {
                const existingKeyedComparison = runningComparisonMap.get(l);
                if (existingKeyedComparison) {
                    existingKeyedComparison.add(r);
                }
                else {
                    runningComparisonMap.set(l, new Set([r]));
                }

                depth++;

                // a type is a subtype of itself
                if (l === r) return true;

                // any is a subtype of every type; every type is a subtype of any
                if (l === BuiltinType.any || r === BuiltinType.any) return true;

                // `void function foo() { return; }` is valid, in effect `return <unit>` returns null
                // so for a return type, null is assignable to void
                if (forReturnType && l === BuiltinType.null && r === BuiltinType.void) return true;

                // except for the above return type case, void is not a subtype of anything except itself and any
                if (l === BuiltinType.void || r === BuiltinType.void) {
                    runningComparisonMap.get(l)!.delete(r);
                    return false;
                }

                // it would be nice to error on this, but plenty of legacy code relies on
                // number being assignable to boolean
                if (l === BuiltinType.numeric && r === BuiltinType.boolean) return true;

                if (widenLiterals) {
                    const widenedLeft = l.kind === TypeKind.literal ? l.underlyingType : undefined;
                    const widenedRight = r.kind === TypeKind.literal ? r.underlyingType : undefined;
                    if (widenedLeft && widenedRight && widenedLeft === widenedRight) {
                        return true;
                    }

                    // numeric is a subtype of string
                    // however, string is not a subtype of numeric
                    if (widenedLeft === BuiltinType.numeric && widenedRight === BuiltinType.string) {
                        return true;
                    }
                }
                else {
                    if (l.kind == TypeKind.literal && r.kind === TypeKind.literal) {
                        if (l.literalValue === r.literalValue) {
                            return true;
                        }
                        else {
                            runningComparisonMap.get(l)!.delete(r);
                            return false;
                        }
                    }
                    if (l.kind !== TypeKind.literal && r.kind === TypeKind.literal) {
                        runningComparisonMap.get(l)!.delete(r);
                        return false; // number is not a subtype of `0`
                    }
                    if (l.kind === TypeKind.literal && r.kind !== TypeKind.literal && worker(l.underlyingType, r)) {
                        return true; // `0` is a subtype of number
                    }

                    // numeric is a subtype of string
                    // however, string is not a subtype of numeric
                    if (l === BuiltinType.numeric && r === BuiltinType.string) {
                        return true;
                    }
                }


                function propertyCounts(structLike: cfStructLike) {
                    // doesn't make too much sense for an object literal
                    let optional = 0;
                    let required = 0;
                    for (const symTabEntry of structLike.members.values()) {
                        if (symTabEntry.links?.optional) {
                            optional += 1;
                        }
                        else {
                            required += 1;
                        }
                    }
                    return {
                        optional,
                        required,
                        total: optional + required
                    };
                }

                //
                // {x: number, y: number} <: {x: number}, because L has AT LEAST all the properties of R
                // {x: number} !<: {x: number, y: number}
                //
                function isLeftStructSubtypeOfRightStruct(l: cfStructLike, r: cfStructLike) {
                    // the outer types may be different wrappers for the same underlying struct
                    // e.g. a SymbolTableWrapper around `this` from within a CFC, and a CFC wrapper wrapping the same `this` but for a reference from another file
                    if (l.members === r.members) return true;

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
                    if (l.kind === TypeKind.cfc && r.kind === TypeKind.cfc) {
                        return !!findAncestor(l.cfc, (node) => node === r.cfc, /*followCfcInheritance*/ true);
                    }

                    if (l.kind === TypeKind.struct && r === BuiltinType.EmptyInterface) {
                        return true;
                    }

                    //if (sourceIsLiteralExpr && l.members.size !== r.members.size) return false; // barring optional properties...
                    const countsR = propertyCounts(r);
                    const countsL = propertyCounts(l);
                    if (countsL.required < countsR.required) {
                        runningComparisonMap.get(l)!.delete(r);
                        return false;
                    }
                    if (sourceIsLiteralExpr && countsL.total > countsR.total) {
                        runningComparisonMap.get(l)!.delete(r);
                        return false;
                    }

                    for (const [propName, rightVal] of r.members) {
                        const leftVal = l.members.get(propName);
                        // check for optional property of R -- if left doesn't exist, OK; if left does exist, check for subtype-ness
                        if (rightVal.links?.optional) {
                            if (!leftVal) continue;
                            if (!worker(leftVal.firstLexicalType!, rightVal.firstLexicalType!)) {
                                runningComparisonMap.get(l)!.delete(r);
                                return false;
                            }
                        }
                        // check for required properties of R -- left must exist, not be optional, and be a subtype
                        else {
                            if (!leftVal || (leftVal.links?.optional) || !worker(leftVal.firstLexicalType!, rightVal.firstLexicalType!)) {
                                runningComparisonMap.get(l)!.delete(r);
                                return false;
                            }
                        }
                    }

                    // we've checked everything in target against source;
                    // if source is a literal expression, it may not have additional properties
                    if (sourceIsLiteralExpr) {
                        for (const propName of l.members.keys()) {
                            if (!r.members.has(propName)) {
                                runningComparisonMap.get(l)!.delete(r);
                                return false;
                            }
                        }
                    }

                    return true;
                }

                if (l.kind === TypeKind.array && r.kind === TypeKind.array) {
                    if (worker(l.memberType, r.memberType)) {
                        return true;
                    }
                    else {
                        runningComparisonMap.get(l)!.delete(r);
                    }
                }
                if (isStructLike(l) && isStructLike(r)) {
                    if (isLeftStructSubtypeOfRightStruct(l, r)) {
                        return true;
                    }
                    else {
                        runningComparisonMap.get(l)!.delete(r);
                        return false;
                    }
                }
                
                if (l.kind === TypeKind.functionSignature && r.kind === TypeKind.functionSignature) {
                    // covariant in return type
                    if (!worker(l.returns, r.returns)) {
                        runningComparisonMap.get(l)!.delete(r);
                        return false;
                    }

                    // contravariant in parameter types
                    // also every parameter needs the same names...?

                    const hasSpread = !!(r.params.length > 0 && r.params[r.params.length-1].flags & TypeFlags.spread);
                    const minParams = r.params.filter(param => !(param.flags & TypeFlags.optional)).length;
                    if (!hasSpread && l.params.length < minParams) {
                        runningComparisonMap.get(l)!.delete(r);
                        return false;
                    }

                    for (let i = 0; i < l.params.length; i++) {
                        // if the arg in the right side is a spread, just check remaining left types against the (un-array-wrapped) spread type
                        if (r.params[i].flags & TypeFlags.spread) {
                            const rType = r.params[i].paramType;
                            const spreadType = rType.kind === TypeKind.array ? rType.memberType : null;
                            if (!spreadType) {
                                runningComparisonMap.get(l)!.delete(r);
                                return false; // all spread types must be arrays, we should have caught this before getting here
                            }
                            for (let j = i; j < l.params.length; j++) {
                                const lpt = l.params[i].paramType;
                                // contravariant, flip left/right
                                if (!worker(spreadType, lpt)) {
                                    runningComparisonMap.get(l)!.delete(r);
                                    return false;
                                }
                            }

                            break;
                        }

                        const lpt = l.params[i].paramType;
                        const rpt = r.params[i].paramType;
                        // contravariant, flip left/right
                        if (!worker(rpt, lpt)) {
                            runningComparisonMap.get(l)!.delete(r);
                            return false;
                        }
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
                if (l.kind === TypeKind.union) {
                    for (const leftConstituent of l.types) {
                        if (!worker(leftConstituent, r)) {
                            runningComparisonMap.get(l)!.delete(r);
                            return false;
                        }
                    }
                    return true;
                }
                if (r.kind === TypeKind.union) {
                    for (const rightConstituent of r.types) {
                        if (worker(l, rightConstituent)) return true;
                    }

                    runningComparisonMap.get(l)!.delete(r);
                    return false;
                }
                if (l.kind === TypeKind.intersection) {
                    for (const leftConstituent of l.types) {
                        if (worker(leftConstituent, r)) return true;
                    }

                    runningComparisonMap.get(l)!.delete(r);
                    return false;
                }
                if (r.kind === TypeKind.intersection) {
                    for (const rightConstituent of r.types) {
                        if (!worker(l, rightConstituent)) {
                            runningComparisonMap.get(l)!.delete(r);
                            return false;
                        }
                    }
                    return true;
                }
                
                // can we instantiate these any further?
                let didReinstantiate = false;
                if (l.kind === TypeKind.typeConstructorInvocation) {
                    didReinstantiate = true;
                    l = evaluateType(EmptyInstantiationContext, l);
                }
                if (r.kind === TypeKind.typeConstructorInvocation) {
                    didReinstantiate = true;
                    r = evaluateType(EmptyInstantiationContext, r);
                }

                if (didReinstantiate) {
                    if (worker(l,r)) {
                        return true;
                    }
                    else {
                        runningComparisonMap.get(l)!.delete(r);
                        return false;
                    }
                }

                runningComparisonMap.get(l)!.delete(r);
                return false;
            }
            finally {
                depth--;
            }
        }
    }

    function chooseOverloadWithPositionalArgs(overloadSet: cfFunctionOverloadSet, args: CallArgument[]) : cfFunctionOverloadSet["overloads"] {
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
                const paramType = overload.params[i].paramType;
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

    function chooseOverloadWithNamedArgs(overloadSet: cfFunctionOverloadSet, args: CallArgument[]) : cfFunctionOverloadSet["overloads"] {
        const argMap = new Map<string, Type>();
        for (const arg of args) {
            const name = getTriviallyComputableString(arg.name);
            if (!name) return [];
            argMap.set(name, getCachedEvaluatedNodeType(arg));
        }

        const availableOverloads = new Set(overloadSet.overloads);
        const deleteableOverloads = new Set<cfFunctionOverloadSet["overloads"][number]>();

        outer:
        for (const overload of availableOverloads) {
            for (const param of overload.params) {
                const argType = argMap.get(param.canonicalName);
                if ((!argType && !(param.flags & TypeFlags.optional)) || (argType && !isLeftSubtypeOfRight(argType, param.paramType))) {
                    deleteableOverloads.add(overload);
                    continue outer;
                }
            }
        }

        for (const deleteableOverload of deleteableOverloads) {
            availableOverloads.delete(deleteableOverload);
        }

        return [...availableOverloads];
    }

    function chooseOverload(overloadSet: cfFunctionOverloadSet, args: CallArgument[]) : cfFunctionOverloadSet["overloads"] {
        let named = 0;
        let positional = 0;
        for (const arg of args) {
            if (arg.name) named++;
            else positional++;
        }

        if (named === args.length) {
            return chooseOverloadWithNamedArgs(overloadSet, args);
        }
        else if (positional === args.length) {
            return chooseOverloadWithPositionalArgs(overloadSet, args);
        }
        else {
            return [];
        }
    }

    function countNamedArgs(args: CallArgument[]) : number {
        let count = 0;
        for (const arg of args) {
            if (arg.name) {
                count++;
            }
        }
        return count;
    }

    function checkCallExpression(node: CallExpression) {
        checkNode(node.left);

        // the resolved symbol may shadow a built-in always-visible engine symbol
        // function call identifiers always call the built-in if it names a built-in
        // e.g function `foo(string encodeForHtml) { return encodeForHTML(encodeForHTML); }` is valid,
        // invoking the builtin function 'encodeForHTML' on the string argument 'encodeForHTML'
        // if the above is true, checking has cached the non-engine type, and we need to override that decision
        const symbol = getResolvedSymbol(node.left);

        const shadowsBuiltinSymbol = !!symbol?.alwaysVisibleEngineSymbol;
        const isBuiltinSymbol = symbol?.scopeName === "__cfEngine";
        if (shadowsBuiltinSymbol || isBuiltinSymbol) {
            let sig = getEffectivelyDeclaredType(symbol.symTabEntry); // builtin symbols we just use the first lexical type
            if (!sig) {
                sig = setEffectivelyDeclaredType(symbol.symTabEntry, evaluateType(DUMMY_CONTAINER, symbol.symTabEntry.firstLexicalType!));
            }
            if (sig.kind === TypeKind.functionSignature) {
                setCachedEvaluatedNodeType(node.left, sig);
                setCachedEvaluatedNodeType(node, sig.returns);

                if (sig.returns === BuiltinType.never) {
                    flowBecameUnreachable = true;
                }

                const namedArgCount = countNamedArgs(node.args);
                if (namedArgCount > 0 && namedArgCount !== node.args.length) {
                    issueDiagnosticAtRange(node.left.range, "All arguments must be named, if any are named.");
                }

                // check the expressions themselves, but for now we won't check that they are correct for the signature
                checkList(node.args);
            }
            return;
        }

        const sig = getCachedEvaluatedNodeType(node.left);
        
        if (sig.kind === TypeKind.functionSignature || sig.kind === TypeKind.functionOverloadSet || sig.kind === TypeKind.genericFunctionSignature) {
            const namedArgCount = countNamedArgs(node.args);
            if (namedArgCount > 0 && namedArgCount !== node.args.length) {
                issueDiagnosticAtRange(node.left.range, "All arguments must be named, if any are named.");
            }
        }

        let returnType : Type;

        if (sig.kind === TypeKind.functionOverloadSet) {
            checkList(node.args);
            const overloads = chooseOverload(sig, node.args);
            if (overloads.length !== 1) {
                setCachedEvaluatedNodeType(node, returnType = BuiltinType.any);
                issueDiagnosticAtRange(getCallExpressionNameErrorRange(node), "No overload matches this call.");
            }
            else {
                setCachedEvaluatedNodeType(node, returnType = evaluateType(node, overloads[0].returns));
            }
        }
        else if (sig.kind === TypeKind.functionSignature) {
            checkList(node.args);
            checkCallLikeArguments(sig, node.args, getCallExpressionNameErrorRange(node), /*isNewExpr*/false);
            setCachedEvaluatedNodeType(node, returnType = sig.returns);
        }
        else if (sig.kind === TypeKind.genericFunctionSignature) {
            if (!GENERIC_FUNCTION_INFERENCE) {
                return;
            }

            const typeParamMap = new Map<string, Type | undefined>();
            const definitelyResolvedTypeParamMap = new Map<string, Type>();
            const pushResolution = (name: string, type: Type) => {
                typeParamMap.set(name, type);
                definitelyResolvedTypeParamMap.set(name, type);
            }

            for (const p of sig.typeParams) {
                typeParamMap.set(p.name, undefined);
            }

            for (let i = 0; i < sig.params.length && i < node.args.length; i++) {
                const sigParamType = sig.params[i].paramType;
                const callSiteArg = node.args[i];


                if (sigParamType.kind === TypeKind.functionSignature) {
                    const isGenericInReturnPosition = sigParamType.returns.kind === TypeKind.typeId && typeParamMap.has(sigParamType.returns.name);

                    if (isGenericInReturnPosition && (callSiteArg.expr.kind === NodeKind.functionDefinition || callSiteArg.expr.kind === NodeKind.arrowFunctionDefinition)) {
                        for (let j = 0; j < callSiteArg.expr.params.length; j++) {
                            const resolutions = resolveGenericFunctionTypeParams(
                                typeParamMap,
                                sigParamType.params[j].paramType,
                                evaluateType(callSiteArg, typeFromJavaLikeTypename(callSiteArg.expr.params[j].javaLikeTypename)));
                            if (resolutions) {
                                for (const [k,v] of resolutions) {
                                    if (v) {
                                        pushResolution(k,v);
                                    }
                                }
                            }
                        }

                        for (let j = 0; j < callSiteArg.expr.params.length; j++) {
                            const argScopeSymTabEntry = callSiteArg.expr.containedScope!.arguments!.get(callSiteArg.expr.params[j].canonicalName)!;
                            argScopeSymTabEntry.firstLexicalType = evaluateType(callSiteArg, sigParamType.params[j].paramType, definitelyResolvedTypeParamMap);
                        }

                        if (callSiteArg.expr.fromTag) {
                            // not possible, can't have a tag expr in an inline function expression, like `needsACallback(<cffunction>....)`
                        }
                        else {
                            const returnType = checkFunctionBody(
                                callSiteArg.expr,
                                // fixme: we need to send the actual determined types;
                                // have they been checked by virtue of `resolveGenericFunctionTypeParams`?
                                sig.params.map((param) => createType({...param, paramType: BuiltinType.any})));
                            pushResolution((sigParamType.returns as cfTypeId).name, returnType);
                        }
                    }
                    else {
                        checkNode(callSiteArg.expr);
                        getCachedEvaluatedNodeType(callSiteArg.expr);
                    }
                }
                else {
                    // if param is optional and callSiteArg is undefined we don't need to do this
                    checkNode(callSiteArg);
                    const argType = getCachedEvaluatedNodeType(node.args[i])
                    const resolutions = resolveGenericFunctionTypeParams(typeParamMap, sigParamType, argType);
                    if (resolutions) {
                        for (const [k,v] of resolutions) {
                            if (v) typeParamMap.set(k,v)
                        }
                    }
                }
            }

            if (definitelyResolvedTypeParamMap.size === typeParamMap.size) {
                returnType = evaluateType(EmptyInstantiationContext, sig.returns, definitelyResolvedTypeParamMap);
                setCachedEvaluatedNodeType(node, returnType);
            }
            else {
                returnType = BuiltinType.any;
            }

            function widenToCommonType(t: Type, u: Type) : Type | undefined {
                if (t.kind === TypeKind.literal && u.kind === TypeKind.literal) {
                    if (t.underlyingType === BuiltinType.string || u.underlyingType === BuiltinType.string) return BuiltinType.string;
                    if (t.underlyingType === BuiltinType.numeric && u.underlyingType === BuiltinType.numeric) return BuiltinType.numeric;
                }
                return undefined;
            }

            function resolveGenericFunctionTypeParams(unifiees: ReadonlyMap<string, Type | undefined>, target: Type, source: Type) : ReadonlyMap<string, Type | undefined> | undefined {
                if (target.kind === TypeKind.typeId) {
                    if (unifiees.has(target.name)) {
                        if (!unifiees.get(target.name)) {
                            return new Map([[target.name, source]]);
                        }

                        const resolvedTarget = unifiees.get(target.name)!;
                        if (!isAssignable(source, resolvedTarget, /*sourceIsLiteralExpr*/ false)) {
                            const widerType = widenToCommonType(source, resolvedTarget);
                            if (widerType) {
                                return new Map([[target.name, widerType]]);
                            }
                            else {
                                throw "unassignable from source to resolved generic --- that means T was resolved but now we have a different T?"
                            }
                        }

                        return undefined;
                    }
                    else {
                        throw "these types are closed -- we're not looking up arbitrary generic type IDs, they should have been resolved already during the containing type's instantiation"
                    }
                }
                else if (target.kind === TypeKind.array && source.kind === TypeKind.array) {
                    return resolveGenericFunctionTypeParams(unifiees, target.memberType, source.memberType);
                }
                else if (isStructLike(target) && isStructLike(source)) {
                    if (target.kind === TypeKind.cfc && source.kind === TypeKind.cfc) {
                        // uh, cfcs can't be generic?
                        return undefined;
                    }
                    if (source.members.size < target.members.size) {
                        return undefined;
                    }
                    const freshResolutions = new Map<string, Type | undefined>([...unifiees]);
                    for (const [name, symTabEntry] of target.members) {
                        const targetType = symTabEntry.firstLexicalType!;
                        if (!source.members.has(name)) return;
                        const result = resolveGenericFunctionTypeParams(freshResolutions, targetType, source.members.get(name)!.firstLexicalType!);
                        if (result) {
                            for (const [k,v] of result) freshResolutions.set(k,v);
                        }
                    }

                    return freshResolutions;
                }
                else if (target.kind === TypeKind.functionSignature && source.kind === TypeKind.functionSignature) {
                    if (target.params.length !== source.params.length) return undefined;

                    const freshResolutions = new Map<string, Type | undefined>([...unifiees]);

                    for (let i = 0; i < target.params.length; i++) {
                        const targetParamType = target.params[i].paramType;
                        const sourceParamType = source.params[i].paramType;
                        const result = resolveGenericFunctionTypeParams(unifiees, targetParamType, sourceParamType);
                        if (result) {
                            for (const[k,v] of result) freshResolutions.set(k,v);
                        }
                    }

                    const returnTypeResolution = resolveGenericFunctionTypeParams(unifiees, target.returns, source.returns);
                    if (returnTypeResolution) {
                        for (const[k,v] of returnTypeResolution) freshResolutions.set(k,v);
                    }

                    return freshResolutions;
                }
                else {
                    return undefined;
                }
            }

            if (returnType === BuiltinType.never) {
                flowBecameUnreachable = true;
            }
        }
        else if (sig === BuiltinType.any) {
            checkList(node.args); // check the arg expressions, but we don't check that they make sense or have the right arity or etc.
            return;
        }
        else {
            function getCallExprCallableName(node: CallExpression) : Node {
                if (node.left.kind === NodeKind.indexedAccess) {
                    const accessElement = node.left.accessElements[node.left.accessElements.length - 1];
                    if (accessElement.accessType === IndexedAccessType.dot || accessElement.accessType === IndexedAccessType.optionalDot) {
                        return accessElement.property;
                    }
                    else if (accessElement.accessType === IndexedAccessType.bracket || accessElement.accessType === IndexedAccessType.optionalBracket) {
                        return accessElement.expr;
                    }
                }
                return node.left;
            }
            issueDiagnosticAtNode(getCallExprCallableName(node), `Type '${stringifyType(sig)}' is not callable.`);
        }
    }

    function checkCallLikeArguments(sig: Type, args: CallArgument[], nameRange: SourceRange, isNewExpr: boolean, requireNamedArgs = false) : void {
        if (sig === BuiltinType.any) {
            return;
        }
        if (sig.kind === TypeKind.functionSignature) {
            const namedArgCount = countNamedArgs(args);

            const minRequiredParams = sig.params.filter(param => !(param.flags & TypeFlags.optional) && !(param.flags & TypeFlags.spread)).length;
            // maxParams is undefined if there was a spread param, since it accepts any number of trailing args
            const maxParams = sig.params.length > 0 && sig.params[sig.params.length - 1].flags & TypeFlags.spread
                ? undefined
                : sig.params.length;
            
            let hasArgumentCollectionArg = false;

            // If the user supplied at least one named arg, we require all args to be named
            // It is also possible that the callsite requires named args (or rather, does not support unnamed args by position only),
            // which is the case for a tag call
            if (namedArgCount > 0 || requireNamedArgs) {
                const paramNameMap = new Map<string, {uiName: string, param: cfFunctionSignatureParam}>();
                for (const param of sig.params) {
                    paramNameMap.set(param.canonicalName, {uiName: param.uiName, param});
                }
                const seenArgs = new Set<string>();
                for (const arg of args) {
                    const argName = arg.name?.kind === NodeKind.identifier
                        ? stringifyLValue(arg.name)
                        : isSimpleOrInterpolatedStringLiteral(arg.name)
                        ? stringifyStringAsLValue(arg.name)
                        : undefined;
                    if (arg.name && argName) {
                        hasArgumentCollectionArg = hasArgumentCollectionArg || (argName.canonical === "argumentcollection");
                        if (seenArgs.has(argName?.canonical)) {
                            const uiName = argName.ui || argName.canonical;
                            issueDiagnosticAtNode(arg.name, `Duplicate argument '${uiName}'`);
                        }

                        const paramPair = paramNameMap.get(argName.canonical);
                        if (!paramPair) {
                            if (argName.canonical !== "argumentcollection") {
                                const uiName = argName.ui || argName.canonical;
                                issueDiagnosticAtNode(arg.name, `'${uiName}' is not a recognized parameter for this ${isNewExpr ? "constructor" : "function"}.`);
                            }
                        }
                        else {
                            const argType = getCachedEvaluatedNodeType(arg);
                            const paramType = paramPair.param.paramType;
                            if (!isAssignable(argType, paramType)) {
                                issueDiagnosticAtNode(arg, `Argument of type '${stringifyType(argType)}' is not assignable to parameter of type '${stringifyType(paramType)}'.`);
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
                        issueDiagnosticAtRange(nameRange, `Required named parameters are missing: ${missingNamedParams}`);
                    }
                }
            }
            else {
                const spreadAdjust = maxParams === undefined ? -1 : 0; // if there was a trailing spread arg, drop the spread count by 1
                const minToCheck = Math.min(args.length, sig.params.length + spreadAdjust);
                for (let i = 0; i < minToCheck; i++) {
                    const argType = getCachedEvaluatedNodeType(args[i]);
                    const paramType = sig.params[i].paramType;
                    if (!isAssignable(argType, paramType, isLiteralExpr(args[i].expr))) {
                        issueDiagnosticAtNode(args[i], `Argument of type '${stringifyType(argType)}' is not assignable to parameter of type '${stringifyType(paramType)}'.`);
                    }
                }
            }

            if (!hasArgumentCollectionArg && (args.length < minRequiredParams || (maxParams !== undefined && args.length > maxParams))) {
                let msg;
                if (minRequiredParams !== maxParams) {
                    if (maxParams === undefined) msg = `Expected at least ${minRequiredParams} arguments, but got ${args.length}`;
                    else msg = `Expected between ${minRequiredParams} and ${maxParams} arguments, but got ${args.length}`;
                }
                else msg = `Expected ${maxParams} arguments, but got ${args.length}`;
                issueDiagnosticAtRange(nameRange, msg);
            }

            // most of this is done above, with the exception of "push type into inline function definition"
            // which we also do in the generic instantiator, and its necessary there to get the return type in something like `<T>(t:T) => t`
            // so we should put this one spot
            // for (let i = 0; i < node.args.length; i++) {
            //     if (sig.params[i]?.flags & TypeFlags.spread) continue;
            //     const paramType = sig.params[i]?.type ?? null;
            //     if (!paramType) break;
            //     const arg = node.args[i];
            //     const argType = getCachedEvaluatedNodeType(arg);
            //     if (!isAssignable(/*assignThis*/ argType, /*to*/ paramType)) {
            //         // error
            //     }
            //     if (paramType.kind === TypeKind.functionSignature && (arg.expr.kind === NodeKind.functionDefinition || arg.expr.kind === NodeKind.arrowFunctionDefinition)) {
            //         //pushTypesIntoInlineFunctionDefinition(node, paramType, arg.expr);
            //         //checkNode(arg.expr.body as Node /*fixme: we know this is a script function definition but can't prove it here; anyway, all function defs should have Node as a body, not Node|Node[] ?*/);
            //     }
            // }
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
        // const type = getCachedEvaluatedNodeType(node.expr);
        // if (!(type.flags & TypeFlags.any) && !(type.flags & TypeFlags.numeric)) {
        //     // true for ++/--
        //     //typeErrorAtNode(node.expr, "Unary operator requires a numeric operand.");
        //     // need "coercible to bool" for "!"
        // }
    }

    function checkBinaryOperator(node: BinaryOperator) {
        checkNode(node.left);
        checkNode(node.right);

        switch (node.optype) {
            case BinaryOpType.assign: {
                // an assignment, even fv-unqualified, will always be bound to a scope
                // `x = y` is effectively `variables.x = y`
                if (node.left.kind === NodeKind.identifier || node.left.kind === NodeKind.indexedAccess) {
                    const lhsSymbol = getResolvedSymbol(node.left);
                    const rhsType = getCachedEvaluatedNodeType(node.right);

                    if (CHECK_FLOW_TYPES && lhsSymbol) {
                        const effectivelyDeclaredType = getEffectivelyDeclaredType(lhsSymbol.symTabEntry);
                        if (effectivelyDeclaredType) {
                            if (isAssignable(rhsType, effectivelyDeclaredType)) {
                                setCachedEvaluatedFlowType(node.left.flow!, lhsSymbol.symTabEntry.symbolId, rhsType);
                            }
                            else {
                                setCachedEvaluatedFlowType(node.left.flow!, lhsSymbol.symTabEntry.symbolId, effectivelyDeclaredType);
                                const r = stringifyType(rhsType);
                                const l = stringifyType(effectivelyDeclaredType);
                                issueDiagnosticAtNode(node.right, `Type '${r}' is not assignable to type '${l}'`, DiagnosticKind.error);
                            }
                        }
                        else if (!effectivelyDeclaredType
                            && (isInCfcPsuedoConstructor(node) || isInEffectiveConstructorMethod(node))
                            && node.left.kind === NodeKind.indexedAccess
                            && node.left.root.kind === NodeKind.identifier
                            && node.left.root.canonicalName === "variables"
                        ) {
                            // fixme: check against a type annotation?
                            setCachedEvaluatedFlowType(node.left.flow!, lhsSymbol.symTabEntry.symbolId, rhsType);
                            setEffectivelyDeclaredType(lhsSymbol.symTabEntry, rhsType);
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
            case BinaryOpType.assign_add:
            case BinaryOpType.assign_sub:
            case BinaryOpType.assign_mul:
            case BinaryOpType.assign_div:
            case BinaryOpType.assign_mod:
            case BinaryOpType.exp: {
                break;
            }
            default: {
                // exhaustiveCaseGuard(node);
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

    function setCachedEvaluatedFlowType(flow: Flow, symbolId: SymbolId, type: Type) : void {
        if (!flow || !flow.flowId) {
            // this is definitely triggered like this:
            // ```
            // return;
            // for (var x in y) {} // name = 'x'
            // ```
            const hasFlow = !!flow;
            const hasFlowId = !!(flow?.flowId);
            const name = sourceFile.symbolIdToSymbol.get(symbolId)!.uiName;
            debug.out(`Expected a flow when setting flowtype for '${name}'; hasFlow=${hasFlow}, hasFlowID='${hasFlowId}'`)
            return;
        }

        if (CHECK_FLOW_TYPES) {
            if (!sourceFile.cachedFlowTypes.has(flow.flowId)) {
                sourceFile.cachedFlowTypes.set(flow.flowId, new Map());
            }
            sourceFile.cachedFlowTypes.get(flow.flowId)!.set(symbolId, type);
        }
    }

    function getCachedEvaluatedFlowType(flow: Flow, symbolId: SymbolId) : Type | undefined {
        if (CHECK_FLOW_TYPES) {
            return sourceFile.cachedFlowTypes.get(flow.flowId)?.get(symbolId);
        }
        else {
            return BuiltinType.any;
        }
    }

    function checkReturnStatement(node: ReturnStatement) {
        checkNode(node.expr);
        const func = getContainingFunction(node);
        if (!func) {
            const errNode = node.fromTag ? node.tagOrigin.startTag! : node.returnToken!;
            issueDiagnosticAtNode(errNode, "A return statement must be contained inside a function body.");
            return;
        }

        const exprType = node.expr ? getCachedEvaluatedNodeType(node.expr) : BuiltinType.null;

        const sig = getCachedEvaluatedNodeType(func);
        if (!sig || sig.kind !== TypeKind.functionSignature) {
            // why is there possibly no signature or no function signature?
            // we might not have marked the containing function as having a return type, yet...
            // so, the inverse question -- when do we have a function signature here?
            // we might be inferring the return type, rather than checking against an explicitly inidcated return type
            returnTypes.push(exprType);
            return;
        }

        if (!isAssignable(exprType, sig.returns, /*sourceIsLiteralExpr*/ node.expr ? isLiteralExpr(node.expr) : false, /*forReturnType*/ true)) {
            // if we got an exprType, we got an expr or a just return token; if this is from tag, we definitely got a tag
            const errNode = node.fromTag ? node.tagOrigin.startTag : (node.expr || node.returnToken);
            if (errNode) {
                issueDiagnosticAtNode(errNode, `Type '${stringifyType(exprType)}' is not assignable to declared return type '${stringifyType(sig.returns)}'`);
            }
        }
        else {
            if (node.flow !== UnreachableFlow) {
                returnTypes.push(exprType);
            }
        }
    }

    // `var x` without an initializer is valid in `for (var x in y)`;
    // otherwise it should always be `var x = ...`
    // both are variable declarations
    // we would like to extract `x` out of the above in both cases
    // note that `for (var x.y in z)` is valid code, and there was recently a complaint on the ACF bug tracker that `for (var x['y'] in z)` should also be valid
    // these would effectively declare `var x = {y: ?}` 
    function getVariableDeclarationLValue(node: VariableDeclaration | BinaryOperator) : Identifier | IndexedAccess | undefined {
        let workingNode : Node = node.kind === NodeKind.variableDeclaration ? node.expr : node;
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
            issueDiagnosticAtRange(mergeRanges(node.finalModifier, node.expr), `final-qualified declaration in a for initializer will fail at runtime.`);
        }

        if (engineVersion.engine === Engine.Adobe && canonicalPath.length === 1) {
            const enclosingFunction = getContainingFunction(node);
            if (enclosingFunction) {
                for (const param of enclosingFunction.params) {
                    if (param.canonicalName === name.canonical) {
                        issueDiagnosticAtNode(node, `Identifier '${name.ui}' is already declared in arguments scope.`)
                    }
                }
            }
        }

        let rhsType : Type | undefined = undefined;
        let rhsExpr : Node | undefined = undefined;
        let assignabilityErrorNode : Node;
        let didConsumeTypeAnnotation = false;

        if (node.expr.kind === NodeKind.binaryOperator) {
            if ((node.typeAnnotation?.type?.kind === TypeKind.functionSignature || node.typeAnnotation?.shimKind === TypeShimKind.nonCompositeFunctionTypeAnnotation)
                && (node.expr.right.kind === NodeKind.functionDefinition || node.expr.right.kind === NodeKind.arrowFunctionDefinition)
            ) {
                checkFunctionDefinition(node.expr.right, node.typeAnnotation);
                didConsumeTypeAnnotation = true;
            }
            else {
                checkNode(node.expr.right);
            }

            rhsExpr = node.expr.right;
            rhsType = getCachedEvaluatedNodeType(node.expr.right);
            assignabilityErrorNode = node.expr.right;
        }
        else if (isForInit && node.parent?.kind === NodeKind.for && node.parent.subType === ForSubType.forIn && node === node.parent.init) {
            rhsType = getCachedEvaluatedNodeType(node.parent.expr); // `for (x in y)`, x gets its type from `y`
            rhsExpr = node.parent.expr;
            assignabilityErrorNode = node;

            if (rhsType.kind === TypeKind.array) {
                rhsType = rhsType.memberType;
            }
            else {
                rhsType = BuiltinType.any;
            }
        }
        else if (node.expr.kind === NodeKind.identifier) {
            // this is a variable declaration, like `var foo;`
            // check that it hasn't already been declared?...
            return;
        }
        else {
            return; // unreachable?
        }

        const symbol = walkupScopesToResolveSymbol(node, name.canonical)?.symTabEntry;

        if (symbol) {
            let effectivelyDeclaredType = getEffectivelyDeclaredType(symbol);

            // first declaration
            if (!effectivelyDeclaredType) {
                if (node.typeAnnotation?.shimKind === TypeShimKind.annotation && !didConsumeTypeAnnotation) {
                    effectivelyDeclaredType = evaluateType(node, node.typeAnnotation.type);
                }
                else {
                    effectivelyDeclaredType = rhsType;
                }

                setEffectivelyDeclaredType(symbol, effectivelyDeclaredType);
            }

            let flowType : Type;

            if (!isAssignable(rhsType, effectivelyDeclaredType, isLiteralExpr(rhsExpr))) {
                if (CHECK_FLOW_TYPES) {
                    const l = stringifyType(effectivelyDeclaredType);
                    const r = stringifyType(rhsType);
                    issueDiagnosticAtNode(assignabilityErrorNode, `Mismatched types on local redeclaration; type '${r}' is not assignable to initially declared or inferred type '${l}'`);
                }

                flowType = effectivelyDeclaredType;
            }
            else {
                flowType = rhsType;
            }

            setCachedEvaluatedFlowType(lValue.flow!, symbol.symbolId, flowType);
        }

    }

    function setEffectivelyDeclaredType(symbol: SymTabEntry, type: Type) : Type {
        return (symbol.links ?? (symbol.links = {})).effectiveDeclaredType = type;
    }

    function getEffectivelyDeclaredType(symbol: SymTabEntry) : Type | undefined {
        return symbol.links?.effectiveDeclaredType ?? symbol.firstLexicalType;
    }

    function checkTag(tag: CfTag.Common) {
        const tagCallSignature = tryGetTagCallSignature(tag.canonicalName);
        if (!tagCallSignature || tagCallSignature.kind !== TypeKind.functionSignature) {
            return;
        }

        function callArgsFromTagAttrs(tag: CfTag.Common) : CallArgument[] {
            const args : CallArgument[] = [];
            for (const attr of tag.attrs)   {
                const name = Identifier(attr.name, attr.name.token.text);
                const expr = getAttributeValue(tag.attrs, attr.name.token.text);
                if (!expr) {
                    issueDiagnosticAtNode(attr.name, "Tag attribute has no associated value.", DiagnosticKind.error);
                }
                else {
                    args.push(CallArgument(name, /*equals*/null, /*dotDotDot*/null, expr, /*comma*/null));
                }
            }
            
            return args;
        }

        function assignTypesToAttrsFromSignature(sig: cfFunctionSignature, attrs: TagAttribute[]) {
            const nameToType = (() => {
                const result = new Map<string, Type>();
                for (const param of sig.params) {
                    result.set(param.canonicalName, param.paramType);
                }
                return result;
            })();

            for (const attr of attrs) {
                const type = nameToType.get(attr.canonicalName);
                if (type) {
                    setCachedEvaluatedNodeType(attr, type);
                }
            }
        }

        assignTypesToAttrsFromSignature(tagCallSignature, tag.attrs);
        setCachedEvaluatedNodeType(tag, tagCallSignature);

        const args = callArgsFromTagAttrs(tag);
        const nameRange = (() => {
            const r = mergeRanges(tag.tagStart, tag.tagName);
            return r.fromInclusive === r.toExclusive
                ? r // it's a 0-length error span, not much else to do with it
                : new SourceRange(r.fromInclusive+1, r.toExclusive) // otherwise, it's the "<cftagname" but starting on the char after the "<"
        })();
        checkCallLikeArguments(tagCallSignature, args, nameRange, /*isNewExpr*/ false, /*requireNamedArgs*/ true);
    }

    // fixme: needs to take a SourceFile arg or that defaults to our current or ...
    function getCachedEvaluatedNodeTypeImpl(node: Node | null, workingSourceFile: SourceFile) : Type {
        if (!node) {
            return BuiltinType.any;
        }

        let targetId = node.nodeId;

        /*if (node.kind === NodeType.indexedAccess) {
            targetId = node.accessElements[node.accessElements.length-1].nodeId;
        }*/

        if (workingSourceFile.cachedNodeTypes.has(targetId)) {
            return workingSourceFile.cachedNodeTypes.get(targetId)!;
        }
        else {
            return BuiltinType.any;
        }
    }

    function getCachedEvaluatedNodeType(node: Node | null) {
        return getCachedEvaluatedNodeTypeImpl(node, sourceFile);
    }

    function setCachedEvaluatedNodeType(node: Node, type: Type) {
        sourceFile.cachedNodeTypes.set(node.nodeId, type);
        return type;
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

    // take a SymbolTable and wrap it in a Struct, so we can bridge the gap between those worlds, mostly for the 
    // sake of completions; a "scope" in CF is essentially a struct, but not quite; and vice versa; so the abstraction is not perfect but it's close
    // we cache and reuse already-wrapped sybmol tables so that we can compare types by identity
    function structViewOfScope(scopeContents: SymbolTable, interfaceExtension?: Interface) : SymbolTableTypeWrapper {
        if (!interfaceExtension) interfaceExtension = BuiltinType.EmptyInterface;
        if (structViewCache.has([scopeContents, interfaceExtension])) {
            return structViewCache.get([scopeContents, interfaceExtension])!;
        }
        else {
            const type = SymbolTableTypeWrapper(scopeContents, interfaceExtension);
            structViewCache.set([scopeContents, interfaceExtension], type);
            return type;
        }
        
    }

    // fixme: consider libraries? walk into parent cfcs?
    function walkupToFindInterfaceDefinition(name: string, node: Node) : Interface | undefined {
        let working : Node | null = node;
        while (working) {
            if (working.containedScope) {
                if (working.containedScope.typeinfo.mergedInterfaces.has(name)) {
                    return working.containedScope.typeinfo.mergedInterfaces.get(name)!;
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
        const name = node.canonicalName;
        let isBuiltinScopeName = false;

        if (name !== undefined) {
            //const useContainer = getContainer(node);

            if (isStaticallyKnownScopeName(name)) {
                isBuiltinScopeName = true;
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
                        return;
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
            if (!resolvedSymbol) {
                // 1/29/22 -- fixed in checkStructLiteralInitializerMember, we shouldn't end up here if the key is an identifier
                // {x:y} -- don't warn on "x" being undefined, it's just a struct key
                //
                // const isKeyofKVPairStructLiteral = node.parent?.kind === NodeKind.structLiteralInitializerMember
                //     && node.parent.subType === StructLiteralInitializerMemberSubtype.keyed
                //     && node.parent.key === node
                //     && !node.parent.shorthand;
                if (warnOnUndefined && !isBuiltinScopeName /*&& !isKeyofKVPairStructLiteral*/) {
                    issueDiagnosticAtNode(node, `Cannot find name '${name}'.`, DiagnosticKind.warning);
                }
                return;
            }

            const FIXME_CURRENT_CONTAINER = findAncestor(node, (node) => !!node.containedScope); // should be a "currentContainer" member var
            const isOuterVar = resolvedSymbol.container !== FIXME_CURRENT_CONTAINER;
            const flowType = !CHECK_FLOW_TYPES ? undefined
                : isOuterVar ? undefined
                : node.flow ? determineFlowType(node.flow, resolvedSymbol.symTabEntry.symbolId)
                : undefined;

            const maybeMemberFunctionDefinition = tryGetCfcMemberFunctionDefinition(resolvedSymbol?.symTabEntry);

            // it is declared in the same container, but there is no apparent flowtype yet;
            // implying that it is used before assignment
            // except in the case of member functions, which are always visible
            // maybe we could hoist just member functions so they are always first in flow
            if (!isOuterVar && (CHECK_FLOW_TYPES && !flowType) && !maybeMemberFunctionDefinition) {
                if (CHECK_FLOW_TYPES) {
                    issueDiagnosticAtNode(node, `${node.uiName} is used before its first local declaration.`);
                }
                return;
            }

            if (CHECK_FLOW_TYPES && flowType && (flowType.flags & TypeFlags.containsUndefined)) {
                issueDiagnosticAtNode(node, `${node.uiName} is used before being assigned.`);
            }

            let forcedType : Type | undefined = undefined;

            if (maybeMemberFunctionDefinition) {
                const decl = maybeMemberFunctionDefinition;
                if (!(decl.flags & NodeFlags.checked)) {
                    if (!functionDefinitionHasUserSpecifiedReturnType(decl)) {
                        if (checkerStackContains(decl)) {
                            const name = decl.name?.ui;
                            const errorNode = getFunctionDefinitionNameTerminalErrorNode(decl);
                            issueDiagnosticAtNode(errorNode, `Function '${name}' requires an explicit return type because it directly or indirectly references itself.`);
                            forcedType = BuiltinType.any;
                            forcedReturnTypes.set(decl, forcedType);
                        }
                    }

                    if (!checkerStackContains(decl)) {
                        checkFunctionDefinition(decl);
                    }
                }
            }

            const apparentType = forcedType ? forcedType // when do we force a type? like forced-any or etc.?
                : flowType ? evaluateType(node, flowType) // if there's a flowtype, use that
                : getEffectivelyDeclaredType(resolvedSymbol.symTabEntry) || BuiltinType.any; //sourceFile.effectivelyDeclaredTypes.get(resolvedSymbol.symTabEntry.symbolId) || BuiltinType.any;

            
            setCachedEvaluatedNodeType(node, apparentType);
            setResolvedSymbol(node, resolvedSymbol);

            // no flow mutation here, so we don't set a cached flow type, though maybe there would be perf gains if
            // we "cached" it's current type as the "its type at whatever flownode this is"
        }
    }

    function checkIndexedAccess(node: IndexedAccess) {
        checkNode(node.root);

        let type : Type | undefined = getCachedEvaluatedNodeType(node.root);
        let symbol : SymbolResolution | undefined;

        if (!type || type === BuiltinType.any) {
            return;
        }

        // we set cached types on 'root' elements,
        // that is, the indexed-access root node itself, and the subsequent elements
        // not on the component identifiers, dots, brackets, etc.
        if (isStructLikeOrArray(type)) {
            for (let i = 0; i < node.accessElements.length; i++) {
                const element = node.accessElements[i];
                if (element.accessType === IndexedAccessType.bracket && type.kind === TypeKind.array) {
                    symbol = undefined;
                    type = type.memberType;
                }
                else if (element.accessType === IndexedAccessType.dot || element.accessType === IndexedAccessType.bracket) {
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
                        symbol = (node.root as Identifier).canonicalName === "this"
                            ? walkupThisToResolveSymbol(propertyName)
                            : walkupSuperToResolveSymbol(propertyName);
                        if (symbol) {
                            type = evaluateType(node, symbol.symTabEntry.firstLexicalType!);
                            setResolvedSymbol(element, symbol);
                            setEffectivelyDeclaredType(symbol.symTabEntry, type);
                        }
                        else {
                            type = undefined;
                        }
                    }
                    else {
                        symbol = getStructMember(element, type, propertyName);
                        if (symbol) {
                            setResolvedSymbol(element, symbol);
                            type = getEffectivelyDeclaredType(symbol.symTabEntry);
                        }
                        else {
                            type = undefined;
                        }
                    }

                    if (!type || type === BuiltinType.any) {
                        type = BuiltinType.any; // subsequent access elements will also be any
                        if (warnOnUndefined && propertyName) {
                            const errNode = element.accessType === IndexedAccessType.dot
                                ? element.property
                                : element.expr;
                            issueDiagnosticAtNode(errNode, `Cannot find property name '${propertyName}'.`, DiagnosticKind.warning);
                        }
                    }
                }
                else {
                    type = BuiltinType.any;
                }

                setCachedEvaluatedNodeType(element, type);
            }

            setCachedEvaluatedNodeType(node, type); // in `a.b.c`, the whole indexedAccess expression type is typeof c
            if (symbol) {
                setResolvedSymbol(node, symbol);
            }
        }
        else {
            setCachedEvaluatedNodeType(node, BuiltinType.any);
            for (const element of node.accessElements) {
                setCachedEvaluatedNodeType(element, BuiltinType.any);
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
        if (parentType === BuiltinType.any) {
            return;
        }

        if (node.accessType === IndexedAccessType.dot) {
            if (isStructLike(parentType)) {
                const name = node.property.token.text;
                if (isStructLike(parentType)) {
                    if (!parentType.members.has(name) && warnOnUndefined) {
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

    function mutateCfFunctionSignatureFromNonCompositeAnnotation(annotation: NonCompositeFunctionTypeAnnotation, signature: Mutable<cfFunctionSignature>, context: Node) : cfFunctionSignature {
        for (const paramAnnotation of annotation.params) {
            let sigParamUpdateTarget : cfFunctionSignatureParam | undefined = undefined;
            for (const param of signature.params) {
                if (param.canonicalName === paramAnnotation.name.text.toLowerCase()) {
                    sigParamUpdateTarget = param;
                    break;
                }
            }

            if (!sigParamUpdateTarget) {
                issueDiagnosticAtRange(
                    paramAnnotation.name.range,
                    `Annotation for parameter ${paramAnnotation.name.text} does not match any actual parameter name, and will be discarded.`,
                    DiagnosticKind.warning);
                continue;
            }

            const instantiatedAnnotatedParamType = evaluateType(context, paramAnnotation.type, new Map(), false, false);
            if (!instantiatedAnnotatedParamType) {
                issueDiagnosticAtRange(paramAnnotation.name.range, "Type failed to instantiate and will be treated as 'any'.", DiagnosticKind.warning);
            }
            else {
                // this is safe because sigParamUpdateTarget is a member of finalType,
                // and finalType has not yet met with the world; so it is treated as mutable
                (sigParamUpdateTarget as Mutable<cfFunctionSignatureParam>).paramType = instantiatedAnnotatedParamType;
            }
        }

        return signature;
    }

    function checkFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition, typeAnnotation = node.typeAnnotation) {
        if (node.flags & NodeFlags.checked) {
            return;
        }

        // for cfc member functions, some work was already done in the binder to extract the signature, but we didn't have visibility into CFC resolution there;
        // so here we can try to resolve CFC return types / param types

        const isMemberFunction = isCfcMemberFunctionDefinition(node);
        let memberFunctionSignature : cfFunctionSignature | undefined;
        
        let symbol = isNamedFunction(node)
            ? walkupScopesToResolveSymbol(sourceFile, node.name.canonical)
            : undefined;

        let POP_CHECKER_STACK = false; // easy to forget, make sure to pop if necessary

        if (isMemberFunction) {
            checkerStack.push(node);
            POP_CHECKER_STACK = true;

            if (node.kind === NodeKind.functionDefinition && node.name.canonical) {
                if (symbol) setResolvedSymbol(node, symbol);
                if (symbol?.symTabEntry.firstLexicalType && symbol.symTabEntry.firstLexicalType.kind === TypeKind.functionSignature) {
                    const freshType = evaluateType(symbol.container, symbol.symTabEntry.firstLexicalType); // consider a type annotation?
                    if (freshType.kind === TypeKind.functionSignature) {
                        memberFunctionSignature = freshType;
                        const variablesSymbol = sourceFile.containedScope.variables?.get(node.name.canonical);
                        // keep both `variables` and `this` in sync with member functions
                        if (variablesSymbol) {
                            variablesSymbol.firstLexicalType = freshType;
                            sourceFile.containedScope.variables?.set(node.name.canonical, variablesSymbol);
                            sourceFile.containedScope.this?.set(node.name.canonical, variablesSymbol);
                        }
                    }
                }
            }
        }

        const cfSyntaxDirectedTypeSig = memberFunctionSignature ?? (evaluateType(node, extractCfFunctionSignature(node)) as cfFunctionSignature);
        let finalType : cfFunctionSignature | cfFunctionOverloadSet = cfSyntaxDirectedTypeSig;
        
        if (typeAnnotation?.shimKind === TypeShimKind.annotation) {
            const evaluatedSignature = evaluateType(node, typeAnnotation.type);
            if (evaluatedSignature.kind === TypeKind.functionSignature) {
                if (!isAnnotatedSigCompatibleWithCfFunctionSig(evaluatedSignature, cfSyntaxDirectedTypeSig)) {
                    issueDiagnosticAtNode(node, `Type '${stringifyType(cfSyntaxDirectedTypeSig)}' is not assignable to the annotated type '${stringifyType(typeAnnotation.type)}'.`)
                }
                else {
                    // copy cf-sig param names into annotated-type param names
                    // intent is we don't have to duplicate names from code signature to annotation signature, although we still need to give some name,
                    // like `(x: any, x: string, x: cfc<a.b.c>) => any`
                    // and then each param name has its name "filled in" by the actual cf code signature
                    for (let i = 0; i < cfSyntaxDirectedTypeSig.params.length; i++) {
                        // fixme: these constcasts should should somehow be part of the constructor
                        (evaluatedSignature.params[i] as Mutable<cfFunctionSignatureParam>).canonicalName = cfSyntaxDirectedTypeSig.params[i].canonicalName;
                        (evaluatedSignature.params[i] as Mutable<cfFunctionSignatureParam>).uiName = cfSyntaxDirectedTypeSig.params[i].uiName;
                    }
                }

                finalType = evaluatedSignature;
                if (isMemberFunction && sourceFile.containedScope.variables!.has(node.name.canonical)) {
                    sourceFile.containedScope.variables!.get(node.name.canonical)!.firstLexicalType = finalType; // updates the 'this' copy of the symbol too, the refs are the same
                }
            }
            else if (evaluatedSignature.kind === TypeKind.functionOverloadSet) {
                for (const overload of evaluatedSignature.overloads) {
                    if (!isAnnotatedOverloadCompatibleWithCfFunctionSig(overload.params, cfSyntaxDirectedTypeSig.params, overload.returns, cfSyntaxDirectedTypeSig.returns)) {
                        issueDiagnosticAtNode(node, `Type '${stringifyType(cfSyntaxDirectedTypeSig)}' is not assignable to the annotated type '${stringifyType(typeAnnotation.type)}'.`)
                        break;
                    }
                }
                finalType = evaluatedSignature;
                if (isMemberFunction && sourceFile.containedScope.variables!.has(node.name.canonical)) {
                    sourceFile.containedScope.variables!.get(node.name.canonical)!.firstLexicalType = finalType; // updates the 'this' copy of the symbol too, the refs are the same
                }
            }
            else if (evaluatedSignature !== BuiltinType.any) {
                issueDiagnosticAtNode(node, `Expected a function signature as an annotated type, but got type '${stringifyType(evaluatedSignature)}'.`)
            }
        }
        else if (typeAnnotation?.shimKind === TypeShimKind.nonCompositeFunctionTypeAnnotation) {
            mutateCfFunctionSignatureFromNonCompositeAnnotation(typeAnnotation, finalType, node);
        }

        // put access modifiers on the type signature for cfc member functions
        if (node.kind === NodeKind.functionDefinition && isMemberFunction) {
            const accessModifier = getFunctionDefinitionAccessLiteral(node);
            let accessModifierFlag : TypeFlags = TypeFlags.none;
            switch (accessModifier) {
                case "remote":    accessModifierFlag = TypeFlags.remote;    break;
                case "public":    accessModifierFlag = TypeFlags.public;    break;
                case "protected": accessModifierFlag = TypeFlags.protected; break;
                case "private":   accessModifierFlag = TypeFlags.private;   break;
                default: exhaustiveCaseGuard(accessModifier);
            }
            (finalType as Mutable<Type>).flags |= accessModifierFlag;
        }
      
        setCachedEvaluatedNodeType(node, finalType);

        if (symbol) {
            // this may not be always true
            // we might need to infer the return type
            // we can set it again after inference, but maybe would get wrong return type results while checking a recursive call?
            // we know the args are right (except in a generic function, where the caller will skip this method and check/infer the args itself?)
            setEffectivelyDeclaredType(symbol.symTabEntry, finalType);
        }

        // fixme: why do we only do this for TypeKind.functionSignature?
        const inferredReturnType = finalType.kind === TypeKind.functionSignature ? (() => {
            for (const param of finalType.params) {
                const argSymbol = node.containedScope!.arguments!.get(param.canonicalName);
                if (!argSymbol) { // weird error case?:  we have a param but did not set it in arguments scope?
                    continue;
                }
                argSymbol.firstLexicalType = param.paramType;
            }

            const inferredReturnType = checkFunctionBody(node, finalType.params);
            return forcedReturnTypes.has(node)
                ? forcedReturnTypes.get(node)!
                : inferredReturnType;
        })() : undefined;

        if (finalType.kind === TypeKind.functionSignature && CHECK_RETURN_TYPES && inferredReturnType) {
            if (!isAssignable(inferredReturnType, finalType.returns, /*sourceIsLiteralExpr*/ false, /*forReturnType*/ true)) {
                if (node.kind === NodeKind.functionDefinition) {
                    let literalReturnType : CanonicalizedName | undefined = undefined;
                    if (node.fromTag) {
                        const literal = getTriviallyComputableString(getAttributeValue(node.attrs, "returntype"));
                        if (literal) {
                            literalReturnType = {ui: literal, canonical: literal.toLowerCase()};
                        }
                    }
                    else {
                        if (node.returnType) {
                            literalReturnType = stringifyDottedPath(node.returnType)
                        }
                    }
                    
                    if (node.finalFlow !== UnreachableFlow && literalReturnType && (literalReturnType.canonical !== "void" && literalReturnType.canonical !== "any")) {
                        const range = node.fromTag ? getAttributeValue(node.attrs, "returntype")!.range : node.returnType!.range;
                        issueDiagnosticAtRange(range, `Function does not have a final return statement, and may return 'null' which is not assignable to declared return type '${literalReturnType.ui}'.`);
                    }
                    else {
                        const range = node.fromTag ? node.tagOrigin.startTag!.range : mergeRanges(node.accessModifier, node.returnType, node.functionToken, node.nameToken);
                        issueDiagnosticAtRange(range, `Function declares return type '${stringifyType(finalType.returns)}' but actual return type is '${stringifyType(inferredReturnType)}'.}`);
                    }
                }
            }
            else {
                if (!node.typeAnnotation) {
                    (finalType as Mutable<cfFunctionSignature>).returns = inferredReturnType;
                }
            }
        }

        node.flags |= NodeFlags.checked;

        if (POP_CHECKER_STACK) {
            checkerStack.pop();
        }

        if (symbol) {
            // fixme: interplay between inference and explicitly declared type
            // what do we consider an explicitly declared type
            // what if the user says "function foo(string bar, baz) {}"?
            //   - there is no return type
            //   - half of the arguments have explicit types
            //   - considered a user defined type that overrides inference?
            //   - probably we just care about return type
            //

            symbol.symTabEntry.firstLexicalType = finalType;

            if (node.flow) { // fixme: should always have this?
                // having a symbol implies that we intended to have set an assignment flow on this node,
                // because the name is now of this type from this point in the flow on
                // we have a symbol for `function foo() {}` but not for `var foo = function() {}`
                setCachedEvaluatedFlowType(node.flow!, symbol.symTabEntry.symbolId, finalType);
            }
        }
    }

    // fixme: we probably have enough info from "effectivelyDeclaredTypes" to set flowstart type information,
    // without having to pass in `params[]`
    function checkFunctionBody(node: FunctionDefinition | ArrowFunctionDefinition, params: cfFunctionSignatureParam[]) : Type {
        // "start flow" of a function has the argument types set to their declared types
        function setStartFlowArgTypes(startFlow: Flow) {
            for (const param of params) {
                if (node.containedScope!.arguments!.has(param.canonicalName)) {
                    const symbolId = node.containedScope!.arguments!.get(param.canonicalName)!.symbolId;
                    setCachedEvaluatedFlowType(startFlow, symbolId, param.paramType);
                }
            }
        }

        const savedFlowBecameUnreachable = flowBecameUnreachable;
        const savedReturnTypes = returnTypes;
        returnTypes = [];

        if (node.kind === NodeKind.functionDefinition && node.fromTag) {
            if (node.body.length > 0) {
                setStartFlowArgTypes(node.body[0].flow!);   
            }
            checkList(node.body);
        }
        else {
            setStartFlowArgTypes(node.body.flow!);
            checkNode(node.body);
        }

        const postFlow = sourceFile.endOfNodeFlowMap.get(node.nodeId);
        if (postFlow && isReachableFlow(postFlow)) {
            returnTypes.push(BuiltinType.null);
        }

        let actualReturnType : Type;
        if (node.kind === NodeKind.arrowFunctionDefinition && node.body.kind !== NodeKind.block) {
            actualReturnType = getCachedEvaluatedNodeType(node.body);
        }
        else {
            actualReturnType = unionify(returnTypes);
        }

        returnTypes = savedReturnTypes;
        flowBecameUnreachable = savedFlowBecameUnreachable;

        return actualReturnType;
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

        const postFlow = sourceFile.endOfNodeFlowMap.get(node.nodeId);
        if (postFlow && postFlow !== UnreachableFlow) {
            postFlow.predecessors = postFlow.predecessors.filter((flow) => !flow.becameUnreachable);
            if (postFlow.predecessors.length === 0) {
                postFlow.becameUnreachable = true;
            }
        }
    }

    function checkSwitchCase(node: SwitchCase) {
        const savedFlowBecameUnreachable = flowBecameUnreachable;
        flowBecameUnreachable = false;

        if (node.fromTag) {
            if (node.tagOrigin.startTag?.canonicalName === "cfcase") {
                const attr = getAttributeValue((node.tagOrigin.startTag as CfTag.Common).attrs, "value") ?? null;
                // pre-cf2021 it has to be a string or numeric literal
                checkNode(attr);
            }
            checkList(node.body);
            node.flow!.becameUnreachable = flowBecameUnreachable;
            flowBecameUnreachable = savedFlowBecameUnreachable;
            return;
        }

        if (node.caseType === SwitchCaseType.case) {
            // pre cf-2021 it has to be a string or numeric literal
            checkNode(node.expr);
        }
        
        checkList(node.body);
        node.flow!.becameUnreachable = flowBecameUnreachable;

        flowBecameUnreachable = savedFlowBecameUnreachable;
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
        setCachedEvaluatedNodeType(node, unionify([
            getCachedEvaluatedNodeType(node.ifTrue),
            getCachedEvaluatedNodeType(node.ifFalse)
        ]));
    }

    //
    // type lookup
    //
    function tryGetTagCallSignature(tagName: string) : Type | undefined {
        const taglib = libTypeResolver("__cfTags")
        if (taglib && taglib.kind === TypeKind.interface) {
            return taglib.members.get(tagName)?.firstLexicalType;
        }
        else {
            return undefined;
        }
    }

    function walkUpContainersToResolveType(context: Node, type: cfTypeId) : Type | undefined {
        let node : Node | null = context;
        const typeName = type.name;

        while (node) {
            if (node.containedScope) {
                if (node.containedScope.typeinfo) {
                    if (node.containedScope.typeinfo.aliases.has(typeName)) {
                        return node.containedScope.typeinfo.aliases.get(typeName)!;
                    }
                }
                if (node.kind === NodeKind.sourceFile) {
                    for (const lib of node.libRefs.values()) {
                        if (lib.containedScope.typeinfo.mergedInterfaces.has(type.name)) {
                            return lib.containedScope.typeinfo.mergedInterfaces.get(type.name)!;
                        }
                    }
                    return libTypeResolver(type.name);
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
            setCachedEvaluatedNodeType(node, BuiltinType.EmptyInterface);
            return;
        }

        checkList(node.members);

        const memberTypes = new Map<string, SymTabEntry>();
        let keysAreStaticallyKnowable = true;

        outer:
        for (const member of node.members) {
            switch (member.subType) {
                case StructLiteralInitializerMemberSubtype.keyed: {
                    const memberType = getCachedEvaluatedNodeType(member);
                    if (memberType === BuiltinType.never) continue; // spreads or shorthand in unsupported engines may produce this?

                    const key = getTriviallyComputableString(member.key);

                    // if the key is an expression, we can't statically know the key
                    // and we're forced to say the whole record is just `{}`
                    if (!key || member.key.kind !== NodeKind.identifier && member.key.kind !== NodeKind.simpleStringLiteral) {
                        keysAreStaticallyKnowable = false;
                        break outer;
                    }

                    const canonicalName = key.toLowerCase();
                    memberTypes.set(canonicalName, {
                        symbolId: -1, // do we need this here?
                        uiName: key,
                        canonicalName,
                        declarations: [member],
                        firstLexicalType: undefined,
                        links: {
                            effectiveDeclaredType: memberType
                        }
                    });

                    break;
                }
                case StructLiteralInitializerMemberSubtype.spread: {
                    // ? no-op ?
                    break;
                }
                default: exhaustiveCaseGuard(member);
            }
        }

        if (keysAreStaticallyKnowable) {
            setCachedEvaluatedNodeType(node, Struct(memberTypes));
        }
        else {
            setCachedEvaluatedNodeType(node, BuiltinType.EmptyInterface);
        }
    }

    function checkStructLiteralInitializerMember(node: StructLiteralInitializerMember) {
        switch (node.subType) {
            case StructLiteralInitializerMemberSubtype.keyed: {
                if (node.shorthand) {
                    if (!supports.structLiteralShorthand(engineVersion)) {
                        issueDiagnosticAtNode(node, `CF engine ${engineVersion.uiString} does not support struct literal shorthand notation.`);
                        setCachedEvaluatedNodeType(node, BuiltinType.never);
                        break;
                    }
                    if (node.key.kind !== NodeKind.identifier || !node.key.canonicalName) { // fixme: when is this undefined or empty?
                        issueDiagnosticAtNode(node, "Shorthand struct literal initializers must be identifiers.");
                        setCachedEvaluatedNodeType(node, BuiltinType.never);
                        break;
                    }

                    checkNode(node.key);
                    const symbol = walkupScopesToResolveSymbol(node, node.key.canonicalName);
                    if (symbol) {
                        const type = getEffectivelyDeclaredType(symbol.symTabEntry) || BuiltinType.any; // also could prefer flow type if it exists
                        setCachedEvaluatedNodeType(node, type);
                    }
                }
                else {
                    //
                    // simple string literal key is a literal key
                    //   - {"x": 1} : {x: number}
                    // identifier key is a literal key
                    //   - {x: 1} : {x: number}
                    // indexed-access key implies nested properties
                    //   - {x.y: 1}    : {x: {y: number}} (dot access)
                    //   - {x["y"]: 1} : {x: {y: number}} (bracket access) (on lucee ok, on adobe an error)
                    // arbitrary expressions as dynamic keys:
                    //   - {arbitrary + expression: 1} : {? : number} (on lucee ok, on adobe an error)
                    // saner use of interpolated string for dynamic keys
                    //   - {"#expr#": key}
                    //

                    if (node.key.kind !== NodeKind.simpleStringLiteral
                        && node.key.kind !== NodeKind.indexedAccess
                        && node.key.kind !== NodeKind.identifier
                    ) {
                        checkNode(node.key);
                    }
                    else {
                        //
                        // no-op - it's ok to not check string / identifier / indexed-access keys
                        // because we can't statically analyze them, and they have no sub expressions to check
                        // (indexed-access might appear to, in the bracket-access case, but really its treated as a series of strings by the engine, see notes above)
                        // (e.g. {x[y]: v} isn't valid, it must be {x["y"]: v})
                        // (this might warrant a separate node type for such cases)
                        // 
                    }
                    checkNode(node.expr);
                    setCachedEvaluatedNodeType(node, getCachedEvaluatedNodeType(node.expr));
                }
                break;
            }
            case StructLiteralInitializerMemberSubtype.spread: {
                if (!supports.structLiteralSpread(engineVersion)) {
                    issueDiagnosticAtNode(node, `CF engine ${engineVersion} does not support struct literal spread syntax.`)
                    setCachedEvaluatedNodeType(node, BuiltinType.never);
                }
                checkNode(node.expr);
                break;
            }
            default: exhaustiveCaseGuard(node);
        }
    }

    function checkArrayLiteral(node: ArrayLiteral) {
        checkList(node.members);
        const arrayMemberTypes = node.members.map((member) => getCachedEvaluatedNodeType(member));
        const membersAsUnion = unionify(arrayMemberTypes);
        const uninstantiatedArray = UninstantiatedArray(membersAsUnion);
        const instantiatedArrayType = evaluateType(node, uninstantiatedArray);
        setCachedEvaluatedNodeType(node, instantiatedArrayType);
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
        const cfc = cfcResolver(ComponentResolutionArgs(sourceFile.absPath, cfcName));
        if (!cfc) return;
        const cfcThis = Cfc(cfc.sourceFile);
        setCachedEvaluatedNodeType(node, cfcThis);
        
        const initSig = cfcThis.members.get("init");
        if (initSig && initSig.firstLexicalType?.kind === TypeKind.functionSignature) {
            setCachedEvaluatedNodeType(node.callExpr.left, initSig.firstLexicalType);
            checkCallLikeArguments(initSig.firstLexicalType, node.callExpr.args, node.callExpr.left.range, /*isNewExpr*/ true);
        }
    }

    function getStructMember(element: Node, type: Type, canonicalName: string) : SymbolResolution | undefined {
        if (!isStructLikeOrArray(type)) {
            return undefined;
        }

        let member = type.members.get(canonicalName);

        if (!member && type.kind === TypeKind.symbolTableTypeWrapper && type.interfaceExtension) {
            member = type.interfaceExtension.members.get(canonicalName);
        }
        else if (type.kind === TypeKind.cfc) {
            member = type.interfaceExtension?.members.get(canonicalName);
            if (!member) {
                let workingNode : SourceFile | null = type.cfc;
                while (workingNode) {
                    member = workingNode.containedScope.this!.get(canonicalName)
                        ?? workingNode.containedScope.typeinfo.mergedInterfaces.get("this")?.members.get(canonicalName);
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
    let libTypeResolver : LibTypeResolver;
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
                case "LibTypeResolver":
                    libTypeResolver = installables[key]!;
                    break;
                default: exhaustiveCaseGuard(key);
            }
        }
    }

    function isReachableFlow(flow: Flow) {
        return flow !== UnreachableFlow && !flow.becameUnreachable;
    }
    //
    // type evaluation
    //
    const evaluateType = (function() {
        type NodeTrie = Map<Type, NodeTrie> & Map<null, Type | "PENDING"> ;
        let typeConstructorInvocationCacheTrie : NodeTrie = new Map();
        
        const enum TypeCache_Status { resolved, resolving, noCache, failure };
        type TypeCache_Resolution = TypeCache_Cached | TypeCache_NoCache;
        interface TypeCache_Cached {
            status: TypeCache_Status.resolved,
            value: Type
        }
        interface TypeCache_NoCache {
            status: TypeCache_Status.resolving | TypeCache_Status.noCache | TypeCache_Status.failure
        }
        
        function setCachedTypeConstructorInvocation(typeFunction: Type, args: readonly Type[], val: "PENDING" | Type) : void {
            function getChildTrieMapOrNull(thisLevel: NodeTrie, type: Type) : NodeTrie | null {
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

        function getCachedTypeConstructorInvocation(typeConstructor: Type, args: readonly Type[]) : TypeCache_Resolution {
            let trieDescender = typeConstructorInvocationCacheTrie.get(typeConstructor);
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

        function evaluateType(
            context: Node,
            type: Type | null,
            typeParamMap?: ReadonlyMap<string, Type>,
            partiallyApplyGenericFunctionSigs?: boolean,
            falbackToAny?: true
        ) : Type;
        function evaluateType(
            context: Node,
            type: Type | null,
            typeParamMap?: ReadonlyMap<string, Type>,
            partiallyApplyGenericFunctionSigs?: boolean,
            falbackToAny?: false
        ) : Type | null;
        function evaluateType(
            context: Node,
            type: Type | null,
            typeParamMap: ReadonlyMap<string, Type> = type?.capturedContext ?? new Map(),
            partiallyApplyGenericFunctionSigs = false,
            fallbackToAny = true
        ) : Type | null {
            let depth = 0;

            const result = typeWorker(type, typeParamMap, partiallyApplyGenericFunctionSigs);

            typeConstructorInvocationCacheTrie = new Map();

            return result;

            function typeWorker(type: Readonly<Type> | null,
                                typeParamMap: ReadonlyMap<string, Type> | undefined = type?.capturedContext,
                                partiallyApplyGenericFunctionSigs = false,
                                lookupDeferrals?: ReadonlySet<string>) : Type | null {
                if (depth > 64) {
                    return BuiltinType.never;
                }
                try {
                    depth++;
                
                    if (!type) {
                        return fallbackToAny ? BuiltinType.any : null;
                    }
                    else if (type.concrete) {
                        return type;
                    }
                    else if (type.kind === TypeKind.array) {
                        return type;
                    }
                    else if (type.kind === TypeKind.intersection) {
                        return type;
                    }
                    else if (type.kind === TypeKind.union) {
                        // need to dedupe and etc.
                        // it is currently possible to end up with many adhoc unique instances of the same union
                        // which should structurally compare equal, but we want object identity when we can
                        const evaluatedTypes : Type[] = [];
                        for (const unionMemberType of type.types) {
                            const workedUnionType = typeWorker(unionMemberType, typeParamMap);
                            if (workedUnionType) evaluatedTypes.push(workedUnionType);
                            else return null;
                        }
                        const freshUnion = unionify(evaluatedTypes); 
                        if (structurallyCompareTypes(type, freshUnion) === 0) {
                            return type;
                        }
                        else {
                            return freshUnion;
                        }
                    }
                    else if (type.kind === TypeKind.struct) {
                        const evaluatedStructContents = new Map<string, SymTabEntry>();
                        let concrete = true;
                        for (const [canonicalName, symTabEntry] of type.members.entries()) {
                            const evaluatableType = (symTabEntry.links?.effectiveDeclaredType ?? symTabEntry.firstLexicalType)!;
                            const evaluatedType = typeWorker(evaluatableType);
                            if (!evaluatedType) return null;

                            if (evaluatableType !== evaluatedType) { // fixme: merely expanding a non-generic alias will trigger this, since (alias !== *alias)
                                concrete = false;
                            }
                            evaluatedStructContents.set(canonicalName, {
                                uiName: symTabEntry.uiName,
                                declarations: null,
                                canonicalName,
                                firstLexicalType: undefined,
                                links: {
                                    effectiveDeclaredType: evaluatedType
                                },
                                symbolId: -1,
                            });
                        }

                        const result = Struct(evaluatedStructContents);

                        if (concrete) {
                            (result as Mutable<Type>).concrete = true;
                        }

                        return type;
                    }
                    else if (type.kind === TypeKind.functionSignature || (type.kind === TypeKind.genericFunctionSignature && partiallyApplyGenericFunctionSigs)) {
                        const sig = type;
                        const updatedLookupDeferrals = type.kind === TypeKind.genericFunctionSignature
                            ? new Set(
                                ...type.typeParams.map(param => param.name),
                                ...(lookupDeferrals ?? [])
                            )
                            : lookupDeferrals;

                        const params : cfFunctionSignatureParam[] = [];
                        let originalTypeWasConcrete = true;

                        for (let i = 0; i < sig.params.length; i++) {
                            const freshType = typeWorker(sig.params[i].paramType,
                                typeParamMap, partiallyApplyGenericFunctionSigs, updatedLookupDeferrals);

                            if (!freshType) return null;

                            originalTypeWasConcrete = originalTypeWasConcrete && (freshType === sig.params[i].paramType);
                            params.push(
                                cfFunctionSignatureParam(
                                    !(sig.params[i].flags & TypeFlags.optional),
                                    freshType,
                                    sig.params[i].uiName,
                                    !!(sig.params[i].flags & TypeFlags.spread)));
                        }

                        const returns = typeWorker(sig.returns, typeParamMap, partiallyApplyGenericFunctionSigs, updatedLookupDeferrals);

                        if (!returns) return null;

                        originalTypeWasConcrete = originalTypeWasConcrete && (returns === sig.returns);
                        
                        if (type.kind === TypeKind.genericFunctionSignature || !originalTypeWasConcrete) {
                            return type.kind === TypeKind.genericFunctionSignature
                                ? cfGenericFunctionSignature(sig.uiName, type.typeParams, params, returns, sig.attrs)
                                : cfFunctionSignature(sig.uiName, params, returns, sig.attrs);
                        }
                        else {
                            return type;
                        }
                    }
                    else if (type.kind === TypeKind.cfcLookup) {
                        let resolvedCfc : CfcResolution | undefined;

                        if (type.explicitSpecifier) {
                            resolvedCfc = cfcResolver(type.explicitSpecifier);
                        }
                        else {
                            const cfcName = type.cfcName.literalValue.toString();
                            resolvedCfc = cfcResolver(ComponentResolutionArgs(sourceFile.absPath, cfcName));
                        }

                        if (resolvedCfc) {
                            const freshType = Cfc(resolvedCfc.sourceFile);
                            (freshType as Mutable<Type>).underlyingType = type;
                            return freshType;
                        }

                        return fallbackToAny ? BuiltinType.any : null;
                    }
                    else if (type.kind === TypeKind.typeConstructorInvocation) {
                        if (type.left.kind === TypeKind.interface) { // not generic since it is not a constructor invocation...
                            const result = instantiateInterfaceWithPreinstantiatedArgs(type.left, new Map());
                            if (result.status === TypeCache_Status.resolving) {
                                if (typeParamMap) {
                                    return createType({...type, capturedContext: typeParamMap});
                                }
                                return type;
                            }
                            else if (result.status === TypeCache_Status.resolved) {
                                return result.value;
                            }
                            else if (result.status === TypeCache_Status.failure) {
                                return null;
                            }
                        }
                        else if (type.left.kind === TypeKind.typeId) {
                            const constructor = typeParamMap?.get(type.left.name) || walkUpContainersToResolveType(context, type.left) || null;
                            if (constructor && constructor.kind === TypeKind.interface) {
                                for (const arg of type.args) {
                                    if (arg.kind === TypeKind.typeId && lookupDeferrals?.has(arg.name)) {
                                        return type;
                                    }
                                }

                                let instantiableParamMap : ReadonlyMap<string, Type>;
                                if (type.capturedContext) {
                                    instantiableParamMap = type.capturedContext;
                                }
                                else {
                                    const mapBuilder = new Map<string, Type>();
                                    if (constructor.typeParams) {
                                        for (let i = 0; i < constructor.typeParams?.length ?? 0; i++) {
                                            const name = constructor.typeParams[i].name;
                                            const argType = typeWorker(type.args[i], typeParamMap);
                                            if (!argType) return null;
                                            mapBuilder.set(name, argType);
                                        }
                                    }
                                    instantiableParamMap = mapBuilder;
                                }

                                const result = instantiateInterfaceWithPreinstantiatedArgs(constructor, instantiableParamMap);

                                if (result.status === TypeCache_Status.resolving) {
                                    return createType({...type, capturedContext: instantiableParamMap});
                                }
                                else if (result.status === TypeCache_Status.resolved) {
                                    return result.value;
                                }
                                else if (result.status === TypeCache_Status.failure) {
                                    return null;
                                }
                            }
                        }
                        
                        return fallbackToAny ? BuiltinType.any : null;
                        
                    }
                    else if (type.kind === TypeKind.typeId && !lookupDeferrals?.has(type.name)) {
                        // @fixme: should error if we can't find it; and return never ?
                        let result = typeParamMap?.get(type.name) || walkUpContainersToResolveType(context, type) || null;
                        if (!result) {
                            return fallbackToAny ? BuiltinType.any : null;
                        }
                        if (type.indexChain) {
                            for (const name of type.indexChain) {
                                if (!isStructLike(result)) return BuiltinType.any; // should error with "not indexable" or etc.
                                const nextType = result.members.get(name)?.firstLexicalType;
                                if (nextType) result = typeWorker(nextType);
                                if (!result) return null;
                            }
                        }
                        
                        return typeWorker(result);
                    }
                    else {
                        // a type not requiring evaluation: number, string, boolean,
                        return type;
                    }
                }
                finally {
                    depth--;
                }

                type InterfaceInstantiationResult =
                    | {status: TypeCache_Status.resolved, value: Type}
                    | {status: TypeCache_Status.resolving} // for something like interface X<T> { v: X<T> }, X<T> is already resolving when we instantiate v
                    | {status: TypeCache_Status.failure}

                function instantiateInterfaceWithPreinstantiatedArgs(iface: Interface, preInstantiatedArgMap: ReadonlyMap<string, Type>) : InterfaceInstantiationResult {
                    const argsArray : Type[] = [];
                    if (iface.typeParams) {
                        for (const typeParam of iface.typeParams) {
                            if (!preInstantiatedArgMap.has(typeParam.name)) { // bad invocation
                                return {status: TypeCache_Status.resolved, value: BuiltinType.any};
                            }
                            argsArray.push(preInstantiatedArgMap.get(typeParam.name)!);
                        }
                    }

                    const cached = getCachedTypeConstructorInvocation(iface, argsArray);
                    if (cached.status === TypeCache_Status.resolved) {
                        return cached;
                    }
                    else if (cached.status === TypeCache_Status.resolving) {
                        return {status: cached.status};
                    }
                
                    // say our current evaluation is for `T<U>`
                    // set `T<U>` to "PENDING" so that we have something to check for to not recurse infinitely on something like `T<U> = {foo: T<U>}`
                    setCachedTypeConstructorInvocation(iface, argsArray, "PENDING");

                    const instantiatedMembers = new Map<string, SymTabEntry>();
                    for (const [name, symTabEntry] of iface.members) {
                        const freshType = typeWorker(symTabEntry.firstLexicalType!, preInstantiatedArgMap, true);
                        if (!freshType) {
                            return {status: TypeCache_Status.failure};
                        }
                        instantiatedMembers.set(name, {...symTabEntry, firstLexicalType: freshType});
                    }

                    let result : Type;

                    if (iface.name === "Array") {
                        // should always have a "T", although we need to enforce user defined interfaces to have always be Array<T>
                        result = cfArray(preInstantiatedArgMap.get("T") ?? BuiltinType.any, instantiatedMembers);
                    }
                    else {
                        result = Struct(instantiatedMembers, iface.indexSignature);
                        (result as Mutable<Type>).underlyingType = iface;
                    }

                    setCachedTypeConstructorInvocation(iface, argsArray, result);
                    return {status: TypeCache_Status.resolved, value: result};
                }
            }
        }

        return evaluateType;
    })();

    return {
        check,
        getCachedEvaluatedNodeType: getCachedEvaluatedNodeTypeImpl,
        getSymbol: getSymbolImpl,
        install,
    }
}

export interface CheckerInstallable {
    CfcResolver: CfcResolver,
    EngineSymbolResolver: EngineSymbolResolver,
    LibTypeResolver: LibTypeResolver
}

export type Checker = ReturnType<typeof Checker>;

//
// this is the "QuickInstance" decorator; we're not supporting this at the moment
//
// function QuickInstance(sourceFile: SourceFile) : void {
//     if (sourceFile.cfFileType !== CfFileType.cfc) return;

//     const methodsToAdd : SymTabEntry[] = [];

//     // method names matching /^scope(.*)/ get replaced with methods named /$1/
//     // also has its first param removed
//     for (const symTabEntry of sourceFile.containedScope.this!.values()) {
//         if (symTabEntry.type.kind === TypeKind.functionSignature) {
//             if (/^scope/i.test(symTabEntry.uiName)) {
//                 (symTabEntry.type as Mutable<Type>).flags &= ~(TypeFlags.private | TypeFlags.public | TypeFlags.protected | TypeFlags.remote);
                
//                 let freshUiName = symTabEntry.uiName.replace(/^scope/i, "");
//                 freshUiName = freshUiName[0].toLowerCase() + freshUiName.slice(1);
                
//                 // fixme: can't set access flags with function sig constructor?
                
//                 const freshType = cfFunctionSignature(freshUiName, symTabEntry.type.params.slice(1), Cfc(sourceFile), symTabEntry.type.attrs);
//                 (symTabEntry.type as Mutable<Type>).flags |= TypeFlags.private;
//                 (freshType as Mutable<Type>).flags |= TypeFlags.public;

//                 methodsToAdd.push({
//                     uiName: freshUiName,
//                     canonicalName: freshUiName.toLowerCase(),
//                     type: freshType,
//                     declarations: symTabEntry.declarations
//                 });
//             }
//         }
//     }

//     // properties get "where getters",
//     // i.e. property named "column" gets a public "getter" named "whereColumn"
//     for (const symTabEntry of sourceFile.containedScope.variables!.values()) {
//         const propertyDecl = symTabEntry.declarations?.find((node) : node is Property => node.kind === NodeKind.property);
//         if (propertyDecl) {
//             let propertyName = getTriviallyComputableString(getAttributeValue(propertyDecl.attrs, "name"));
//             if (propertyName) {
//                 propertyName = propertyName[0].toUpperCase() + propertyName.slice(1);
//                 const param = cfFunctionSignatureParam(true, BuiltinType.string, "");
//                 methodsToAdd.push({
//                     uiName: "where" + propertyName,
//                     canonicalName: "where" + propertyName.toLowerCase(),
//                     type: cfFunctionSignature("where" + propertyName, [param], Cfc(sourceFile), []),
//                     declarations: [propertyDecl]
//                 })
//             }
//         }
//     }

//     for (const method of methodsToAdd) {
//         sourceFile.containedScope.this!.set(method.canonicalName, method);
//     }
// }
