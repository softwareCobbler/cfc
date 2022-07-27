import { Diagnostic, SourceFile, Node, NodeKind, BlockType, IndexedAccess, StatementType, CallExpression, IndexedAccessType, CallArgument, BinaryOperator, BinaryOpType, FunctionDefinition, ArrowFunctionDefinition, IndexedAccessChainElement, NodeFlags, VariableDeclaration, Identifier, Flow, isStaticallyKnownScopeName, For, ForSubType, UnaryOperator, Do, While, Ternary, StructLiteral, StructLiteralInitializerMemberSubtype, StructLiteralInitializerMember, ArrayLiteral, ArrayLiteralInitializerMember, Catch, Try, Finally, New, Switch, CfTag, SwitchCase, SwitchCaseType, Conditional, ConditionalSubtype, SymTabEntry, mergeRanges, ReturnStatement, SymbolResolution, SymbolTable, UnreachableFlow, DiagnosticKind, TagAttribute, SymbolId, TypeShimKind, NonCompositeFunctionTypeAnnotation, Property, TypeAnnotation, InterpolatedStringLiteral, TextSpan, HashWrappedExpr, SymbolFlags, NamedAnnotation } from "./node";
import { CfcResolution, CfcResolver, ComponentResolutionArgs, EngineSymbolResolver, LibTypeResolver, ProjectOptions, ProjectRelativeImportLookup } from "./project";
import { Scanner, CfFileType, SourceRange, TokenType } from "./scanner";
import { cfFunctionSignature, Struct, cfUnion, BuiltinType, TypeFlags, UninstantiatedArray, extractCfFunctionSignature, Type, stringifyType, cfFunctionSignatureParam, cfFunctionOverloadSet, cfTypeId, SymbolTableTypeWrapper, Cfc, Interface, createType, createLiteralType, typeFromJavaLikeTypename, structurallyCompareTypes, TypeKind, isStructLike, cfArray, cfStructLike, isStructLikeOrArray, cfGenericFunctionSignature, cfKeyof, TypeConstructorParam, cfInterpolatedString, cfTuple, TypeConstructor, cfLiteralType, TypeIndexedAccessType, CfcLookup } from "./types";
import { CanonicalizedName, exhaustiveCaseGuard, findAncestor, getUserSpecifiedReturnTypeType, getAttributeValue, getCallExpressionNameErrorRange, getComponentAttrs, getContainingFunction, getFunctionDefinitionAccessLiteral, getFunctionDefinitionNameTerminalErrorNode, getSourceFile, getTriviallyComputableString, isCfcMemberFunctionDefinition, isInCfcPsuedoConstructor, isInEffectiveConstructorMethod, isLiteralExpr, isNamedFunction, isSimpleOrInterpolatedStringLiteral, Mutable, stringifyDottedPath, stringifyLValue, stringifyStringAsLValue, tryGetCfcMemberFunctionDefinition, visit, escapeRegExp } from "./utils";
import { walkupScopesToResolveSymbol as externWalkupScopesToResolveSymbol, TupleKeyedWeakMap } from "./utils";
import { Engine, supports } from "./engines";
import { NilTerminal } from "./node";

const structViewCache = TupleKeyedWeakMap<[SymbolTable, Interface], SymbolTableTypeWrapper>(); // map a SymbolTable -> Interface -> Struct, used to check prior existence of a wrapping of a symbol table into a struct with a possible interface extension

// builtin typedef for `@!interface inject<name extends string, type extends any> = {name: name, type: type}
const builtin_inject = (() => {
    const members = new Map<string, SymTabEntry>();
    const name = "name";
    const type = "type";
    members.set(name, {
        canonicalName: name,
        uiName: name,
        flags: 0,
        declarations: null,
        lexicalType: undefined,
        effectivelyDeclaredType: cfTypeId(name, TypeIndexedAccessType.head),
        symbolId: -1
    });
    members.set(type, {
        canonicalName: type,
        uiName: type,
        flags: 0,
        declarations: null,
        lexicalType: undefined,
        effectivelyDeclaredType: cfTypeId(type, TypeIndexedAccessType.head),
        symbolId: -1
    });

    const params = [
        TypeConstructorParam(name),
        TypeConstructorParam(type)
    ]
    return Interface("", members, params);
})()

// magic builtin string utility types
const builtin_capitalize = (() => {
    return {name: "Capitalize", type: TypeConstructor([], cfTypeId("<<builtin-capitalize>>", TypeIndexedAccessType.head))};
})();
const builtin_uncapitalize = (() => {
    return {name: "Uncapitalize", type: TypeConstructor([], cfTypeId("<<builtin-uncapitalized>>", TypeIndexedAccessType.head))};
})();
const builtin_uppercase = (() => {
    return {name: "Uppercase", type: TypeConstructor([], cfTypeId("<<builtin-uppercase>>", TypeIndexedAccessType.head))};
})();
const builtin_lowercase = (() => {
    return {name: "Lowercase", type: TypeConstructor([], cfTypeId("<<builtin-lowercase>>", TypeIndexedAccessType.head))};
})();

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
    const GENERIC_FUNCTION_INFERENCE = true; // reasonably crash-free, needed for a nice getInstance("") experience
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
    let currentConstraint : Type | null = null;

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
        const savedCurrentConstraint = currentConstraint;

        sourceFile = sourceFile_;
        scanner = sourceFile.scanner;
        diagnostics = sourceFile.diagnostics;
        returnTypes = [];
        flowBecameUnreachable = false;
        warnOnUndefined = false;
        checkerStack = [];
        forcedReturnTypes = new WeakMap();
        currentConstraint = null;

        if (sourceFile.cfFileType === CfFileType.cfc) {
            warnOnUndefined = getAttributeValue(getComponentAttrs(sourceFile) ?? [], "warn-undefined") !== null;

            if (sourceFile.cfc?.extends && sourceFile.cfc?.extends.containedScope.this) {
                sourceFile.containedScope.super = sourceFile.cfc.extends.containedScope.this;
            }

            instantiateInterfaces(sourceFile);

            // in a cfc, need to do in order probably like:
            // 1. properites
            // 2. static blocks
            // 3. functions
            // 4. everything else
            // also this helps with cfc mappers injecting property/function mappings
            // might need to a "pre-pass" that just runs the injection mappings and then does actual checks

            const properties : Property[] = [];
            const functions: Node[] = [];

            const extract = (node: Node | null | undefined) => {
                if (!node) {
                    return undefined;
                }
                else if (node.kind === NodeKind.functionDefinition) {
                    functions.push(node);
                    return undefined; // don't descend but keep visiting
                }
                else if (node.kind === NodeKind.property) {
                    properties.push(node);
                    return undefined; // don't descend but keep visiting
                }
                else {
                    return visit(node, extract); // descend into a block that maybe holds functions or properties that will be hoisted
                }
            }

            visit(sourceFile.content, extract);

            checkList(properties);
            checkList(functions);
            // nodes already checked will have been marked with the checked flag, so they won't get checked twice
            checkList(sourceFile.content);
        }
        else {
            checkList(sourceFile.content);
        }

        sourceFile = savedSourceFile;
        scanner = savedScanner;
        diagnostics = savedDiagnostics;
        returnTypes = savedReturnTypes;
        flowBecameUnreachable = savedFlowBecameUnreachable;
        warnOnUndefined = savedWarnOnUndefined;
        checkerStack = savedCheckerStack;
        forcedReturnTypes = savedForcedReturnTypes;
        currentConstraint = savedCurrentConstraint;
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
        if (!node || (node.flags & NodeFlags.checked)) {
            return;
        }
        checkNodeWorker(node);
        node.flags |= NodeFlags.checked;
    }

    function checkNodeWorker(node: Node) {
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
            case NodeKind.parenthetical:
                // the type of T is the type of (T) is the type of ((T)) ...
                checkNode(node.expr);
                setCachedEvaluatedNodeType(node, getCachedEvaluatedNodeType(node.expr));
                return;
            case NodeKind.hashWrappedExpr: // fallthrough
                checkNode(node.expr);
                const type = getCachedEvaluatedNodeType(node.expr);
                if (CHECK_FLOW_TYPES && !isAssignable(type, BuiltinType.string)) {
                    issueDiagnosticAtNode(node, `Type '${stringifyType(type)}' is not assignable to type 'string'.`);
                }
                return;
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
                setNodeTypeConstraint(node, currentConstraint);
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
                const savedConstraint = currentConstraint;
                currentConstraint = currentConstraint && isStructLike(currentConstraint) ? currentConstraint : null;
                if (currentConstraint) {
                    setNodeTypeConstraint(node, currentConstraint);
                }
                checkStructLiteral(node);
                currentConstraint = savedConstraint;
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
                const propertyMapper = getCfcMapper("properties");
                if (propertyMapper) {
                    tryEvaluatePropertyMapper(propertyMapper, node);

                    function propertyAsTypeForPropertyMappingContext(property: Property) : Interface {
                        const members = new Map<string, SymTabEntry>();

                        members.set("cfname", {
                            canonicalName: "cfname", 
                            uiName: "cfname",
                            flags: 0,
                            declarations: null,
                            lexicalType: undefined,
                            effectivelyDeclaredType: createLiteralType(property.name),
                            symbolId: -1
                        })

                        for (const attr of property.attrs) {
                            // for an attr without a right hand side like
                            // foo=bar baz
                            //         ^^^
                            // we want `baz extend string` to be true
                            const value = getTriviallyComputableString(attr.expr) ?? "";

                            const name = attr.name.token.text;
                            const lcName = name.toLowerCase();

                            members.set(lcName, {
                                canonicalName: lcName,
                                uiName: name,
                                flags: 0,
                                declarations: null,
                                lexicalType: undefined,
                                effectivelyDeclaredType: createLiteralType(value),
                                symbolId: -1
                            })  
                        }
                        return Interface("", members);
                    }

                    function tryEvaluatePropertyMapper(mapper: Type, property: Property) : void {
                        // must be exactly @!typedef properties<T> = (conditional-type)
                        if (
                            mapper.kind !== TypeKind.typeConstructor
                            || mapper.typeParams.length !== 1
                            || mapper.typeParams[0].defaultType
                            || mapper.typeParams[0].extends
                            || mapper.body.kind !== TypeKind.conditional
                        ) {
                            return;
                        }

                        const evalContext = new Map<string, Type>();
                        // @!typedef properties<T> = ... [inject<name, type>] ---> `inject` is provided by compiler in this context
                        evalContext.set("inject", builtin_inject);
                        // @!typedef properties<T> = ... ----> T maps to a compiler provided interface containing property information
                        evalContext.set(mapper.typeParams[0].name,  propertyAsTypeForPropertyMappingContext(property));
                        const resultType = evaluateType(mapper.body, evalContext);
                        runMappedComponentMembersInjectionResult(resultType);
                    }
                }
                // if we're in Wirebox mode, and the property has an inject attribute that we can resolve, add the type to the property value
                // this can be replaced with cfc mappers?
                // we have "inject" which adds symbols to the cfc, we would need maybe an "as" or "mapas" or "mapto" "astype" "totype" or something
                //
                // like mapper<P> = P.inject extends string ? astype<cfc<P.inject>> : 0
                //
                // if (sourceFile.libRefs.has("<<magic/wirebox>>")) {
                //     const nameAttrVal = getAttributeValue(node.attrs, "name");
                //     const nameStringVal = getTriviallyComputableString(nameAttrVal)?.toLowerCase();
                //     const injectAttrVal = getAttributeValue(node.attrs, "inject");
                //     const injectStringVal = getTriviallyComputableString(injectAttrVal)?.toLowerCase();
                //     if (!injectStringVal || !nameStringVal) return;

                //     const mappings = sourceFile.libRefs.get("<<magic/wirebox>>")?.containedScope.typeinfo.mergedInterfaces.get("__INTERNAL__WireboxMappings");
                //     const targetSymbol = sourceFile.containedScope.variables!.get(nameStringVal);
                //     if (!mappings || !targetSymbol || mappings.kind !== TypeKind.interface) return;

                //     const cfcLookup = mappings.members.get(injectStringVal)?.lexicalType;
                //     if (!cfcLookup) return;
                //     const cfc = evaluateType(cfcLookup);

                //     setEffectivelyDeclaredType(targetSymbol, cfc);

                //     if (node.flow) {
                //         // should have an assignment flow on a property
                //         setCachedEvaluatedFlowType(node.flow, targetSymbol.symbolId, cfc);
                //     }
                // }
                return;
            }
            case NodeKind.paramStatement:
                return;
            case NodeKind.staticAccess:
                // lefthand side is a cfc path name
                // righthand side is an expression, presumably a property lookup on <cfc>.static
                // can be like left::r1.r2(ahThisIsLocal).etc[3]
                checkNode(node.right);
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

    /**
     * take the result of an instantiation of `builtin_inject` and update component members accordingly
     */
    function runMappedComponentMembersInjectionResult(injectionResultType: Type) {
        if (injectionResultType.kind === TypeKind.struct) {
            const nameType = injectionResultType.members.get("name")?.effectivelyDeclaredType;
            const type = injectionResultType.members.get("type")?.effectivelyDeclaredType;
            const hasValidName = nameType?.kind === TypeKind.literal && typeof nameType.literalValue === "string";
            const hasValidType = !!type;
            if (hasValidName && hasValidType) {
                const name = nameType.literalValue;
                const lcName = nameType.literalValue.toLowerCase();
                const symbol : SymTabEntry = {
                    canonicalName: lcName,
                    uiName: name,
                    flags: SymbolFlags.synthesizedInjection,
                    declarations: null,
                    lexicalType: undefined,
                    effectivelyDeclaredType: type,
                    symbolId: -1
                };

                sourceFile.containedScope.variables?.set(lcName, symbol);
                sourceFile.containedScope.this?.set(lcName, symbol);
            }
        }
    }

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
                        const mergedType = mergeTypes(l.members.get(key)!.effectivelyDeclaredType ?? BuiltinType.any, r.members.get(key)!.effectivelyDeclaredType ?? BuiltinType.any);
                        // doesn't copy links?
                        mergedMembers.set(key, {
                            canonicalName: key,
                            uiName: l.members.get(key)!.uiName,
                            flags: 0,
                            declarations: [],
                            lexicalType: undefined,
                            effectivelyDeclaredType: mergedType,
                            symbolId: -1,
                        })
                    }
                    else {
                        // only one, so it is optional
                        // cast to Mutable<> borders on dangerous here
                        // we make a shallow copy of the target SymTabEntry, and then a shallow copy of `links`, and then update `optional`
                        // This should not mutate the source object in any way
                        const oneDefinitelyExists = {...(l.members.get(key) || r.members.get(key))!};
                        (oneDefinitelyExists as Mutable<SymTabEntry>).links = {...(oneDefinitelyExists.links || {})};
                        (oneDefinitelyExists as Mutable<SymTabEntry>).links!.optional = true;
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
        const literals = new Set<string | number | boolean>();

        for (const type of types) {
            work(type);
        }

        if (membersBuilder.size === 0) {
            return BuiltinType.any; // maybe never?
        }
        if (membersBuilder.has(BuiltinType.any)) {
            return BuiltinType.any;
        }
        if (membersBuilder.has(BuiltinType.never)) {
            return BuiltinType.never;
        }

        return cfUnion(membersBuilder, flags);

        function work(worktype: Type) {
            if (worktype.flags & TypeFlags.containsUndefined) {
                flags |= TypeFlags.containsUndefined;
            }
            if (worktype.kind === TypeKind.union) {
                for (const type of worktype.types) {
                    work(type);
                }
            }
            else {
                if (worktype.kind === TypeKind.literal) {
                    if (!literals.has(worktype.literalValue)) {
                        literals.add(worktype.literalValue);
                        membersBuilder.add(worktype);
                    }
                }
                else {
                    let thisTypeIsDisjointFromAllTheOthers = true;
                    for (const alreadyHaveThisType of membersBuilder) {
                        if (isLeftSubtypeOfRight(worktype, alreadyHaveThisType, /*sourceIsLiteral*/false, /*forReturn*/false, /*widenLiterals*/false)
                            || isLeftSubtypeOfRight(alreadyHaveThisType, worktype, /*sourceIsLiteral*/false, /*forReturn*/false, /*widenLiterals*/false)
                        ) {
                            membersBuilder.add(mergeTypes(worktype, alreadyHaveThisType));
                            membersBuilder.delete(alreadyHaveThisType);
                            thisTypeIsDisjointFromAllTheOthers = false;
                            break;
                        }
                    }

                    if (thisTypeIsDisjointFromAllTheOthers) {
                        membersBuilder.add(worktype);
                    }
                }
            }
        }
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
    /**
     * if called for types that might perform explicit inference (like "a#infer v#") then the caller must provide a typeParamMap and an inferenceResults out param
     */
    function isLeftSubtypeOfRight(l: Type, r: Type, sourceIsLiteralExpr = false, forReturnType = false, widenLiterals = false, typeParamMap?: ReadonlyMap<string, Type>, inferenceResults?: Map<string, Type>) : boolean {
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
                    const widenedLeft = l.kind === TypeKind.literal ? l.underlyingType : l;
                    const widenedRight = r.kind === TypeKind.literal ? r.underlyingType : r;
                    if (widenedLeft === widenedRight) {
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
                    if (l.kind === TypeKind.literal && r.kind === TypeKind.keyof) {
                        if (!r.concrete) {
                            // we could instantiate it, but we don't have a context?
                            // if it's not concrete it doesn't have any keys in it
                            runningComparisonMap.get(l)!.delete(r);
                            return false;
                        }
                        else if (l.kind !== TypeKind.literal || l.underlyingType !== BuiltinType.string) {
                            runningComparisonMap.get(l)!.delete(r);
                            return false;
                        }
                        else {
                            return r.keyNames.has(l.literalValue as string);
                        }
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

                if (l.kind === TypeKind.literal && (typeof l.literalValue === "string") && r.kind === TypeKind.interpolatedString && r.flags & TypeFlags.inferenceTarget) {
                    // if we're called as inference target we MUST have been passed a typeParamMap and an inferenceResults map
                    return checkAndInferInterpolatedStringTypes(l.literalValue, r, typeParamMap!, inferenceResults!);
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
                            const x = leftVal.effectivelyDeclaredType ?? leftVal.lexicalType;
                            const y = rightVal.effectivelyDeclaredType ?? rightVal.lexicalType;
                            if (!x || !y || !worker(x, y)) {
                                runningComparisonMap.get(l)!.delete(r);
                                return false;
                            }
                        }
                        // check for required properties of R -- left must exist, not be optional, and be a subtype
                        else {
                            if (!leftVal || (leftVal.links?.optional)) {
                                runningComparisonMap.get(l)!.delete(r);
                                return false;
                            }
                            else {
                                const x = leftVal?.effectivelyDeclaredType ?? leftVal.lexicalType;
                                const y = rightVal?.effectivelyDeclaredType ?? rightVal.lexicalType;
                                if (!x || !y || !worker(x,y)) {
                                    runningComparisonMap.get(l)!.delete(r);
                                    return false;
                                }
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
                    if (r === BuiltinType.anyFunction) {
                        return true;
                    }

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
                if (l.kind === TypeKind.keyof) {
                    // this isn't accurate but we don't have much use for it
                    // (keyof T) <: U if every member of T is in U, or if U is string
                    // (keyof T) is a union but we don't really treat it that way
                    if (r !== BuiltinType.string) {
                        runningComparisonMap.get(l)!.delete(r);
                        return false;
                    }
                    return true;
                }

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
                    if (sourceIsLiteralExpr) {
                        for (const rightConstituent of r.types) {
                            if (worker(l, rightConstituent)) return true;
                        }
                    }
                    else {
                        // {a: boolean} <: {a: false} | {a: true} ?
                        const merged = (() => {
                            let merged = r.types[0]; // .length always >= 2 right? otherwise we should have collapsed it to a single non-union type way before here
                            for (let i = 1; i < r.types.length; i++) {
                                merged = mergeTypes(merged, r.types[i]);
                            }
                            return merged;
                        })();
                        widenLiterals = true; // this mutates a function argument, probably optimizes poorly
                        if (worker(l, merged)) {
                            return true;
                        }
                        widenLiterals = false;
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
                    l = evaluateType(l);
                }
                if (r.kind === TypeKind.typeConstructorInvocation) {
                    didReinstantiate = true;
                    r = evaluateType(r);
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

    function checkAndInferInterpolatedStringTypes(target: string, r: cfInterpolatedString, typeParamMap: ReadonlyMap<string, Type>, inferenceResults: Map<string, Type>) : boolean {
        const regexBuilder : string[] = ["^"];
        const matchGroupToInferenceNameMap : string[] = [/*note regexp match groups start at index 1, but we start at 0*/];
        for (const each of r.expr.elements) {
            if (each.kind === NodeKind.textSpan) {
                regexBuilder.push(each.text);
            }
            else {
                const wrappedType = (each.expr as TypeAnnotation).type as cfTypeId;
                const constraintType = wrappedType.next ? evaluateType(wrappedType.next, typeParamMap) : null;
                const inferedAsName = ((each.expr as TypeAnnotation).type as cfTypeId).name;

                if (constraintType) {
                    if (constraintType.kind === TypeKind.literal && typeof constraintType.literalValue === "string") {
                        regexBuilder.push("(" + constraintType.literalValue + ")");
                        matchGroupToInferenceNameMap.push(inferedAsName);
                    }
                    else if (constraintType.kind === TypeKind.union && constraintType.types.every(type => type.kind === TypeKind.literal && typeof type.literalValue === "string")) {
                        const elems = constraintType.types.map(type => {
                            return escapeRegExp((type as cfLiteralType).literalValue as string);
                        });
                        regexBuilder.push("(" + elems.join("|") + ")");
                        matchGroupToInferenceNameMap.push(inferedAsName);
                    }
                    else if (constraintType === BuiltinType.string) {
                        regexBuilder.push(`(.*)`);
                        matchGroupToInferenceNameMap.push(inferedAsName);
                    }
                    else {
                        // constraint `wrappedType.next.name` does not extend string
                        return false;
                    }
                }
                else {
                    regexBuilder.push(`(.*)`);
                    matchGroupToInferenceNameMap.push(inferedAsName);
                }
            }
        }

        regexBuilder.push("$");

        const result = new RegExp(regexBuilder.join("")).exec(target);
        if (!result) {
            return false;
        }
        else {
            for (let i = 0; i < matchGroupToInferenceNameMap.length; ++i) {
                inferenceResults.set(matchGroupToInferenceNameMap[i], createLiteralType(result[i+1]));
            }
            return true;
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
                sig = setEffectivelyDeclaredType(symbol.symTabEntry, evaluateType(symbol.symTabEntry.lexicalType!));
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

        // we can do this no matter the type of receiver; foo(v=42, /*no name*/ 43) is always an error no matter the type of foo (well assuming foo is callable but that's a separate problem)
        const enum CallType { named, positional, incoherent };
        const callType = (() => {
            const namedArgCount = countNamedArgs(node.args);
            if (namedArgCount === 0) {
                return CallType.positional;
            }
            else if (namedArgCount !== node.args.length) {
                issueDiagnosticAtRange(node.left.range, "All arguments must be named, if any are named.");
                return CallType.incoherent;
            }
            else {
                return CallType.named;
            }
        })();

        let returnType : Type;

        if (sig.kind === TypeKind.functionOverloadSet) {
            checkList(node.args);
            const overloads = chooseOverload(sig, node.args);
            if (overloads.length !== 1) {
                setCachedEvaluatedNodeType(node, returnType = BuiltinType.any);
                issueDiagnosticAtRange(getCallExpressionNameErrorRange(node), "No overload matches this call.");
            }
            else {
                setCachedEvaluatedNodeType(node, returnType = evaluateType(overloads[0].returns));
            }
        }
        else if (sig.kind === TypeKind.functionSignature) {
            // jul/25/2022 --- checkList(node.args); do this in checkCallLikeArgs so we can apply constraints with knowledge of param types
            checkCallLikeArguments(sig, node.args, getCallExpressionNameErrorRange(node), /*isNewExpr*/false);
            setCachedEvaluatedNodeType(node, returnType = sig.returns);
        }
        else if (sig.kind === TypeKind.genericFunctionSignature) {
            if (!GENERIC_FUNCTION_INFERENCE || callType === CallType.incoherent) {
                return;
            }

            const typeParamMap = new Map<string, Type | undefined>();
            const definitelyResolvedTypeParamMap = new Map<string, Type>();
            const typeConstraintMap = new Map<string, Type>(); // all constraints are "subtypeof / extends" at this point, e.g. <T extends "a" | "b">(...)
            const pushResolution = (name: string, type: Type) => {
                typeParamMap.set(name, type);
                definitelyResolvedTypeParamMap.set(name, type);
            }

            for (const p of sig.typeParams) {
                typeParamMap.set(p.name, undefined);
                if (p.extends) {
                    const evaluatedConstraint = evaluateType(p.extends);
                    typeConstraintMap.set(p.name, evaluatedConstraint);
                }
            }

            //
            // unify named/positional calls
            //

            let params : cfFunctionSignatureParam[] = [];
            // by name, same order as params, but possibly filtered
            // function foo(a, b, c, d) {} --> foo(d=42, b=42) --> [b,d]
            let args : CallArgument[] = [];

            if (callType === CallType.named) {
                const argNameMap = new Map<string, CallArgument>();
                for (let i = 0; i < node.args.length; ++i) {
                    const name = getTriviallyComputableString(node.args[i].name)?.toLowerCase();
                    if (!name) {
                        // ???
                        continue;
                    }
                    argNameMap.set(name, node.args[i]);
                }
                for (let i = 0; i < sig.params.length; ++i) {
                    const param = sig.params[i];
                    const arg = argNameMap.get(param.canonicalName);
                    if (arg) {
                        params.push(param);
                        args.push(arg)
                    }
                }
            }
            else {
                params = sig.params;
                args = node.args;
            }

            for (let i = 0; i < params.length && i < args.length; i++) {
                const sigParamType = params[i].paramType;
                const callSiteArg = args[i];


                if (sigParamType.kind === TypeKind.functionSignature) {
                    const isGenericInReturnPosition = sigParamType.returns.kind === TypeKind.typeId && typeParamMap.has(sigParamType.returns.name);

                    if (isGenericInReturnPosition && (callSiteArg.expr.kind === NodeKind.functionDefinition || callSiteArg.expr.kind === NodeKind.arrowFunctionDefinition)) {
                        for (let j = 0; j < callSiteArg.expr.params.length; j++) {
                            const resolutions = resolveGenericFunctionTypeParams(
                                typeParamMap,
                                typeConstraintMap,
                                sigParamType.params[j].paramType,
                                evaluateType(typeFromJavaLikeTypename(callSiteArg.expr.params[j].javaLikeTypename)));
                            if (resolutions) {
                                for (const [k,v] of resolutions) {
                                    if (v) {
                                        pushResolution(k,v);
                                    }
                                }
                            }
                        }

                        // like an arguments scope in coldfusion,
                        // we have a map here of (string -> Type),
                        // in the positional arg case, the key is a stringified integer
                        // in the named arg case, the key is a string
                        const effectiveParamTypes : cfFunctionSignatureParam[] = [];

                        // unnamed/positional arguments case
                        for (let j = 0; j < callSiteArg.expr.params.length; j++) {
                            const argScopeSymTabEntry = callSiteArg.expr.containedScope!.arguments!.get(callSiteArg.expr.params[j].canonicalName)!;
                            const type = evaluateType(sigParamType.params[j].paramType, definitelyResolvedTypeParamMap);
                            effectiveParamTypes.push({...sigParamType.params[j], uiName: callSiteArg.expr.params[j].uiName, canonicalName: callSiteArg.expr.params[j].canonicalName});
                            argScopeSymTabEntry.lexicalType = type;
                        }

                        if (callSiteArg.expr.fromTag) {
                            // not possible, can't have a tag expr in an inline function expression, like `needsACallback(<cffunction>....)`
                            // not even with tag-islands does this appear possible, like `needsACallback(```<cffunction name="cb"> ... </cffunction>```)
                        }
                        else {
                            const returnType = checkFunctionBody(
                                callSiteArg.expr,
                                // fixme: we need to send the actual determined types;
                                // have they been checked by virtue of `resolveGenericFunctionTypeParams`?
                                effectiveParamTypes);
                            pushResolution((sigParamType.returns as cfTypeId).name, returnType);
                        }
                    }
                    else {
                        checkNode(callSiteArg.expr);
                        getCachedEvaluatedNodeType(callSiteArg.expr);
                    }
                }
                else {
                    // if param is optional and callSiteArg is undefined we don't need to do this?
                    checkNode(callSiteArg);
                    const argType = getCachedEvaluatedNodeType(callSiteArg)
                    const resolutions = resolveGenericFunctionTypeParams(typeParamMap, typeConstraintMap, sigParamType, argType);
                    if (resolutions) {
                        for (const [k,v] of resolutions) {
                            if (v) {
                                pushResolution(k, v);
                            }
                        }
                    }
                }
            }

            if (definitelyResolvedTypeParamMap.size === typeParamMap.size) {
                returnType = evaluateType(sig.returns, definitelyResolvedTypeParamMap);
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

            /**
             * "target" is expected to eventually be a leaf of a type-id, like "T"
             * prior to walking to the leaf level, it could be some larger type that contains T (or some other named target), like `T[]` or `{a: {b: {c: T}}}`
             */
            function resolveGenericFunctionTypeParams(unifiees: ReadonlyMap<string, Type | undefined>, constraintMap: ReadonlyMap<string, Type>, target: Type, source: Type) : ReadonlyMap<string, Type | undefined> | undefined {
                if (target.kind === TypeKind.typeId) {
                    if (unifiees.has(target.name)) {
                        if (!unifiees.get(target.name)) {
                            if (!constraintMap.has(target.name) || isAssignable(source, constraintMap.get(target.name)!)) {
                                return new Map([[target.name, source]]);
                            }
                            else {
                                return undefined;
                            }
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
                        // these types are closed -- we're not looking up arbitrary generic type IDs, they should have been resolved already during the containing type's instantiation
                        // this would be a weird bug on our part -- target name is "T" but there is unifiee named T?... so in foo<T>(v:T) we're looking for U?
                        return undefined;
                    }
                }
                else if (target.kind === TypeKind.array && source.kind === TypeKind.array) {
                    return resolveGenericFunctionTypeParams(unifiees, constraintMap, target.memberType, source.memberType);
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
                        const targetType = symTabEntry.effectivelyDeclaredType;
                        const sourceType = source.members.get(name)?.effectivelyDeclaredType;
                        if (!sourceType || !targetType) {
                            return;
                        }
                        const result = resolveGenericFunctionTypeParams(freshResolutions, constraintMap, targetType, sourceType);
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
                        const result = resolveGenericFunctionTypeParams(unifiees, constraintMap, targetParamType, sourceParamType);
                        if (result) {
                            for (const[k,v] of result) freshResolutions.set(k,v);
                        }
                    }

                    const returnTypeResolution = resolveGenericFunctionTypeParams(unifiees, constraintMap, target.returns, source.returns);
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

    function setNodeTypeConstraint(node: Node, constraint: Type | null) : void {
        if (!constraint) {
            return;
        }
        sourceFile.nodeTypeConstraints.set(node, constraint);
    }
    // function getTypeConstraint(type: Type) : Type | undefined {
    //     return sourceFile.typeConstraints.get(type);
    // }

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
                            const paramType = paramPair.param.paramType;
                            const savedConstraint = currentConstraint;
                            currentConstraint = paramType;
                            checkNode(arg);
                            currentConstraint = savedConstraint;
                            const argType = getCachedEvaluatedNodeType(arg);
                            if (!isAssignable(argType, paramType, isLiteralExpr(arg.expr))) {
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
                    const paramType = sig.params[i].paramType;

                    const savedConstraint = currentConstraint;
                    currentConstraint = paramType;
                    checkNode(args[i]);
                    currentConstraint = savedConstraint;

                    const argType = getCachedEvaluatedNodeType(args[i]);
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
        const optype = node.optype;
        switch (optype) {
            case BinaryOpType.assign: {
                // an assignment, even fv-unqualified, will always be bound to a scope
                // `x = y` is effectively `variables.x = y`
                if (node.left.kind === NodeKind.identifier || node.left.kind === NodeKind.indexedAccess) {
                    const lhsSymbol = getResolvedSymbol(node.left);
                    
                    if (lhsSymbol) {
                        checkNode(node.right);
                        const rhsType = getCachedEvaluatedNodeType(node.right);

                        const effectivelyDeclaredType = getEffectivelyDeclaredType(lhsSymbol.symTabEntry);
                        if (effectivelyDeclaredType) {
                            if (!isAssignable(rhsType, effectivelyDeclaredType)) {
                                if (CHECK_FLOW_TYPES) {
                                    const r = stringifyType(rhsType);
                                    const l = stringifyType(effectivelyDeclaredType);
                                    issueDiagnosticAtNode(node.right, `Type '${r}' is not assignable to type '${l}'`, DiagnosticKind.error);
                                }
                            }
                        }
                        else if (!effectivelyDeclaredType
                            && (isInCfcPsuedoConstructor(node) || isInEffectiveConstructorMethod(node))
                            && node.left.kind === NodeKind.indexedAccess
                            && node.left.root.kind === NodeKind.identifier
                            && node.left.root.canonicalName === "variables"
                        ) {
                            checkNode(node.right);
                            const rhsType = getCachedEvaluatedNodeType(node.right);
                            const widenedRhsType = recursiveWidenTypeByDiscardingLiteralTypes(rhsType);

                            // fixme: check against a type annotation?

                            setCachedEvaluatedFlowType(node.left.flow!, lhsSymbol.symTabEntry.symbolId, widenedRhsType);
                            setEffectivelyDeclaredType(lhsSymbol.symTabEntry, widenedRhsType);
                        }
                    }
                }

                break;
            }
            case BinaryOpType.assign_cat: {
                checkNode(node.right);
                const rhsType = getCachedEvaluatedNodeType(node.right);
                if (CHECK_FLOW_TYPES && !isAssignable(rhsType, BuiltinType.string)) {
                    issueDiagnosticAtNode(node.right, `Type ${stringifyType(rhsType)} is not assignable to type 'string'.`);
                }
                break;
            }
            case BinaryOpType.contains:
            case BinaryOpType.does_not_contain: {
                checkNode(node.right);
                const rhsType = getCachedEvaluatedNodeType(node.right);
                if (CHECK_FLOW_TYPES && !isAssignable(rhsType, BuiltinType.string)) {
                    issueDiagnosticAtNode(node.right, `Type ${stringifyType(rhsType)} is not assignable to type 'string'.`);
                }
                setCachedEvaluatedNodeType(node, BuiltinType.boolean);
                break;
            }
            case BinaryOpType.cat: {
                checkNode(node.right);
                const rhsType = getCachedEvaluatedNodeType(node.right);
                if (CHECK_FLOW_TYPES && !isAssignable(rhsType, BuiltinType.string)) {
                    issueDiagnosticAtNode(node.right, `Type ${stringifyType(rhsType)} is not assignable to type 'string'.`);
                }
                setCachedEvaluatedNodeType(node, BuiltinType.string);
                break;
            }
            case BinaryOpType.eq:
            case BinaryOpType.neq:
            case BinaryOpType.strict_eq:
            case BinaryOpType.strict_neq: {
                checkNode(node.right);
                const lhsType = getCachedEvaluatedNodeType(node.left);
                const rhsType = getCachedEvaluatedNodeType(node.right);
                // "isEqualityComparable" we are assuming is the same as assignability; i.e. if `a = b` is sound, then `a == b` or `a === b` is sound
                if (CHECK_FLOW_TYPES && !isAssignable(rhsType, lhsType)) {
                    issueDiagnosticAtNode(node.right, `Type '${stringifyType(rhsType)}' is not comparable to type '${stringifyType(lhsType)}'.`);
                }
                setCachedEvaluatedNodeType(node, BuiltinType.boolean);
                break;
            }
            case BinaryOpType.equivalent:
            case BinaryOpType.implies:
            case BinaryOpType.and:
            case BinaryOpType.or:
            case BinaryOpType.xor: {
                checkNode(node.right);
                const rhsType = getCachedEvaluatedNodeType(node.right);
                if (CHECK_FLOW_TYPES && !isAssignable(rhsType, BuiltinType.boolean)) {
                    issueDiagnosticAtNode(node.right, `Type '${stringifyType(rhsType)}' is not assignable to type 'boolean'.`);
                }
                setCachedEvaluatedNodeType(node, BuiltinType.boolean);
                break;
            }
            case BinaryOpType.assign_add:
            case BinaryOpType.assign_sub:
            case BinaryOpType.assign_mul:
            case BinaryOpType.assign_div:
            case BinaryOpType.assign_mod: {
                checkNode(node.right);
                const rhsType = getCachedEvaluatedNodeType(node.right);
                if (CHECK_FLOW_TYPES && !isAssignable(rhsType, BuiltinType.numeric)) {
                    issueDiagnosticAtNode(node.right, `Type '${stringifyType(rhsType)}' is not assignable to type 'numeric'.`);
                }
                // no node type
                break;
            }
            case BinaryOpType.add:
            case BinaryOpType.sub:
            case BinaryOpType.mul:
            case BinaryOpType.div:
            case BinaryOpType.quotient:
            case BinaryOpType.mod:
            case BinaryOpType.exp: {
                checkNode(node.right);

                if (CHECK_FLOW_TYPES) {
                    const lhsType = getCachedEvaluatedNodeType(node.left);
                    const rhsType = getCachedEvaluatedNodeType(node.right);
                    if (!isAssignable(lhsType, BuiltinType.numeric)) {
                        issueDiagnosticAtNode(node.left, `Type '${stringifyType(lhsType)}' is not assignable to type 'numeric'.`);
                    }
                    if (!isAssignable(rhsType, BuiltinType.numeric)) {
                        issueDiagnosticAtNode(node.right, `Type '${stringifyType(rhsType)}' is not assignable to type 'numeric'.`);
                    }
                }
                setCachedEvaluatedNodeType(node, BuiltinType.numeric);
                break;
            }
            case BinaryOpType.lt:
            case BinaryOpType.lte:
            case BinaryOpType.gt:
            case BinaryOpType.gte: {
                checkNode(node.right);

                if (CHECK_FLOW_TYPES) {
                    const lhsType = getCachedEvaluatedNodeType(node.left);
                    const rhsType = getCachedEvaluatedNodeType(node.right);
                    if (!isAssignable(lhsType, BuiltinType.numeric)) {
                        issueDiagnosticAtNode(node.left, `Type '${stringifyType(lhsType)}' is not assignable to type 'numeric'.`);
                    }
                    if (!isAssignable(rhsType, BuiltinType.numeric)) {
                        issueDiagnosticAtNode(node.right, `Type '${stringifyType(rhsType)}' is not assignable to type 'numeric'.`);
                    }
                }
                setCachedEvaluatedNodeType(node, BuiltinType.boolean);
                break;
            }
            case BinaryOpType.nullCoalesce: {
                checkNode(node.right);
                // node type union of left/right?
                break;
            }
            default: {
                exhaustiveCaseGuard(optype);
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
            const name = sourceFile.symbolIdToSymbol.get(symbolId)?.uiName || "<<no-symbol>>";
            debug.out(`Expected a flow when setting flowtype for '${name}'; hasFlow=${hasFlow}, hasFlowID='${hasFlowId}'`)
            return;
        }

        if (!sourceFile.cachedFlowTypes.has(flow.flowId)) {
            sourceFile.cachedFlowTypes.set(flow.flowId, new Map());
        }
        sourceFile.cachedFlowTypes.get(flow.flowId)!.set(symbolId, type);
    }

    function getCachedEvaluatedFlowType(flow: Flow, symbolId: SymbolId) : Type | undefined {
        return sourceFile.cachedFlowTypes.get(flow.flowId)?.get(symbolId);
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
            if (
                ((node.typeAnnotation?.shimKind === TypeShimKind.annotation && node.typeAnnotation.type.kind === TypeKind.functionSignature)
                    || (node.typeAnnotation?.shimKind === TypeShimKind.nonCompositeFunctionTypeAnnotation))
                && (node.expr.right.kind === NodeKind.functionDefinition || node.expr.right.kind === NodeKind.arrowFunctionDefinition)
            ) {
                checkFunctionDefinition(node.expr.right, node.typeAnnotation);
                didConsumeTypeAnnotation = true;

                rhsExpr = node.expr.right;
                rhsType = getCachedEvaluatedNodeType(node.expr.right);
            }
            else {
                // if there is a type annotation, it might be for some literal type, and we need to check assignability
                // this pattern might be used to support an annotation like "@!const", too
                const shouldWidenLiteralTypes = node.typeAnnotation?.shimKind === TypeShimKind.annotation ? false : true;
                const savedCurrentConstraint = currentConstraint;
                currentConstraint = node.typeAnnotation?.shimKind === TypeShimKind.annotation ? evaluateType(node.typeAnnotation.type) : null;
                checkNode(node.expr.right);
                currentConstraint = savedCurrentConstraint;

                const baseRhsType = getCachedEvaluatedNodeType(node.expr.right);
                rhsExpr = node.expr.right;
                rhsType = shouldWidenLiteralTypes ? recursiveWidenTypeByDiscardingLiteralTypes(baseRhsType) : baseRhsType;
            }

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
                    effectivelyDeclaredType = evaluateType(node.typeAnnotation.type);
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
        return symbol.effectivelyDeclaredType = type;
    }

    function getEffectivelyDeclaredType(symbol: SymTabEntry) : Type | undefined {
        return symbol.effectivelyDeclaredType;
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
                    issueDiagnosticAtNode(node, `Cannot find name '${node.uiName ?? node.canonicalName ?? "<<error-name>>"}'.`, DiagnosticKind.warning);
                }
                return;
            }

            const FIXME_CURRENT_CONTAINER = findAncestor(node, (node) => !!node.containedScope); // should be a "currentContainer" member var
            const isOuterVar = resolvedSymbol.container !== FIXME_CURRENT_CONTAINER;
            const flowType = isOuterVar ? undefined
                : node.flow ? determineFlowType(node.flow, resolvedSymbol.symTabEntry.symbolId)
                : undefined;

            const maybeMemberFunctionDefinition = tryGetCfcMemberFunctionDefinition(resolvedSymbol?.symTabEntry);

            // it is declared in the same container, but there is no apparent flowtype yet;
            // implying that it is used before assignment
            // except in the case of member functions, which are always visible
            // maybe we could hoist just member functions so they are always first in flow
            if (!(resolvedSymbol.symTabEntry.flags & SymbolFlags.synthesizedInjection)
                && !isOuterVar && !flowType && !maybeMemberFunctionDefinition
            ) {
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
                    if (getUserSpecifiedReturnTypeType(decl).none) {
                        if (checkerStackContains(decl)) {
                            const name = decl.name?.ui;
                            const errorNode = getFunctionDefinitionNameTerminalErrorNode(decl);

                            if (CHECK_RETURN_TYPES) {
                                issueDiagnosticAtNode(errorNode, `Function '${name}' requires an explicit return type because it directly or indirectly references itself.`);
                            }

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
                : flowType ? evaluateType(flowType) // if there's a flowtype, use that
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

                if (element.accessType === IndexedAccessType.bracket) {
                    checkNode(element.expr);
                }

                if (element.accessType === IndexedAccessType.bracket && type.kind === TypeKind.array) {
                    symbol = undefined;
                    type = type.memberType;
                }
                else if (element.accessType === IndexedAccessType.dot || element.accessType === IndexedAccessType.bracket) {
                    // in bracket access, if expr is not a string literal, it might have a string literal type, we should check for that
                    const propertyName = element.accessType === IndexedAccessType.dot
                        ? element.property.token.text.toLowerCase()
                        : element.expr.kind === NodeKind.simpleStringLiteral
                        ? getTriviallyComputableString(element.expr)
                        : undefined;

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
                            type = evaluateType(symbol.symTabEntry.lexicalType!);
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

    /**
     * Mutates the provided signature, and may return a fresh signature generated from the mutated signature
     * Caller should rebind the input to the output, i.e. use this as `input = mutateCfFunctionSignatureFromNonCompositeAnnotation(x, input)`
     */
    function mutateCfFunctionSignatureFromNonCompositeAnnotation(annotation: NonCompositeFunctionTypeAnnotation, signature: Mutable<cfFunctionSignature>) : cfFunctionSignature | cfGenericFunctionSignature {
        if (annotation.typeparams) {
            return generic(annotation.params, annotation.typeparams);
        }
        else {
            return nonGeneric();
        }

        function generic(paramAnnotations: NamedAnnotation[], typeparams: NamedAnnotation[]) {
            const mappedParamAnnotations = (() => {
                const result = new Map<string, NamedAnnotation>();
                for (const paramAnnotation of paramAnnotations) {
                    result.set(paramAnnotation.name.text.toLowerCase(), paramAnnotation);
                }
                return result;
            })()

            for (const param of signature.params) {
                const annotation = mappedParamAnnotations.get(param.canonicalName);
                if (annotation) {
                    (param as Mutable<cfFunctionSignatureParam>).paramType = annotation.type;
                    mappedParamAnnotations.delete(param.canonicalName);
                }
            }

            for (const unmatchedAnnotation of mappedParamAnnotations.values()) {
                issueDiagnosticAtRange(
                    unmatchedAnnotation.name.range,
                    `Annotation for parameter ${unmatchedAnnotation.name.text} does not match any actual parameter name, and will be discarded.`,
                    DiagnosticKind.warning);
            }

            if (annotation.returns) {
                signature.returns = annotation.returns;
            }

            return cfGenericFunctionSignature(
                signature.uiName,
                typeparams.map((typeparam) => TypeConstructorParam(typeparam.name.text, /* defaultType */ undefined, /* extends */ typeparam.type ?? undefined)),
                signature.params,
                annotation.returns ?? BuiltinType.any,
                signature.attrs
            );
        }

        function nonGeneric() {
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

                const instantiatedAnnotatedParamType = evaluateType(paramAnnotation.type, new Map(), false, false);
                if (!instantiatedAnnotatedParamType) {
                    issueDiagnosticAtRange(paramAnnotation.name.range, "Type failed to instantiate and will be treated as 'any'.", DiagnosticKind.warning);
                }
                else {
                    (sigParamUpdateTarget as Mutable<cfFunctionSignatureParam>).paramType = instantiatedAnnotatedParamType;
                }
            }

            if (annotation.returns) {
                signature.returns = annotation.returns;
            }

            return signature;
        }
    }

    function getCfcMapper(name: string) : Type | undefined {
        const mapper = sourceFile
            .containedScope.typeinfo.namespaces.get("cf_CfcTransform")
            ?.containedScope.typeinfo.aliases.get(name);
        return mapper ? evaluateType(mapper) : undefined;
    }

    function __experimental__runCompletionsHook(incomingSourceFile: SourceFile, userText: string) : string[] {
        const savedSourceFile = sourceFile;
        sourceFile = incomingSourceFile;

        const completionsHook = sourceFile.containedScope.typeinfo.namespaces.get("cf_CfcTransform")?.containedScope.typeinfo.aliases.get("completions_hook");
        if (!completionsHook) {
            return [];
        }

        const properties : Property[] = [];
        const functions : FunctionDefinition[] = [];

        const extract = (node: Node | null | undefined) => {
            if (!node) {
                return undefined;
            }
            else if (node.kind === NodeKind.functionDefinition) {
                functions.push(node);
                return undefined; // don't descend but keep visiting
            }
            else if (node.kind === NodeKind.property) {
                properties.push(node);
                return undefined; // don't descend but keep visiting
            }
            else {
                return visit(node, extract); // descend into a block that maybe holds functions or properties that will be hoisted
            }
        }

        visit(sourceFile.content, extract);

        // @!typedef completions_hook<UserText, Fs, Ps> = ...
        const evalContext = new Map<string, Type>();

        evalContext.set("UserText", createLiteralType(userText));
        // Ps : {cfname: <attrname>, ...otherattrs} | ...
        evalContext.set(
            "Ps",
            cfUnion(
                new Set(
                    properties.map((p) => {
                        const m = new Map<string, SymTabEntry>();
                        m.set("cfname", {
                            canonicalName: p.name,
                            uiName: p.name,
                            symbolId: -1,
                            declarations: null,
                            flags: 0,
                            lexicalType: undefined,
                            effectivelyDeclaredType: createLiteralType(p.name),
                        });
                        return Struct(m);
                    })
                )
            )
        );

        let result : string[];

        if (completionsHook.kind !== TypeKind.typeConstructor) {
            result = [];
        }
        else {
            const hookResult = evaluateType(completionsHook.body, evalContext);
            if (hookResult.kind === TypeKind.literal && typeof hookResult.literalValue === "string") {
                result = [hookResult.literalValue];
            }
            else if (hookResult.kind === TypeKind.union && hookResult.types.every(type => type.kind === TypeKind.literal && typeof type.literalValue === "string")) {
                result = hookResult.types.map(type => (type as cfLiteralType).literalValue as string);
            }
            else {
                result = []
            }
        }

        sourceFile = savedSourceFile;

        return result;
    }

    function checkFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition, typeAnnotation = node.typeAnnotation) {
        if (node.flags & NodeFlags.checked) {
            return;
        }

        const isMemberFunction = isCfcMemberFunctionDefinition(node);

        if (isMemberFunction) {
            const functionMapper = getCfcMapper("functions");

            if (functionMapper && node.kind === NodeKind.functionDefinition) {
                tryEvaluateFunctionMapper(functionMapper, node);

                function functionAsTypeForFunctionMappingContext(f: FunctionDefinition) {
                    const members = new Map<string, SymTabEntry>();
                    members.set("cfname", {
                        canonicalName: "cfname",
                        uiName: "cfname",
                        flags: 0,
                        declarations: null,
                        lexicalType: undefined,
                        effectivelyDeclaredType: createLiteralType(f.name?.ui ?? "<<missing-name>>"),
                        symbolId: -1
                    })

                    for (const attr of f.attrs) {
                        // for an attr without a right hand side like
                        // foo=bar baz
                        //         ^^^
                        // we want `baz extend string` to be true
                        const value = getTriviallyComputableString(attr.expr) ?? "";

                        const name = attr.name.token.text;

                        members.set(name, { // case-sensitive match in mapper, so F.fooBar extends string is true if the attr name is exactly "fooBar"
                            canonicalName: name,
                            uiName: name,
                            flags: 0,
                            declarations: null,
                            lexicalType: undefined,
                            effectivelyDeclaredType: createLiteralType(value),
                            symbolId: -1
                        })
                    }

                    members.set("cfargs", {
                        canonicalName: "cfargs",
                        uiName: "cfargs",
                        flags: 0,
                        declarations: null,
                        lexicalType: undefined,
                        effectivelyDeclaredType: cfTuple(f.params.map((v) : SymTabEntry => {
                            return {
                                canonicalName: v.canonicalName,
                                uiName: v.uiName,
                                flags: 0,
                                declarations: null,
                                lexicalType: undefined,
                                effectivelyDeclaredType: BuiltinType.any, // need to pull type info
                                symbolId: -1,
                                links: {
                                    optional: !v.required
                                }
                            }
                        })),
                        symbolId: -1
                    });

                    members.set("cfreturns", {
                        canonicalName: "cfreturns",
                        uiName: "cfreturns",
                        flags: 0,
                        declarations: null,
                        lexicalType: undefined,
                        effectivelyDeclaredType: BuiltinType.any, // probably have to require function has explicit return type, or we fallback to any
                        symbolId: -1
                    });

                    return Interface("", members);
                }

                function tryEvaluateFunctionMapper(mapper: Type, f: FunctionDefinition) {
                    // must be exactly @!typedef function<F> = (conditional-type)
                    if (
                        mapper.kind !== TypeKind.typeConstructor
                        || mapper.typeParams.length !== 1
                        || mapper.typeParams[0].defaultType
                        || mapper.typeParams[0].extends
                        || mapper.body.kind !== TypeKind.conditional
                    ) {
                        return;
                    }

                    const evalContext = new Map<string, Type>();
                    // @!typedef functions<F> = ... [inject<name, type>] ---> `inject` is provided by compiler in this context
                    evalContext.set("inject", builtin_inject);
                    // @!typedef functions<F> = ... ----> F maps to a compiler provided interface containing property information
                    evalContext.set(mapper.typeParams[0].name,  functionAsTypeForFunctionMappingContext(f));
                    const resultType = evaluateType(mapper.body, evalContext);
                    runMappedComponentMembersInjectionResult(resultType);
                }
            }
        }

        // for cfc member functions, some work was already done in the binder to extract the signature, but we didn't have visibility into CFC resolution there;
        // so here we can try to resolve CFC return types / param types
        // fixme -- binder doesn't need to do that? ...

        let memberFunctionSignature : cfFunctionSignature | undefined;

        let symbol = isNamedFunction(node)
            ? walkupScopesToResolveSymbol(sourceFile, node.name.canonical)
            : undefined;

        let POP_CHECKER_STACK = false; // easy to forget, make sure to pop if necessary

        if (isMemberFunction) {
            checkerStack.push(node);
            POP_CHECKER_STACK = true;

            if (node.kind === NodeKind.functionDefinition && node.name.canonical) {
                if (symbol) {
                    setResolvedSymbol(node, symbol);
                    sourceFile.containedScope.variables?.set(node.name.canonical, symbol.symTabEntry);
                    sourceFile.containedScope.this?.set(node.name.canonical, symbol.symTabEntry);
                }
                // if (symbol?.symTabEntry.firstLexicalType && symbol.symTabEntry.firstLexicalType.kind === TypeKind.functionSignature) {
                //     const freshType = evaluateType(symbol.container, symbol.symTabEntry.firstLexicalType); // consider a type annotation?
                //     if (freshType.kind === TypeKind.functionSignature) {
                //         memberFunctionSignature = freshType;
                //         const variablesSymbol = sourceFile.containedScope.variables?.get(node.name.canonical);
                //         // keep both `variables` and `this` in sync with member functions
                //         if (variablesSymbol) {
                //             variablesSymbol.firstLexicalType = freshType;
                //             sourceFile.containedScope.variables?.set(node.name.canonical, variablesSymbol);
                //             sourceFile.containedScope.this?.set(node.name.canonical, variablesSymbol);
                //         }
                //     }
                // }
            }
        }

        const cfSyntaxDirectedTypeSig = memberFunctionSignature ?? (evaluateType(extractCfFunctionSignature(node)) as cfFunctionSignature);
        let finalType : cfFunctionSignature | cfGenericFunctionSignature | cfFunctionOverloadSet = cfSyntaxDirectedTypeSig;
        
        if (typeAnnotation?.shimKind === TypeShimKind.annotation) {
            const evaluatedSignature = evaluateType(typeAnnotation.type);
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
                    sourceFile.containedScope.variables!.get(node.name.canonical)!.lexicalType = finalType; // updates the 'this' copy of the symbol too, the refs are the same
                }
            }
            else if (evaluatedSignature.kind === TypeKind.genericFunctionSignature) {
                // to see if we can get interesting behavior, we do not do any checking here right now
                finalType = evaluatedSignature;
                if (isMemberFunction && sourceFile.containedScope.variables!.has(node.name.canonical)) {
                    sourceFile.containedScope.variables!.get(node.name.canonical)!.lexicalType = finalType; // updates the 'this' copy of the symbol too, the refs are the same
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
                    sourceFile.containedScope.variables!.get(node.name.canonical)!.lexicalType = finalType; // updates the 'this' copy of the symbol too, the refs are the same
                }
            }
            else if (evaluatedSignature !== BuiltinType.any) {
                issueDiagnosticAtNode(node, `Expected a function signature as an annotated type, but got type '${stringifyType(evaluatedSignature)}'.`)
            }
        }
        else if (typeAnnotation?.shimKind === TypeShimKind.nonCompositeFunctionTypeAnnotation) {
            finalType = mutateCfFunctionSignatureFromNonCompositeAnnotation(typeAnnotation, finalType);
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
        // What if it is a generic? For generics, we don't check it, we just say "yeah, it is what it says it is"
        const inferredReturnType = finalType.kind === TypeKind.functionSignature ? (() => {
            for (const param of finalType.params) {
                const argSymbol = node.containedScope!.arguments!.get(param.canonicalName);
                if (!argSymbol) { // weird error case?:  we have a param but did not set it in arguments scope?
                    continue;
                }
                argSymbol.lexicalType = param.paramType;
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
                    
                    if (CHECK_FLOW_TYPES) {
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
            }
            else {
                //
                // in `/*@!type () => {x:number}*/ [struct] function foo() { ... }`
                // we want to use the annotated return type, since the user went through the trouble of spelling it, it is probably as precise as necessary
                // in `struct function foo() { ... }`
                // we want to use the inferred return type, IF IT TYPECHECKED as a subtype of the cf type, since "struct" is not very informative, and the user
                // probably wants the more specific type, but with legacy cf function return type runtime type checking
                //
                const {hasAnnotatedReturnType} = getUserSpecifiedReturnTypeType(node);
                if (!hasAnnotatedReturnType) {
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

            setEffectivelyDeclaredType(symbol.symTabEntry, finalType);

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
    //
    // we have "params" which has type info, and also node.containedScope.arguments which has type info
    // why do we need both?
    //
    // we set the flow type of the arguments on the startflow of the function to be the params[] types, can we just use node.containedScope.arguments types?
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
            return taglib.members.get(tagName)?.lexicalType;
        }
        else {
            return undefined;
        }
    }

    function discardDotPrefixAccesses(typeId: cfTypeId) : cfTypeId | undefined {
        let working : cfTypeId | undefined = typeId.next;
        while (working) {
            if (working.accessType === TypeIndexedAccessType.bracket_string || working.accessType === TypeIndexedAccessType.bracket_id) {
                return working;
            }
            else {
                working = working.next;
            }
        }
        return undefined;
    }

    /**
     * Given a cfTypeId representing a bracket access (a.b["c"]["d"]), index into some type
     * A cfTypeId that is a bracket access should contain only `next` descendants of bracket access type (i.e. a.b.["c"].d is not valid)
     */
    function bracketAccessIntoType(typeId: cfTypeId | undefined, indexable: Type, typeParamMap: ReadonlyMap<string, Type>) : Type | undefined {
        if (!typeId) {
            return indexable;
        }

        let working_type : Type | undefined = indexable;
        let working_name : cfTypeId | undefined = typeId;

        // index into interface/structs
        while (working_type && working_name) {
            if (working_type.kind === TypeKind.union) {
                const types : Type[] = (() => {
                    const result : Type[] = [];
                    for (const type of working_type.types) {
                        const constituentType = bracketAccessIntoType(working_name, type, typeParamMap);
                        if (constituentType) {
                            result.push(constituentType);
                        }
                    }
                    return result;
                })();
                return unionify(types);
            }

            if (!isStructLike(working_type)) {
                return undefined;
            }

            if (working_name.accessType === TypeIndexedAccessType.bracket_string) {
                working_type = working_type.members.get(working_name.name)?.effectivelyDeclaredType;
            }
            else if (working_name.accessType === TypeIndexedAccessType.bracket_id) {
                const {next, ...sansNext} = working_name;
                // it can be generic but it must evaluate to a string
                const evaluatedName = evaluateType(sansNext, typeParamMap);
                if (evaluatedName.kind !== TypeKind.literal || typeof evaluatedName.literalValue !== "string") {
                    return undefined;
                }
                working_type = working_type.members.get(evaluatedName.literalValue)?.effectivelyDeclaredType;
            }


            if (!working_type) {
                return undefined;
            }

            working_name = working_name.next;
        }

        return working_type;
    }

    /**
     * we walkup into parent components to resolve their types, too
     * which almost certainly not The Right Thing, but was easier to get running than an import syntax
     * eventually we'll need to import types
     */
    function resolveTypenameFromTypeParamMapOrContainerWalk(context: Node | undefined, typeParamMap: ReadonlyMap<string, Type> | undefined, typeId: cfTypeId) : Type | undefined {
        // first globally available builtins
        // we should eventually issue diagnostics on users defining these types, since we will always match these
        // also, indexing into a builtin like Uppercase.Foo.Bar is meaningless, so a diagnostic on that would be nice
        switch (typeId.name) {
            case builtin_capitalize.name: return builtin_capitalize.type;
            case builtin_uncapitalize.name: return builtin_uncapitalize.type;
            case builtin_uppercase.name: return builtin_uppercase.type;
            case builtin_lowercase.name: return builtin_lowercase.type;
        }

        const maybeInTypeParamMap = typeParamMap?.get(typeId.name);
        if (maybeInTypeParamMap) {
            return maybeInTypeParamMap;
        }
        else if (!context) {
            return walkUpContainersToResolveFirstType(sourceFile, typeId);
        }
        else {
            return walkUpContainersToResolveFirstType(context, typeId);
        }
    }

    /**
     * first type is `A` in `Foo.A["B"]` with context `@!namespace Foo { @!interface A { B: string } }`
     * deep resolve finds "B" in A["B"]
     * there are probably issues here with recursively dealing with unions?
     */
    function deepResolveTypenameFromTypeParamMapOrContainerWalk(context: Node | undefined, typeParamMap: ReadonlyMap<string, Type> | undefined, typeId: cfTypeId) : Type | undefined {
        const firstType = resolveTypenameFromTypeParamMapOrContainerWalk(context, typeParamMap, typeId);
        if (!firstType) {
            return undefined;
        }

        const remainingIndexAccesses = discardDotPrefixAccesses(typeId);

        if (!remainingIndexAccesses) {
            return firstType;
        }

        return bracketAccessIntoType(remainingIndexAccesses, firstType, typeParamMap ?? new Map());
    }

    function walkUpContainersToResolveFirstType(context: Node, type: cfTypeId) : Type | undefined {
        let node : Node | null = context;
        const typeName = type.name;

        while (node) {
            if (node.containedScope) {
                if (node.containedScope.typeinfo) {
                    if (node.containedScope.typeinfo.aliases.has(typeName)) {
                        return node.containedScope.typeinfo.aliases.get(typeName)!;
                    }
                    if (node.containedScope.typeinfo.mergedInterfaces.has(typeName)) {
                        return node.containedScope.typeinfo.mergedInterfaces.get(typeName)!;
                    }
                    if (node.containedScope.typeinfo.namespaces.has(typeName)) {
                        return descendNamespaceLikeContextToFindFirstType(node, type);
                    }
                }

                for (const importDef of sourceFile.containedScope.typeinfo.imports.values()) { // context not source file
                    // we only support qualified right now
                    if (importDef.qualifiedName !== type.name) {
                        continue;
                    }

                    const maybeImport = cfcResolver(ProjectRelativeImportLookup(importDef.path));

                    if (maybeImport && maybeImport.sourceFile.cfFileType === CfFileType.dCfm) {
                        const nameWithoutImportQualifier = type.next;
                        const result = descendNamespaceLikeContextToFindFirstType(maybeImport.sourceFile, nameWithoutImportQualifier);
                        if (result) {
                            return result;
                        }
                    }
                }

                if (node.kind === NodeKind.sourceFile) {
                    for (const lib of node.libRefs.values()) {
                        // this uh isn't related to libRefs?...
                        if (node.containedScope.typeinfo.aliases.has(typeName)) {
                            return node.containedScope.typeinfo.aliases.get(typeName)!;
                        }
                        if (lib.containedScope.typeinfo.mergedInterfaces.has(type.name)) {
                            return lib.containedScope.typeinfo.mergedInterfaces.get(type.name)!;
                        }
                    }

                    // walk up into parent component
                    if (node.cfc?.extends) {
                        node = node.cfc.extends;
                    }
                    else {
                        // no parent component, resolve from lib
                        // (didn't we just do this in the `for` block above?)
                        return libTypeResolver(type.name);
                    }
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

        function descendNamespaceLikeContextToFindFirstType(context: Node, pathElements: cfTypeId | undefined) : Type | undefined {
            let working_context : Node | undefined = context;
            let working_pathname : cfTypeId | undefined = pathElements;

            if (!working_context || !working_pathname) {
                return undefined;
            }
            
            while (working_context && working_pathname) {
                // annotation required here
                // https://github.com/microsoft/TypeScript/issues/48708
                const maybeNamespace : Node | undefined = working_context.containedScope?.typeinfo.namespaces.get(working_pathname.name);
                if (maybeNamespace) {
                    working_context = maybeNamespace;
                    working_pathname = working_pathname?.next;
                    continue;
                }

                // fixme --- need to "merge interfaces across namespaces", or not support merged interfaces,
                // or otherwise handle the array of interfaces here
                const maybeInterface = working_context.containedScope?.typeinfo.interfaces.get(working_pathname.name)?.[0];
                if (maybeInterface) {
                    return maybeInterface;
                }

                const maybeAlias = working_context.containedScope?.typeinfo.aliases.get(working_pathname.name);
                if (maybeAlias) {
                    return maybeAlias;
                }

                break;
            }

            return undefined;
        }
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
                        flags: 0,
                        canonicalName,
                        declarations: [member],
                        lexicalType: undefined,
                        effectivelyDeclaredType: memberType,
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
                    let key : string | null = null;
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
                        key = getTriviallyComputableString(node.key) ?? null;
                    }
                    const savedCurrentConstraint = currentConstraint;
                    currentConstraint = currentConstraint && isStructLike(currentConstraint) && key
                        ? currentConstraint.members.get(key)?.effectivelyDeclaredType ?? null
                        : null;
                    checkNode(node.expr);
                    setCachedEvaluatedNodeType(node, getCachedEvaluatedNodeType(node.expr));
                    setNodeTypeConstraint(node.expr, currentConstraint)
                    currentConstraint = savedCurrentConstraint;
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
        const savedConstraint = currentConstraint;
        currentConstraint = currentConstraint?.kind === TypeKind.array ? currentConstraint.memberType : null;
        checkList(node.members);
        const arrayMemberTypes = node.members.map((member) => getCachedEvaluatedNodeType(member));
        const membersAsUnion = unionify(arrayMemberTypes);
        const uninstantiatedArray = UninstantiatedArray(membersAsUnion);
        const instantiatedArrayType = evaluateType(uninstantiatedArray);
        setCachedEvaluatedNodeType(node, instantiatedArrayType);
        currentConstraint = savedConstraint;
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

        if (!cfcThis.members) {
            // bug from bailing prior to fully binding/checking and there is no `containedScope.*` ???
            // debugger;
            console.log(`internal compiler error -- expected members property on cfc with path ${cfcName}`);
            return;
        }

        const initSig = cfcThis.members.get("init"); // @@@@@@@@@@@@@@@@ .get "reading property of undefined"
        if (initSig && initSig.lexicalType?.kind === TypeKind.functionSignature) {
            setCachedEvaluatedNodeType(node.callExpr.left, initSig.lexicalType);
            checkCallLikeArguments(initSig.lexicalType, node.callExpr.args, node.callExpr.left.range, /*isNewExpr*/ true);
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
        else if (!member && type.kind === TypeKind.cfc) {
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
                    cfcResolver = (args) => {
                        const result = installables[key]!(args);
                        if (result) {
                            sourceFile.directDependencies.add(result.sourceFile);
                        }
                        return result;
                    };
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

    /**
     * a type like {x: (0|1)[]} becomes {x: numeric[]}
     */
    function recursiveWidenTypeByDiscardingLiteralTypes(type: Type) : Type {
        switch (type.kind) {
            case TypeKind.literal: {
                return type.underlyingType;
            }
            case TypeKind.array: {
                const widenedMemberType = recursiveWidenTypeByDiscardingLiteralTypes(type.memberType);
                if (widenedMemberType === type.memberType) {
                    return type;
                }
                else {
                    return createType({...type, memberType: widenedMemberType});
                }
            }
            case TypeKind.struct: {
                const widenedMembers = new Map<string, SymTabEntry>();
                for (const [name,symtabEntry] of type.members) {
                    widenedMembers.set(name, {
                        ...symtabEntry,
                        effectivelyDeclaredType: recursiveWidenTypeByDiscardingLiteralTypes(
                            symtabEntry.effectivelyDeclaredType ?? symtabEntry.lexicalType ?? BuiltinType.any)
                    });
                }
                return Struct(widenedMembers);
            }
            case TypeKind.union: {
                const widenedTypes = type.types.map(recursiveWidenTypeByDiscardingLiteralTypes);
                return unionify(widenedTypes);
            }
            default: {
                return type;
            }
        }
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
            type: Type | null,
            typeParamMap?: ReadonlyMap<string, Type>,
            partiallyApplyGenericFunctionSigs?: boolean,
            initialFallbackToAny?: true
        ) : Type;
        function evaluateType(
            type: Type | null,
            typeParamMap?: ReadonlyMap<string, Type>,
            partiallyApplyGenericFunctionSigs?: boolean,
            initialFallbackToAny?: false
        ) : Type | null;
        function evaluateType(
            type: Type | null,
            typeParamMap: ReadonlyMap<string, Type> = type?.capturedContext ?? new Map(),
            partiallyApplyGenericFunctionSigs = false,
            initialFallbackToAny = true
        ) : Type | null {
            let depth = 0;

            // can we share "pending" results from nested evaluateType calls? (subtype comparison will sometimes have to call evaluate type...)
            // i.e they kick off a new `evaluateType` run? ...
            // maybe we should make it so subtype comparison should never have to call evaluate type?...
            const saved_typeConstructorInvocationCacheTrie = typeConstructorInvocationCacheTrie;

            const result = typeWorker(type, typeParamMap, partiallyApplyGenericFunctionSigs, undefined, initialFallbackToAny);

            typeConstructorInvocationCacheTrie = saved_typeConstructorInvocationCacheTrie;

            return result;

            function typeWorker(type: Readonly<Type> | null,
                                typeParamMap: ReadonlyMap<string, Type> | undefined = type?.capturedContext,
                                partiallyApplyGenericFunctionSigs = false,
                                lookupDeferrals?: ReadonlySet<string>,
                                fallbackToAny = initialFallbackToAny,
                                ) : Type | null {
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
                            const evaluatableType = symTabEntry.effectivelyDeclaredType ?? symTabEntry.lexicalType ?? BuiltinType.any;
                            const evaluatedType = typeWorker(evaluatableType);
                            if (!evaluatedType) return null;

                            if (evaluatableType !== evaluatedType) { // fixme: merely expanding a non-generic alias will trigger this, since (alias !== *alias) (also consider Array after instantiation to be concrete...) )
                                concrete = false;
                            }
                            evaluatedStructContents.set(canonicalName, {
                                uiName: symTabEntry.uiName,
                                flags: 0,
                                declarations: null,
                                canonicalName,
                                lexicalType: undefined,
                                effectivelyDeclaredType: evaluatedType,
                                symbolId: -1,
                            });
                        }

                        const result = Struct(evaluatedStructContents);

                        if (concrete) {
                            (result as Mutable<Type>).concrete = true;
                        }

                        return result;
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
                            const freshType = typeWorker(
                                sig.params[i].paramType,
                                typeParamMap,
                                partiallyApplyGenericFunctionSigs,
                                updatedLookupDeferrals
                            );

                            if (!freshType) {
                                return null;
                            }

                            if (sig.params[i].flags & TypeFlags.spread && freshType.kind === TypeKind.tuple) {
                                originalTypeWasConcrete = false;
                                for (const tupleElement of freshType.elements) {
                                    if (!tupleElement.effectivelyDeclaredType) {
                                        return null;
                                    }
                                    const freshType = typeWorker(
                                        tupleElement.effectivelyDeclaredType,
                                        typeParamMap,
                                        partiallyApplyGenericFunctionSigs,
                                        updatedLookupDeferrals
                                    );
                                    if (!freshType) {
                                        return null;
                                    }
                                    params.push(
                                        cfFunctionSignatureParam(
                                            /*required*/ !tupleElement.links?.optional,
                                            freshType,
                                            tupleElement.uiName,
                                            /*spread*/ false
                                        )
                                    );
                                }
                            }
                            else {
                                originalTypeWasConcrete = originalTypeWasConcrete && (freshType === sig.params[i].paramType);
                                params.push(
                                    cfFunctionSignatureParam(
                                        !(sig.params[i].flags & TypeFlags.optional),
                                        freshType,
                                        sig.params[i].uiName,
                                        !!(sig.params[i].flags & TypeFlags.spread)));
                            }
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
                            if (type.left.name === "cfc" && !type.left.next) {
                                const targetType = type.args[0];
                                if (!targetType) {
                                    return fallbackToAny ? BuiltinType.any : null;
                                }
                                const shouldBeStringLiteralType = typeWorker(targetType, typeParamMap);
                                if (!shouldBeStringLiteralType || shouldBeStringLiteralType.kind !== TypeKind.literal || typeof shouldBeStringLiteralType.literalValue !== "string") {
                                    return fallbackToAny ? BuiltinType.any : null;
                                }
                                return typeWorker(CfcLookup(shouldBeStringLiteralType.literalValue));
                            }

                            const constructor = deepResolveTypenameFromTypeParamMapOrContainerWalk(type.context, typeParamMap, type.left);

                            if (!constructor || (constructor.kind !== TypeKind.typeConstructor && constructor.kind !== TypeKind.interface)) {
                                return fallbackToAny ? BuiltinType.any : null;
                            }

                            // huh, we don't support user-defined type constructors that aren't interface defs
                            if (constructor.kind === TypeKind.typeConstructor) {
                                const maybeStringLiteral = (type.args[0] && typeWorker(type.args[0], typeParamMap)) ?? BuiltinType.any;
                                if (maybeStringLiteral.kind === TypeKind.literal && typeof maybeStringLiteral.literalValue === "string") {
                                    if (constructor === builtin_capitalize.type) {
                                        const result = maybeStringLiteral.literalValue.length > 0
                                            ? maybeStringLiteral.literalValue[0].toLocaleUpperCase() + maybeStringLiteral.literalValue.slice(1)
                                            : "";
                                        return createLiteralType(result);
                                    }
                                    else if (constructor === builtin_uncapitalize.type) {
                                        const result = maybeStringLiteral.literalValue.length > 0
                                            ? maybeStringLiteral.literalValue[0].toLocaleLowerCase() + maybeStringLiteral.literalValue.slice(1)
                                            : "";
                                        return createLiteralType(result);
                                    }
                                    else if (constructor === builtin_lowercase.type) {
                                        const result = maybeStringLiteral.literalValue.toLocaleLowerCase();
                                        return createLiteralType(result);
                                    }
                                    else if (constructor === builtin_uppercase.type) {
                                        const result = maybeStringLiteral.literalValue.toLocaleUpperCase();
                                        return createLiteralType(result);
                                    }
                                }
                            }


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

                                if (constructor.kind === TypeKind.interface) {
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
                                else if (constructor.kind === TypeKind.typeConstructor) {
                                    // need to do the cache thing just like w/ interfaces to support recursive things like @!typedef X<T> = {x: string, y: X<T>}
                                    const freshParamMap = mapMerge(typeParamMap, instantiableParamMap);
                                    return typeWorker(constructor.body, freshParamMap, partiallyApplyGenericFunctionSigs, lookupDeferrals, fallbackToAny);
                                }
                        }

                        return fallbackToAny ? BuiltinType.any : null;

                    }
                    else if (type.kind === TypeKind.typeId && !lookupDeferrals?.has(type.name)) { // if we get a non-generic alias we should probably cache it's full instantiation?
                        let result = deepResolveTypenameFromTypeParamMapOrContainerWalk(type.context, typeParamMap, type);
                        if (!result) {
                            return fallbackToAny ? BuiltinType.any : null;
                        }
                        return typeWorker(result, typeParamMap);
                    }
                    else if (type.kind === TypeKind.conditional) {
                        // even if current evaluation is fallbackToAny=true,
                        // we want to return null here if we can't resolve the A or B in "B extends A ? t : f"
                        // if B or A fails to resolve, we take the false branch
                        const what = typeWorker(type.typeId, typeParamMap, partiallyApplyGenericFunctionSigs, lookupDeferrals, /*fallbackToAny*/ false);
                        const extends_ = typeWorker(type.extends, typeParamMap, partiallyApplyGenericFunctionSigs, lookupDeferrals, /*fallbackToAny*/false);

                        if (!what || !extends_) {
                            return typeWorker(type.alternative, typeParamMap);
                        }

                        // we are in an inferrable position, so we might get inference results in this out param
                        const inferenceResults = new Map<string, Type>();

                        if (isLeftSubtypeOfRight(
                            what,
                            extends_,
                            /*sourceIsLiteralExpr*/ false,
                            /*forReturnType*/ false,
                            /*widenLiterals*/false,
                            typeParamMap,
                            inferenceResults
                        )) {
                            const freshTypeParamMap = mapMerge(typeParamMap, inferenceResults);
                            return typeWorker(type.consequent, freshTypeParamMap);
                        }
                        else {
                            return typeWorker(type.alternative, typeParamMap);
                        }
                    }
                    else if (type.kind === TypeKind.keyof) {
                        // caching? this will probably get very slow against big interfaces used as path lookup maps?
                        const operandType = typeWorker(type.operand);
                        if (!operandType || !isStructLike(operandType)) {
                            return fallbackToAny ? BuiltinType.any : null;
                        }
                        const keys = new Set<string>();
                        for (const symTabEntry of operandType.members.values()) {
                            keys.add(symTabEntry.uiName);
                        }
                        return cfKeyof(type.operand, keys);
                    }
                    // might collapse to a string literal, and might be an inference target for a template string literal
                    else if (type.kind === TypeKind.interpolatedString) {
                        // fixme, this is because we jammed inference targets into interpolated string literal node types
                        type JoiningFourWorlds =
                            | {union: cfLiteralType[], value?: never , node?: never,    inferenceTarget?: never}
                            | {union?: never,          value:  string, node?: never,    inferenceTarget?: never}
                            | {union?: never,          value?: never,  node:  TextSpan, inferenceTarget?: never}
                            | {union?: never,          value?: never,  node?: never,    inferenceTarget:  cfInterpolatedString }


                        const builder : JoiningFourWorlds[] = [];

                        for (const element of type.expr.elements) {
                            if (element.kind === NodeKind.textSpan) {
                                builder.push({node: element});
                            }
                            else {
                                // fixme: heavy reliance on not very strictly typed structure here
                                const typeExpr = ((element.expr as TypeAnnotation | undefined)?.type as cfTypeId | undefined); // can be any type here? at least typeID | typeConstructorInvocation
                                if (!typeExpr) {
                                    continue; // effectively push ""
                                }
                                else if (typeExpr.flags & TypeFlags.inferenceTarget) {
                                    builder.push(
                                        {inferenceTarget: cfInterpolatedString(__FIXME__synthesizeAWholeInterpolatedStringLiteral([element]), true)}
                                    )
                                }
                                else {
                                    const resultingStringLike = typeWorker(typeExpr, typeParamMap);
                                    if (resultingStringLike) {
                                        if (resultingStringLike.kind === TypeKind.literal && typeof resultingStringLike.literalValue === "string") {
                                            builder.push({value: resultingStringLike.literalValue})
                                        }
                                        else if (resultingStringLike.kind === TypeKind.union && resultingStringLike.types.every(type => type.kind === TypeKind.literal && typeof type.literalValue === "string")) {
                                            builder.push({union: resultingStringLike.types as cfLiteralType[]})
                                        }
                                    }
                                    else {
                                        return fallbackToAny ? BuiltinType.any : null;
                                    }
                                }
                            }
                        }

                        // needs to contain a single empty element so cross product wiht N is initially (N x 1) instead of (N x 0)
                        let crossProduct : (cfLiteralType | cfInterpolatedString)[] = [createLiteralType("")];

                        for (const world of builder) {
                            if (world.union) {
                                // crossProduct = union x crossProduct
                                crossProduct = world.union.flatMap(
                                    unionConstituent => crossProduct.map(
                                        base => typeStringJoin(base, unionConstituent)));
                            }
                            else if (world.value) {
                                crossProduct = crossProduct.map(base => typeStringJoin(base, createLiteralType(world.value)));
                            }
                            else if (world.node) {
                                crossProduct = crossProduct.map(base => typeStringJoin(base, createLiteralType(world.node.text)));
                            }
                            else if (world.inferenceTarget) {
                                crossProduct = crossProduct.map(base => typeStringJoin(base, world.inferenceTarget))
                            }
                        }

                        return unionify(crossProduct);

                        function typeStringJoin(l: cfLiteralType | cfInterpolatedString, r: cfLiteralType | cfInterpolatedString) : cfLiteralType | cfInterpolatedString {
                            if (l.kind === TypeKind.literal && r.kind === TypeKind.literal) {
                                return createLiteralType(l.literalValue as string + r.literalValue as string)
                            }
                            else if (l.kind === TypeKind.literal && r.kind === TypeKind.interpolatedString) {
                                const mergedElements = l.literalValue === "" ? r.expr.elements : [__FIXME__synthesizeTextSpan(l.literalValue as string), ...r.expr.elements];
                                const underlying = __FIXME__synthesizeAWholeInterpolatedStringLiteral(mergedElements);
                                return cfInterpolatedString(underlying, !!(r.flags & TypeFlags.inferenceTarget));
                            }
                            else if (l.kind === TypeKind.interpolatedString && r.kind === TypeKind.literal) {
                                const mergedElements = r.literalValue === "" ? l.expr.elements : [...l.expr.elements, __FIXME__synthesizeTextSpan(r.literalValue as string)];
                                const underlying = __FIXME__synthesizeAWholeInterpolatedStringLiteral(mergedElements);
                                return cfInterpolatedString(underlying, !!(l.flags & TypeFlags.inferenceTarget));
                            }
                            else if (l.kind === TypeKind.interpolatedString && r.kind === TypeKind.interpolatedString) {
                                const mergedElements = [...l.expr.elements, ...r.expr.elements];
                                const underlying = __FIXME__synthesizeAWholeInterpolatedStringLiteral(mergedElements);
                                return cfInterpolatedString(underlying, !!(l.flags & TypeFlags.inferenceTarget) || !!(r.flags & TypeFlags.inferenceTarget));
                            }
                            else {
                                throw "unreachable (in join interpolated string type)"
                            }
                        }

                        function __FIXME__synthesizeTextSpan(s: string) {
                            return TextSpan(SourceRange.Nil(), s);
                        }
                        function __FIXME__synthesizeAWholeInterpolatedStringLiteral(es: (TextSpan | HashWrappedExpr)[]) {
                            return InterpolatedStringLiteral(
                                TokenType.QUOTE_SINGLE,
                                NilTerminal(-1),
                                es,
                                NilTerminal(-1)
                            );
                        }
                    }
                    else {
                        // a type not requiring evaluation: number, string, boolean, some alias for a type constructor
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
                        const freshType = typeWorker(symTabEntry.effectivelyDeclaredType ?? symTabEntry.lexicalType ?? BuiltinType.any, preInstantiatedArgMap, true);
                        if (!freshType) {
                            return {status: TypeCache_Status.failure};
                        }
                        instantiatedMembers.set(name, {...symTabEntry, effectivelyDeclaredType: freshType});
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
        __experimental__runCompletionsHook,
        // for completions for generic functions with constrained type params
        // probably a Checker should be per file
        __experimental__instantiateType: (reconfigureForSourceFile: SourceFile, type: Type) => {
            const savedSourceFile = sourceFile;
            sourceFile = reconfigureForSourceFile;
            const result = evaluateType(type);
            sourceFile = savedSourceFile;
            return result;
        },
    }
}

function mapMerge<K,V>(l: undefined | ReadonlyMap<K,V>, r: undefined | ReadonlyMap<K,V>) {
    const result = new Map<K,V>();
    if (l) {
        for (const [k,v] of l.entries()) {
            result.set(k,v);
        }
    }
    if (r) {
        for (const [k,v] of r.entries()) {
            result.set(k,v);
        }
    }
    return result;
}

export interface CheckerInstallable {
    CfcResolver: CfcResolver,
    EngineSymbolResolver: EngineSymbolResolver,
    LibTypeResolver: LibTypeResolver
}

export type Checker = ReturnType<typeof Checker>;
