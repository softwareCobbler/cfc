import { Diagnostic, SymTabEntry, ArrowFunctionDefinition, BinaryOperator, Block, BlockType, CallArgument, FunctionDefinition, Node, NodeKind, Statement, StatementType, VariableDeclaration, mergeRanges, BinaryOpType, IndexedAccessType, IndexedAccess, IndexedAccessChainElement, SourceFile, CfTag, CallExpression, UnaryOperator, Conditional, ReturnStatement, BreakStatement, ContinueStatement, FunctionParameter, Switch, SwitchCase, Do, While, Ternary, For, ForSubType, StructLiteral, StructLiteralInitializerMember, ArrayLiteral, ArrayLiteralInitializerMember, Try, Catch, Finally, ImportStatement, New, SimpleStringLiteral, InterpolatedStringLiteral, Identifier, isStaticallyKnownScopeName, StructLiteralInitializerMemberSubtype, SliceExpression, NodeWithScope, Flow, freshFlow, UnreachableFlow, FlowType, ConditionalSubtype, SymbolTable, Property, ParamStatement, ParamStatementSubType, typeinfo, DiagnosticKind, StaticallyKnownScopeName, SwitchCaseType, DestructuredRecordElement, DestructuredElement, DestructuredRecordElementKind, DestructuredElementType, DestructuredList, DestructuredRecord, DiagnosticPhase, NodeFlags } from "./node";
import { getTriviallyComputableString, visit, getAttributeValue, getContainingFunction, isInCfcPsuedoConstructor, stringifyLValue, isNamedFunctionArgumentName, isObjectLiteralPropertyName, isInScriptBlock, exhaustiveCaseGuard, getComponentAttrs, getTriviallyComputableBoolean, stringifyDottedPath, walkupScopesToResolveSymbol, findAncestor, TupleKeyedMap, isNamedFunction, isInEffectiveConstructorMethod } from "./utils";
import { CfFileType, Scanner, SourceRange } from "./scanner";
import { BuiltinType, Type, Interface, cfTypeId, TypeKind } from "./types";
import { Engine, EngineVersions, supports } from "./engines";
import { ProjectOptions } from "./project";

let symbolId = 0;

export function Binder(options: ProjectOptions) {
    const engineVersion = options.engineVersion;
    const debug = options.debug;
    
    let sourceFile : NodeWithScope<SourceFile>;
    let currentContainer : NodeWithScope;
    let scanner : Scanner;
    let diagnostics: Diagnostic[];

    let diagnosticIssuanceMap! : TupleKeyedMap<[number, number, string], Diagnostic>;
    let currentFlow : Flow;
    let currentJumpTargetPredecessors : Flow[];
    let withPropertyAccessors = false;
    // @useless-transient let pendingSymbolResolutionStack : Map<string, Set<SymbolTable>>[] = [];

    function bind(sourceFile_: SourceFile) {
        if (sourceFile_.cfFileType === CfFileType.dCfm) {
            bindDeclarationFile(sourceFile_);
            return;
        }
        
        sourceFile = sourceFile_ as NodeWithScope<SourceFile>;
        currentContainer = sourceFile_;
        scanner = sourceFile_.scanner;
        diagnostics = sourceFile_.diagnostics;

        diagnosticIssuanceMap = TupleKeyedMap();
        currentFlow = freshFlow([], FlowType.start);
        currentJumpTargetPredecessors = [];
        withPropertyAccessors = false;
        // @useless-transient pendingSymbolResolutionStack = [new Map()];
        
        sourceFile.containedScope = {
            parentContainer: null,
            typeinfo: sourceFile.containedScope.typeinfo, // parse phase will have provided typedefs
            variables: new Map<string, SymTabEntry>(),
            application: new Map<string, SymTabEntry>(),
            url: new Map<string, SymTabEntry>(),
            form: new Map<string, SymTabEntry>(),
            cgi: new Map<string, SymTabEntry>(),
        };

        if (sourceFile.cfFileType === CfFileType.cfc) {
            sourceFile.containedScope.this = new Map<string, SymTabEntry>();
            sourceFile.containedScope.super = new Map<string, SymTabEntry>();
            const componentAttrs = getComponentAttrs(sourceFile);
            if (componentAttrs) {
                const accessors = getAttributeValue(componentAttrs, "accessors");
                if (accessors === null) { // property not present
                    withPropertyAccessors = false;
                }
                else if (accessors === undefined) { // property present by name only, no explicit value
                    withPropertyAccessors = true;
                }
                else { // property present with value, should be a bool
                    withPropertyAccessors = !!getTriviallyComputableBoolean(accessors);
                }
            }
        }

        if (sourceFile.cfFileType === CfFileType.cfc) {
            sourceFile.containedScope.this = new Map<string, SymTabEntry>();
        }

        bindTypeAndInterfacedefsForContainer(sourceFile);
        bindList(sourceFile.content, sourceFile);

        (sourceFile as any) = undefined;
        (currentContainer as any) = undefined;
        (scanner as any) = undefined;
        (diagnostics as any) = undefined;
        (diagnosticIssuanceMap as any) = undefined;
        (currentFlow as any) = undefined;
        (currentJumpTargetPredecessors as any) = undefined;
        (withPropertyAccessors as any) = undefined;
        // @useless-transient (pendingSymbolResolutionStack as any) = undefined;
    }

    function mergeFlowsToJumpTarget(...flows: (Flow|undefined)[]) {
        const filteredFlows = (() : Set<Flow> => {
            const flowFilter = new Set(flows);
            flowFilter.delete(undefined);
            flowFilter.delete(UnreachableFlow);
            return flowFilter as Set<Flow>;
        })();

        return filteredFlows.size === 0
            ? UnreachableFlow
            : freshFlow([...filteredFlows], FlowType.jumpTarget);
    }

    function bindNode(node: Node | null | undefined, parent: Node) : void {
        if (!node) return;

        //
        // in incremental cases, clear out checker state
        // this isn't really great, but it does support re-running checker errors.
        //
        node.flags &= ~(NodeFlags.checked | NodeFlags.checkerError)

        node.flow = currentFlow;
        node.parent = parent;

        bindDirectTerminals(node);

        switch (node.kind) {
            case NodeKind.sourceFile:
                throw "Bind source files by binding its content";
            case NodeKind.comment:
                // fixme: a better solution might be to have the parser do the same thing to typedefs that it does to type annotations
                // this has issues with typedefs bound to trailing trivia, i.e. `function foo{} /** @!typedef ... */ function bar() {}`
                // the typedef above is attached to foo, not bar (and really it should be attached to whatever the outer container is)
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
                bindNode(node.headKey, node);
                bindList(node.rest, node);
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
                console.log("@DEBUG -- hit a typeshim in bindNode, we should be handling these manually when binding containers")
                return;
            case NodeKind.property:
                bindProperty(node);
                return;
            case NodeKind.paramStatement:
                bindParamStatement(node);
                return;
            case NodeKind.staticAccess:
                bindNode(node.left, node);
                bindNode(node.right, node);
                return;
            case NodeKind.destructuredRecord:
                bindDestructuringRoot(node);
                return;
            case NodeKind.destructuredList:
                bindDestructuringRoot(node);
                return;
            case NodeKind.destructuredElement:
                bindMaybeDestructuringLeaf(node);
                return;
            case NodeKind.destructuredRecordElement:
                bindMaybeDestructuringLeaf(node);
                return;
            case NodeKind.typedArrayLiteral:
                // we don't do anything with the typehint
                bindArrayLiteral(node.array);
                return;
            default:
                exhaustiveCaseGuard(node);
        }
    }

    function bindDirectTerminals(node: Node) {
        visit(node, function(visitedNode: Node | null | undefined) {
            if (visitedNode?.kind === NodeKind.terminal) {
                visitedNode.parent = node;
                bindList(visitedNode.trivia, visitedNode);
            }
        });
    }

    function bindList(nodes: readonly Node[], parent: Node) {
        for (let i = 0; i < nodes.length; ++i) {
            bindNode(nodes[i], parent);
        }
    }

    const scopeInterfaceNames : StaticallyKnownScopeName[] = ["variables", "application"];

    function bindTypeAndInterfacedefsForContainer(node: Node) {
        if (!node.containedScope) {
            return;
        }
        for (const [name, defs] of node.containedScope.typeinfo.interfaces) {
            const localMergedDef = defs.length === 1 ? defs[0] : mergeInterfaces(name, defs);
            const parentMergedDef = mergeInterfaceWithParent(node, name, localMergedDef);
            node.containedScope.typeinfo.mergedInterfaces.set(name, parentMergedDef);
        }
        for (const [name, type] of node.containedScope.typeinfo.aliases) {
            currentContainer.containedScope.typeinfo.aliases.set(name, type);
        }
        if (node === sourceFile) {
            for (const name of scopeInterfaceNames) {
                node.containedScope.typeinfo.mergedInterfaces.set(
                    name, mergeInterfaceWithParent(node, name, node.containedScope.typeinfo.mergedInterfaces.get(name)) )
            }
        }
    }
    
    function mergeInterfaces(name: string, interfaces: readonly Readonly<Interface>[]) : Interface {
        const mergedMembers = new Map<string, SymTabEntry>();
        for (const iface of interfaces) {
            for (const [name, symTabEntry] of iface.members) {
                mergedMembers.set(name, symTabEntry);
            }
        }
        return Interface(name, mergedMembers);
    }

    function mergeInterfaceWithParent(base: Node, name: string, iface: Readonly<Interface> | undefined) : Interface {
        if (base === sourceFile) {
            const mergeable : Readonly<Interface>[] = [];
            for (const lib of sourceFile.libRefs.values()) {
                if (lib.containedScope.typeinfo.mergedInterfaces.has(name)) {
                    mergeable.push(lib.containedScope.typeinfo.mergedInterfaces.get(name)!);
                }
            }
            if (iface) mergeable.push(iface);
            return mergeInterfaces(name, mergeable);
        }

        let working : Node | null = base;

        while (working) {
            if (working.containedScope) {
                if (working.containedScope.typeinfo.mergedInterfaces.has(name)) {
                    const mergeable = [working.containedScope.typeinfo.mergedInterfaces.get(name)!];
                    if (iface) mergeable.push(iface);
                    return mergeInterfaces(name, mergeable);
                }
                else {
                    working = working.containedScope.parentContainer;
                }
            }
            else {
                working = working.parent;
            }
        }
        return iface as Interface;
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
                // no-op
                return;
            default:
                exhaustiveCaseGuard(node);
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
            let expr : Node;
            if (node.fromTag) {
                expr = (node.tagOrigin.startTag as CfTag.ScriptLike).expr!;
            }
            else {
                expr = node.expr!;
            }

            bindNode(expr, node);
        }
        
        const savedStartFlow = currentFlow;
        const trueFlow = freshFlow(currentFlow, FlowType.default);
        let trueEndFlow : Flow;
        let falseEndFlow : Flow | undefined = undefined;

        currentFlow = trueFlow;
        bindNode(node.consequent, node);
        trueEndFlow = currentFlow;

        if (node.alternative) {
            currentFlow = savedStartFlow;
            bindNode(node.alternative, node);
            falseEndFlow = currentFlow;
        }

        if (node.subType === ConditionalSubtype.else) {
            currentFlow = trueEndFlow;
        }
        else {
            currentFlow = mergeFlowsToJumpTarget(trueEndFlow, falseEndFlow || savedStartFlow);
        }
    }

    // the symbol and its declarations already fully exist, we just want to include it in another symbol table
    // it is common for `variables` and `this` to share symbols
    function addExistingSymbolToTable(table: SymbolTable, entry: SymTabEntry) {
        table.set(entry.canonicalName, entry);
    }

    function addDeclarationToSymbol(symbol: SymTabEntry, decl: Node) {
        if (symbol.declarations) symbol.declarations.push(decl);
        else symbol.declarations = [decl];
    }

    function addFreshSymbolToTable(symTab: SymbolTable, uiName: string, declaringNode: Node, type?: Type) : SymTabEntry {
        const canonicalName = uiName.toLowerCase();
        let symTabEntry : SymTabEntry;

        // the symbol name might already exist, by way of a duplicate definition, or an auto-generated property getter/setter followed by a user-supplied custom getter/setter, or (others?)
        // in that case, we just add the declaration to the symbol
        if (symTab.has(canonicalName)) {
            symTabEntry = symTab.get(canonicalName)!;
            // don't add a duplicate declaration
            // say we add symbol X with declaration node N<1> to `variables`, and then want to add the same symbol to `this`;
            // and we may have done so for the same symbol name earlier with decl node N<0> (i.e. symbol X where N<0> was an auto-gen'd property setter, now N<1> is a custom setter with the same name)
            // so `variables` and `this` both have X, which has decl N<0>
            // Now, we may have added decl N<1> to X by way of a preceding call to addFreshSymbolToTable for `variables`, and now we are adding to `this`
            // the reference to X in `this` is the same object as in `variables` and so X in `this` already has decl's N<0> and N<1>, and we are trying to add N<1> again
            // could maybe use a Set<Node>
            if (!symTabEntry.declarations?.some((existingDecl) => existingDecl.nodeId === declaringNode.nodeId)) {
                addDeclarationToSymbol(symTabEntry, declaringNode);
            }
        }
        // otherwise, we create a new entry
        else {
            const freshSymbolId = symbolId++;

            symTabEntry = {
                uiName,
                canonicalName,
                declarations: [declaringNode],
                firstLexicalType: type,
                symbolId: symbolId++,
            }

            symTab.set(canonicalName, symTabEntry);
            sourceFile.symbolIdToSymbol.set(freshSymbolId, symTabEntry);
        }

        return symTabEntry;
    }

    function bindForInInit(node: VariableDeclaration) : void {
        let targetScope = sourceFile.containedScope.variables!;
        if (node.varModifier) {
            const containingFunction = getContainingFunction(node);
            if (!containingFunction) {
                issueDiagnosticAtRange(node.expr.range, "Local variables may not be declared at top-level scope.");
                return;
            }
            targetScope = containingFunction.containedScope?.local!;
        }

        bindNode(node.expr, node);

        if (node.expr.kind === NodeKind.identifier) {
            const name = getTriviallyComputableString(node.expr);
            if (!name) return;
            addFreshSymbolToTable(targetScope, name, node);
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
            bindNode(node.expr.right, node);

            currentFlow = freshFlow(currentFlow, FlowType.assignment, node.expr.left);
            bindNode(node.expr.left, node);
        }
        else if (node.expr.kind === NodeKind.identifier) {
            bindNode(node.expr, node);
        }
        else {
            // unreachable? 
        }

        let identifierBaseName : ReturnType<typeof stringifyLValue> | undefined = undefined;
        
        if (node.expr.kind === NodeKind.binaryOperator && node.expr.optype === BinaryOpType.assign &&
            (node.expr.left.kind === NodeKind.indexedAccess || node.expr.left.kind === NodeKind.identifier)) {
            identifierBaseName = stringifyLValue(node.expr.left);
        }
        else if (node.expr.kind === NodeKind.identifier) {
            identifierBaseName = stringifyLValue(node.expr);
        }

        // make sure we got a useable name
        if (!identifierBaseName) {
            return;
        }

        const [uiPath, canonicalPath] = [identifierBaseName.ui.split("."), identifierBaseName.canonical.split(".")];

        if (isStaticallyKnownScopeName(canonicalPath[0]) && canonicalPath.length === 2) {
            let targetScope : SymbolTable | undefined = undefined;
            if (canonicalPath[0] === "local") {
                targetScope = currentContainer.containedScope.local;
            }
            else {
                targetScope = sourceFile.containedScope[canonicalPath[0]];
            }

            if (targetScope) {
                addFreshSymbolToTable(targetScope, uiPath[1], node);
            }

            return;
        }

        if (node.finalModifier || node.varModifier) {
            // @useleess-transient 
            // const canonicalName = canonicalPath[0];
            // resolvePendingSymbolResolutions(canonicalName);

            if (getContainingFunction(node)) {
                addFreshSymbolToTable(currentContainer.containedScope.local!, uiPath[0], node);
            }
            else {
                // we're not in a function, so we must be at top-level scope
                issueDiagnosticAtRange(mergeRanges(node.finalModifier, node.varModifier, (<BinaryOperator>node.expr)?.left), "Local variables may not be declared at top-level scope.");
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

    function isDefaultCaseDescendant(node: Node) {
        return !!findAncestor(node, (node) => {
            return node.kind === NodeKind.switchCase
                && node.caseType === SwitchCaseType.default
                ? true
                : node.kind === NodeKind.switchCase
                && node.caseType === SwitchCaseType.case
                ? "bail"
                : false;
        });
    }

    function bindReturnStatement(node: ReturnStatement) {
        let resultingFlow = UnreachableFlow;
        if (!getContainingFunction(node)) {
            // error, should issue a diagnostic, and we won't update the flow on complete
            resultingFlow = currentFlow;
        }

        if (isDefaultCaseDescendant(node)) {
            currentJumpTargetPredecessors.push(UnreachableFlow);
        }

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

        currentFlow = resultingFlow;
    }

    function bindBreakStatement(node: BreakStatement) {
        currentJumpTargetPredecessors.push(currentFlow);
        currentFlow = UnreachableFlow;

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
                bindList(node.stmtList, node);
                bindNode(node.tagOrigin.endTag, node);
                break;
            case BlockType.scriptSugaredTagCallBlock:
                // check against cf tag meta
                bindList(node.sugaredCallStatementAttrs!, node);
                bindList(node.stmtList, node);
                break;
            case BlockType.scriptTagCallBlock:
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
        let engineInterfaceTypeIdName: string | undefined = undefined; // fixme: parser will need to prevent users from spelling these names, which means we need a list of them to check against

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
                engineInterfaceTypeIdName = "__cfHTTP";
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

        // @useless-transient
        // unqualified result names (i.e. no dots in the name) get written to the transient scope if we are in a container with a transient scope (i.e. a function)
        // otherwise, it goes straight into the root variables scope
        // we do not push a symbol resolution for the __transient in this case; this is an assignment to a var that will be in play for the remainder of the function
        //let targetScope : SymbolTable = currentContainer.containedScope.__transient ?? sourceFile.containedScope.variables!;
        let targetScope : SymbolTable = currentContainer.containedScope.local ?? sourceFile.containedScope.variables!;
        let targetName = name.length === 1 ? name[0] : name[1];

        if (name.length === 2) {
            switch (name[0].toLowerCase()) {
                case "local": {
                    if (currentContainer.containedScope.local) {
                        targetScope = currentContainer.containedScope.local;
                    }
                    else {
                        // const attrVal = getAttributeValue((<CfTag.Common>tag).attrs, attrName);
                        // // this is not really an error; it will simply write into `local.foo` in the global context
                        // // but perhaps is a code smell to be warned about
                        // if (attrVal) errorAtRange(attrVal.range, `Tag binds result name '${name[1]}' to a local scope in a non-local context.`);
                        // return;
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

        // @useless-transient resolvePendingSymbolResolutions(targetName);
        addFreshSymbolToTable(
            targetScope,
            targetName,
            tag,
            engineInterfaceTypeIdName
                ? cfTypeId(engineInterfaceTypeIdName)
                : undefined);
    }

    function bindSimpleStringLiteral(node: SimpleStringLiteral) {
        bindNode(node.textSpan, node);
    }

    function bindInterpolatedStringLiteral(node: InterpolatedStringLiteral) {
        bindList(node.elements, node);
    }

    function bindIdentifier(node: Identifier) {
        bindNode(node.source, node);
        checkIdentifierValidity(node);
    }

    function bindIndexedAccess(node: IndexedAccess) {
        bindNode(node.root, node);
        let parent : Node = node.root;

        if (
            (node.root.kind === NodeKind.simpleStringLiteral || node.root.kind === NodeKind.interpolatedStringLiteral) &&
            node.accessElements[0]?.accessType === IndexedAccessType.bracket
            && !supports.bracketAccessIntoStringLiteral(engineVersion)
        ) {
            issueDiagnosticAtRange(node.accessElements[0].range, `cf engine ${engineVersion.uiString} does not support bracket-access into string literals.`);
        }

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
                //addFreshSymbolToTable(currentContainer.containedScope.arguments!, node.uiName, node); // fixme: already done in bindFunctionDefinition
            }

            return;
        }

        //addFreshSymbolToTable(currentContainer.containedScope.arguments!, node.uiName, node); // fixme: already done in bindFunctionDefinition
        bindNode(node.javaLikeTypename, node);
        bindNode(node.identifier, node);
        bindNode(node.defaultValue, node);
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

    function isBuiltinScopeName(s: string | undefined) : s is "url" | "form" | "cgi" | "variables" | "local" {
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

    // n.b. this is different than a declaring assignment, like `var x = y`;
    // this is for non-declaring assignments like `x = y`
    function bindAssignment(node: BinaryOperator) {
        bindNode(node.left, node);

        bindNode(node.right, node);

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
                    firstAccessAsString = firstAccessElement.property.token.text;
                }
                else if (firstAccessElement.accessType === IndexedAccessType.bracket) {
                    firstAccessAsString = getTriviallyComputableString(firstAccessElement.expr)
                }

                if (!firstAccessAsString) {
                    return;
                }

                if (targetBaseName === "local") {
                    if (currentContainer.containedScope.local) {
                        addFreshSymbolToTable(currentContainer.containedScope.local, firstAccessAsString, node);
                    }
                    else {
                        // assigning to `local.x` in a non-local scope just binds the name `local` to the root variables scope
                        addFreshSymbolToTable(sourceFile.containedScope.variables!, "local", node);
                    }
                }
                else if (targetBaseName === "variables") {
                    if (sourceFile.cfFileType === CfFileType.cfc) {
                        
                        if (isInCfcPsuedoConstructor(node) || isInEffectiveConstructorMethod(node)) {
                            addFreshSymbolToTable(sourceFile.containedScope[targetBaseName]!, firstAccessAsString, node);
                        }
                    }
                    else {
                        addFreshSymbolToTable(sourceFile.containedScope[targetBaseName]!, firstAccessAsString, node);
                    }
                }
                else {
                    if (targetBaseName in sourceFile.containedScope) {
                        addFreshSymbolToTable(sourceFile.containedScope[targetBaseName]!, firstAccessAsString, node);
                    }
                }
            }
            // not a built-in scope name, just use target base name as identifier
            // this is not a declaration so it binds to the root variables scope
            else {
                //weakBindIdentifierToScope(targetBaseName, sourceFile.containedScope.variables!);
            }
        }
        else if (target.kind === NodeKind.identifier) {
            const targetBaseName = getTriviallyComputableString(target);
            if (targetBaseName) {
                // @useless-transient const targetBaseCanonicalName = targetBaseName.toLowerCase();

                const existingSymbol = walkupScopesToResolveSymbol(node, targetBaseName);

                if (existingSymbol) {
                    currentFlow = freshFlow(currentFlow, FlowType.assignment, target);
                    target.flow = currentFlow;
                    return;
                }

                // @useless-transient
                // const targetScope = currentContainer === sourceFile
                //     ? currentContainer.containedScope.variables!
                //     : currentContainer.containedScope.__transient!;

                // if (targetScope === currentContainer.containedScope.__transient) {
                //     // @useless-transient
                //     //pushPendingSymbolResolution(targetBaseName, currentContainer.containedScope.__transient);
                // }

                // if (targetScope.has(targetBaseCanonicalName)) {
                //     const symbolResolution = targetScope.get(targetBaseCanonicalName)!;
                //     if (symbolResolution.declarations) symbolResolution.declarations.push(node);
                //     else symbolResolution.declarations = [node];
                //     return;
                // }

                // addFreshSymbolToTable(targetScope, targetBaseName, node);
            }
        }
    }

    // @useless-transient
    // function pushPendingSymbolResolution(canonicalName: string, symTab: Map<string, SymTabEntry>) {
    //     const stackTop = pendingSymbolResolutionStack[pendingSymbolResolutionStack.length - 1];
    //     if (stackTop.has(canonicalName)) {
    //         stackTop.get(canonicalName)!.add(symTab);
    //     }
    //     else {
    //         stackTop.set(canonicalName, new Set([symTab]));
    //     }
    // }

    // @useless-transient
    // function pushPendingSymbolResolutionFrame() : void {
    //     pendingSymbolResolutionStack.push(new Map());
    // }

    // @useless-transient
    // function popAndMergePendingSymbolResolutionFrame() : void {
    //     const popped = pendingSymbolResolutionStack.pop();
    //     if (!popped) return; // ?
    //     const top = pendingSymbolResolutionStack[pendingSymbolResolutionStack.length - 1];
    //     for (const [symbolCanonicalName, targetScopeSet] of popped) {
    //         if (top.has(symbolCanonicalName)) {
    //             const existingSet = top.get(symbolCanonicalName)!;
    //             for (const scope of targetScopeSet) {
    //                 existingSet.add(scope);
    //             }
    //         }
    //         else {
    //             top.set(symbolCanonicalName, targetScopeSet);
    //         }
    //     }
    // }

    // @useless-transient
    // function resolvePendingSymbolResolutions(canonicalName: string) {
    //     const stackTop = pendingSymbolResolutionStack[pendingSymbolResolutionStack.length - 1];
    //     // if the top-most stack has the target symbol name, we remove the target symbol name from all the
    //     // scopes listed as participating in trying to find this symbol;
    //     // e.g.
    //     // function outer() {
    //     //     function inner1() {
    //     //         a = 42; // pending resolution, set as __transient on `inner1`
    //     //     }
    //     //     function inner2() {
    //     //         a = 42; // pending resolution, set as __transient on `inner2`
    //     //     }
    //     //     var a = 42; // resolved; remove the __transient symbol from `inner1` and `inner2`; in the checker, symbol lookup for 'a' within inner1/inner2 will now find `outer::local::a`
    //     // }
    //     //
    //     if (stackTop.has(canonicalName)) {
    //         for (const scope of stackTop.get(canonicalName)!) {
    //             scope.delete(canonicalName);
    //         }
    //         stackTop.delete(canonicalName);
    //     }
    // }

    // fixme: make more explicit that we grab signatures here for cfc member functions
    function bindFunctionDefinition(node: FunctionDefinition | ArrowFunctionDefinition) {      
        if (node.kind === NodeKind.arrowFunctionDefinition
            && node.params.length === 1 && !node.parens
            && !supports.noParenSingleArgArrowFunction(engineVersion)
        ) {
            issueDiagnosticAtRange(node.params[0].range, `CF engine '${engineVersion.uiString}' requires arrow function parameter lists to be parenthesized.`)
        }

        if (isNamedFunction(node)) {
            currentFlow = freshFlow(currentFlow, FlowType.assignment, node); // a named function def is effectively a variable declaration
            node.flow = currentFlow;

            // lucee appears to not err on the following, but acf does
            // we need to model that hoistable functions are always hoisted into the root scope,
            // but are only visible within their declaration container:
            // function foo() { 
            //      function bar() {}
            // }
            // function bar() {} -- error, functions may only be defined once
            // bar(); -- error, bar is not visible

            let scopeTargets : SymbolTable[];

            if (currentContainer.containedScope.local) { // it's only callable from local, but in ACF the name is taken globally
                // a named function definition from within a function is essentially the same as a "non-var-scoped var declaration"
                // which means it gets put on the variables scope
                scopeTargets = [sourceFile.containedScope.variables!];
            }
            else if (sourceFile.cfFileType === CfFileType.cfc && isInCfcPsuedoConstructor(node)) {
                scopeTargets = [sourceFile.containedScope.this!, sourceFile.containedScope.variables!];
                const maybeAlreadyDefined = sourceFile.containedScope.this!.get(node.name.canonical);
                if (maybeAlreadyDefined && maybeAlreadyDefined.declarations) {
                    for (const decl of [...maybeAlreadyDefined.declarations, node]) {
                        if (decl.kind === NodeKind.functionDefinition) {
                            const range = decl.fromTag
                                ? decl.tagOrigin.startTag!.range
                                : mergeRanges(decl.accessModifier, decl.returnType, decl.functionToken, decl.nameToken);
                            issueDiagnosticAtRange(range, "Duplicate component method declaration.", DiagnosticKind.warning);
                        }
                    }
                }
            }
            else {
                scopeTargets = [sourceFile.containedScope.variables!];
            }
            
            for (const scopeTarget of scopeTargets) {
                addFreshSymbolToTable(
                    scopeTarget,
                    node.name.ui,
                    node);
            }
        }

        const savedFlow = currentFlow;
        currentFlow = freshFlow([], FlowType.start, node);

        node.containedScope = {
            parentContainer: currentContainer,
            typeinfo: typeinfo(),
            local: new Map<string, SymTabEntry>(),
            arguments: new Map<string, SymTabEntry>(),
            // @useless-transient __transient: new Map<string, SymTabEntry>(),
        };

        bindTypeAndInterfacedefsForContainer(node);

        // @useless-transient pushPendingSymbolResolutionFrame();

        // fixme: we also do this in bindFunctionParameter, so we get duplicate nodes in each param's symbol decl list
        for (let i = 0; i < node.params.length; i++) {
            const param = node.params[i];
            //const annotatedType = typeAnnotatedParams ? typeAnnotatedParams[i]?.paramType : null;
            // we also need the annotation-provided type if it exists; for now it is implicitly null
            addFreshSymbolToTable(node.containedScope.arguments!, param.uiName, param);
        }

        if (!node.fromTag && node.kind === NodeKind.functionDefinition) {
            // access modifier is a terminal which gets autobound in bindDirectTerminals, otherwise we would bind that here, too
            bindNode(node.returnType, node);
            bindNode(node.nameToken, node);
        }

        const savedContainer = currentContainer;
        
        currentContainer = node as NodeWithScope;

        bindList(node.params, node);

        if (node.kind === NodeKind.functionDefinition && node.fromTag) {
            bindList(node.body, node);
        }
        else {
            bindNode(node.body, node);
        }

        currentContainer = savedContainer;

        // @useless-transient popAndMergePendingSymbolResolutionFrame();

        sourceFile.endOfNodeFlowMap.set(node.nodeId, currentFlow);
        currentFlow = savedFlow;
        
        if (isNamedFunction(node)) {
            currentFlow = freshFlow(currentFlow, FlowType.assignment, node);
        }
    }

    function bindSwitch(node: Switch) {
        if (node.fromTag) {
            bindNode(node.tagOrigin.startTag, node);
            bindList(node.cases, node);
            bindNode(node.tagOrigin.endTag, node);
            return;
        }
        bindNode(node.expr, node);

        const startFlow = currentFlow;
        const savedJumpTargetPredecessors = currentJumpTargetPredecessors;
        currentJumpTargetPredecessors = [];

        for (const caseNode of node.cases) {
            currentFlow = currentFlow == startFlow
                ? freshFlow(startFlow, FlowType.switchCase, caseNode) // previous case broke or returned, can only arrive via case match
                : freshFlow([startFlow, currentFlow], FlowType.switchCase, caseNode); // previous case did not break or return; we could get here from fallthrough or case match

            bindNode(caseNode, node);
            if (currentFlow === UnreachableFlow && caseNode.caseType !== SwitchCaseType.default) {
                currentFlow = startFlow;
            }
        }

        currentJumpTargetPredecessors.push(currentFlow);
        currentFlow = mergeFlowsToJumpTarget(...currentJumpTargetPredecessors);
        sourceFile.endOfNodeFlowMap.set(node.nodeId, currentFlow);

        currentJumpTargetPredecessors = savedJumpTargetPredecessors;
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
        const trueFlow = freshFlow(currentFlow, FlowType.default, node.ifTrue);
        const falseFlow = freshFlow(currentFlow, FlowType.default, node.ifFalse);
        
        currentFlow = trueFlow;
        bindNode(node.ifTrue, node);
        const endTrueFlow = currentFlow;
        
        currentFlow = falseFlow;
        bindNode(node.ifFalse, node);
        const endFalseFlow = currentFlow;
        
        currentFlow = mergeFlowsToJumpTarget(endTrueFlow, endFalseFlow);
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

        for (let i = 0; i < node.members.length; i++) {
            const isLast = i === node.members.length - 1;
            const member = node.members[i];
            if (isLast && member.comma && !supports.trailingStructLiteralComma(engineVersion)) {
                issueDiagnosticAtRange(new SourceRange(member.range.toExclusive, member.range.toExclusive), "Illegal trailing comma.");
            }
            if (!isLast && !member.comma) {
                const nextNode = node.members[i+1];
                const targetRange = nextNode.subType === StructLiteralInitializerMemberSubtype.keyed ? nextNode.key.range : nextNode.expr.range;
                issueDiagnosticAtRange(targetRange, "Expected ','");
            }
        }
    }

    function bindStructLiteralInitializerMember(node: StructLiteralInitializerMember) {
        switch (node.subType) {
            case StructLiteralInitializerMemberSubtype.keyed: {
                if (node.shorthand) {
                    bindNode(node.key, node);
                }
                else {
                    bindNode(node.key, node);
                    bindNode(node.expr, node);
                }
                break;
            }
            case StructLiteralInitializerMemberSubtype.spread: {
                bindNode(node.expr, node);
                break;
            }
            default: exhaustiveCaseGuard(node);
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
                    issueDiagnosticAtRange(node.tagOrigin.startTag.range, "A catch tag must be contained within a try tag-block.");
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
                issueDiagnosticAtRange(node.tagOrigin.startTag.range, "A finally tag must be contained within a try tag-block.");
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

    function bindProperty(node: Property) {
        if (node.fromTag) {            
            if (!isInCfcPsuedoConstructor(node)) {
                issueDiagnosticAtRange(node.range, "Properties must be declared at the top-level of a component.")
            }
        }
        else {
            currentFlow = freshFlow(currentFlow, FlowType.assignment);
            node.flow = currentFlow;
            
            const uiNameAttr = getAttributeValue(node.attrs, "name");
            if (!uiNameAttr) {
                issueDiagnosticAtRange(node.range, "Properties must have a 'name' attribute.");
                return;
            }

            const uiName = getTriviallyComputableString(uiNameAttr);
            if (!uiName) {
                issueDiagnosticAtRange(node.range, "Property names cannot be dynamic.");
                return;
            }

            if (uiName.indexOf(".") !== -1) {
                issueDiagnosticAtRange(uiNameAttr.range, "'.' cannot appear in this name.");
                return;
            }

            // property gets add to variables scope with just its name;
            addFreshSymbolToTable(sourceFile.containedScope.variables!, uiName, node, BuiltinType.any);

            // if generating accessors, both variables and this get the getter/setter version of it
            if (withPropertyAccessors) {
                // uppercase the first letter of the propertyname, so that
                // "somePropertyName" becomes "setSomePropertyName" and "getSomePropertyName"
                const camelCasedUiName = uiName[0].toUpperCase() + uiName.slice(1);
                const getter = addFreshSymbolToTable(sourceFile.containedScope.variables!, "get" + camelCasedUiName, node, BuiltinType.anyFunction);
                const setter = addFreshSymbolToTable(sourceFile.containedScope.variables!, "set" + camelCasedUiName, node, BuiltinType.anyFunction);

                addExistingSymbolToTable(sourceFile.containedScope.this!, getter);
                addExistingSymbolToTable(sourceFile.containedScope.this!, setter);
            }
        }
    }

    function bindParamStatement(node: ParamStatement) {
        if (node.subType === ParamStatementSubType.withImplicitTypeAndName) {
            const typeIndex = node.attrs.findIndex(tagAttr => tagAttr.canonicalName === "type");
            if (typeIndex !== -1) {
                issueDiagnosticAtRange(node.attrs[typeIndex].range, `Explicit type attribute shadows implicit type attribute '${stringifyDottedPath(node.implicitType).ui}'.`);
            }
        }

        if (node.subType === ParamStatementSubType.withImplicitTypeAndName || node.subType === ParamStatementSubType.withImplicitName) {
            const nameIndex = node.attrs.findIndex(tagAttr => tagAttr.canonicalName === "name");
            const defaultIndex = node.implicitDefaultExpr ? node.attrs.findIndex(tagAttr => tagAttr.canonicalName === "default") : -1;

            if (nameIndex !== -1) {
                issueDiagnosticAtRange(node.attrs[nameIndex].range, `Explicit name attribute shadows implicit name attribute '${stringifyDottedPath(node.implicitName).ui}'.`);
            }
            if (defaultIndex !== -1) {
                issueDiagnosticAtRange(node.attrs[defaultIndex].range, `Explicit default attribute shadows implicit default value.`);
            }
        }
    }

    function issueDiagnosticAtSpan(fromInclusive: number, toExclusive: number, msg: string, kind = DiagnosticKind.error) {
        const freshDiagnostic : Diagnostic = {kind, phase: DiagnosticPhase.bind, fromInclusive, toExclusive, msg };

        if (diagnosticIssuanceMap.has([fromInclusive, toExclusive, msg])) {
            return;
        }

        if (debug) {
            // const debugFrom = scanner.getAnnotatedChar(freshDiagnostic.fromInclusive);
            // const debugTo = scanner.getAnnotatedChar(freshDiagnostic.toExclusive);
            // bump 0-offsetted info to editor-centric 1-offset
            // freshDiagnostic.__debug_from_line = debugFrom.line+1;
            // freshDiagnostic.__debug_from_col = debugFrom.col+1;
            // freshDiagnostic.__debug_to_line = debugTo.line+1;
            // freshDiagnostic.__debug_to_col = debugTo.col+1;
        }

        diagnosticIssuanceMap.set([fromInclusive, toExclusive, msg], freshDiagnostic);
        diagnostics.push(freshDiagnostic);
    }

    function bindDeclarationFile(sourceFile: SourceFile) {
        sourceFile.containedScope = {
            parentContainer: null,
            typeinfo: typeinfo(),
            __declaration: new Map<string, SymTabEntry>()
        };

        for (const node of sourceFile.content) {
            if (node.kind === NodeKind.typeShim) {
                if (node.type.kind === TypeKind.functionSignature) {
                    addFreshSymbolToTable(sourceFile.containedScope!.__declaration!, node.type.uiName, node, node.type);
                }
                else if (node.type.kind === TypeKind.interface) {
                    if (!sourceFile.containedScope.typeinfo.interfaces.has(node.type.name)) {
                        sourceFile.containedScope.typeinfo.interfaces.set(node.type.name, []);
                    }
                    sourceFile.containedScope.typeinfo.interfaces.get(node.type.name)!.push(node.type);
                }
            }
            else {
                issueDiagnosticAtRange(node.range, "Illegal non-declaration in declaration file.");
                continue;
            }
        }

        bindTypeAndInterfacedefsForContainer(sourceFile);
    }

    function bindDestructuringRoot(node: DestructuredRecord | DestructuredList) {
        if (engineVersion.engine === Engine["Lucee"]) {
            issueDiagnosticAtRange(node.range, "Lucee does not support destructuring assignments.");
        }
        else if (engineVersion.engine === Engine["Adobe"] && engineVersion.semver.compare(EngineVersions["acf.2021"].semver) === -1) {
            issueDiagnosticAtRange(node.range, `Adobe supports destructuring assignments starting in version ${EngineVersions["acf.2021"].uiString}. cfls is currently configured for ${engineVersion.uiString}.`);
        }

        const restCandidates = (() => {
            switch (node.kind) {
                case NodeKind.destructuredList: {
                    return node.elements;
                }
                case NodeKind.destructuredRecord: {
                    // maybe a TS bug here, node.elements.filter(isDestructuredElement) doesn't apply the type guard?
                    const restCandidates : DestructuredElement[] = []
                    for (const element of node.elements) {
                        if (isDestructuredElement(element)) {
                            restCandidates.push(element);
                        }
                    }
                    return restCandidates;
                }
                default: exhaustiveCaseGuard(node);
            }

            function isDestructuredElement(v: Node) : v is DestructuredElement {
                return v.kind === NodeKind.destructuredElement;
            }
        })();
        
        maybeIssueDiagnosticsOnTooManySpreads(restCandidates);

        node.elements.forEach((next: Node) => bindNode(next, node));

        function maybeIssueDiagnosticsOnTooManySpreads(elements: DestructuredElement[]) : void {
            let totalRestElements = 0;
            for (const element of elements) {
                if (element.elementType === DestructuredElementType.rest) {
                    totalRestElements++;
                    if (totalRestElements > 1) {
                        issueDiagnosticAtRange(element.range, "A destructuring assignment should have exactly zero or one '...rest' binding.");
                    }
                }
            }
        }
    }

    function bindMaybeDestructuringLeaf(node: DestructuredElement | DestructuredRecordElement) {
        switch (node.kind) {
            case NodeKind.destructuredElement: {
                switch (node.elementType) {
                    case DestructuredElementType.common: {
                        if (node.value.kind === NodeKind.identifier) {
                            maybeAddSymbolToLocalScope(node.value);
                        }
                        else {
                            bindNode(node.value, node);
                        }
                        return;
                    }
                    case DestructuredElementType.rest: {
                        if (node.parent?.kind === NodeKind.destructuredList) {
                            // check it is "last" postition (for lists)
                            if (node.parent.elements.indexOf(node) !== node.parent.elements.length - 1) {
                                issueDiagnosticAtRange(node.range, "An array '...rest' binding should always be the last binding in the destructuring list.");
                            }
                        }
                        else if (node.parent?.kind === NodeKind.destructuredRecordElement) {
                            // check that we are not on the right side of a rebinding (for records)
                            // {x: ...bad} parses, but is non-sensical
                            issueDiagnosticAtRange(node.range, "An object '...rest' binding cannot be on the right-hand side of a named object property.");
                        }
                        else {
                            // no-op
                        }

                        maybeAddSymbolToLocalScope(node.value);

                        return;
                    }
                    default: exhaustiveCaseGuard(node);
                }
            }
            case NodeKind.destructuredRecordElement: {
                if (node.value.type === DestructuredRecordElementKind.bare) {
                    maybeAddSymbolToLocalScope(node.value.value.value);
                }
                else {
                    bindNode(node.value.value, node);
                }
                return;
            }
            default: exhaustiveCaseGuard(node);
        }

        function maybeAddSymbolToLocalScope(identifier: Identifier) : void {
            if (getContainingFunction(node)) {
                addFreshSymbolToTable(currentContainer.containedScope.local!, identifier.uiName || "<<name?>>", node);
            }
            else {
                // the diagnostic here would be "can't use a destructuring expression in top-level scope",
                // but we have to be in a var declaration, so the var declaration binder should handle that
            }
        }
    }

    function checkIdentifierValidity(node: Node) : void {
        const defaultMsg = (nameLike: {uiName: string | undefined, canonicalName: string | undefined}) => `'${nameLike.uiName || nameLike.canonicalName}' cannot be used as an identifier in this position.`;
        switch (node.kind) {
            case NodeKind.identifier: {
                switch (node.canonicalName) {
                    case "final": {
                        if (isNamedFunctionArgumentName(node) || isObjectLiteralPropertyName(node)) {
                            // ok as a named arg name and object property name
                            break;
                        }
                        else if (engineVersion.engine === Engine.Adobe) {
                            // invalid as an identifier, both tag and script
                            issueDiagnosticAtRange(node.range, defaultMsg(node));
                        }
                        break;
                    }
                    case "not": {
                        if (isObjectLiteralPropertyName(node)) {
                            // x = {not: 0} fails on both lucee and acf
                            issueDiagnosticAtRange(node.range, "The identifier 'not' cannot be used to define an object property name. Consider quote-escaping it.");
                            break;
                        }

                        if (engineVersion.engine === Engine.Adobe) {
                            issueDiagnosticAtRange(node.range, defaultMsg(node));
                            break;
                        }
                    }
                    case "break":
                    case "case":
                    case "catch":
                    case "continue":
                    case "default":
                    case "do":
                    case "else":
                    case "false":
                    case "finally":
                    case "for":
                    case "function":
                    case "if":
                    case "import":
                    case "new":
                    case "return":
                    case "switch":
                    case "true":
                    case "try":
                    case "var":
                    case "while": {
                        if (node.canonicalName === "function" && node.parent?.kind === NodeKind.functionDefinition) {
                            issueDiagnosticAtRange(node.range, defaultMsg(node));
                            break;
                        }
                        else if (engineVersion.engine === Engine.Adobe && !isNamedFunctionArgumentName(node) && !isObjectLiteralPropertyName(node) && isInScriptBlock(node)) {
                            issueDiagnosticAtRange(node.range, defaultMsg(node));
                            break;
                        }
                    }
                }
            }
            default: break;
        }
    }

    function issueDiagnosticAtRange(range: SourceRange, msg: string, kind = DiagnosticKind.error) : void {
        issueDiagnosticAtSpan(range.fromInclusive, range.toExclusive, msg, kind);
    }

    return { bind };
}

export type Binder = ReturnType<typeof Binder>;
