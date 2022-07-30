/**
 * The languageTool accepts requests (our own, limited set of request types, not the LSP request types) and processes
 * them, adding/dropping files from a project, checking files, finding completions, etc.
 */
import type { IREPLACED_AT_BUILD } from "./buildShim";
import { Project, FileSystem } from "../compiler/project"
import { ClientAdapter } from "../services/clientAdapter";
import { CancellationTokenConsumer } from "../compiler/cancellationToken";
import { CflsRequest, CflsRequestType, CflsResponse, CflsResponseType, CflsConfig, InitArgs, SerializableCflsConfig } from "./cflsTypes";
import * as Completions from "./completions";
import { exhaustiveCaseGuard, getAttribute, getSourceFile } from "../compiler/utils";
import { BinaryOperator, BinaryOpType, BlockType, FunctionDefinition, NodeKind, Property, SourceFile } from "../compiler/node";
import { SourceRange } from "../compiler/scanner";
import { EngineVersions } from "../compiler/engines";
import { TypeKind } from "../compiler/types";

declare const REPLACED_AT_BUILD : IREPLACED_AT_BUILD;

function send(msg: CflsResponse) {
    process.send!(msg);
}

const languageTool = LanguageTool();

const NO_DATA = undefined;
const CANCELLED = null;

type Result<T> = typeof NO_DATA | typeof CANCELLED | T;

process.on("message", (msg: CflsRequest) => {
    let response : CflsResponse | undefined = undefined;
    switch (msg.type) {
        case CflsRequestType.init: {
            languageTool.init(msg.initArgs);
            response = {type: CflsResponseType.initialized, id: msg.id};
            break;
        }
        case CflsRequestType.diagnostics: {
            const diagnostics = languageTool.naiveGetDiagnostics(msg.fsPath, msg.freshText);
            if (diagnostics === NO_DATA) {
                break;
            }
            else if (diagnostics === CANCELLED) {
                response = {type: CflsResponseType.cancelled, id: msg.id};
            }
            else {
                response = {
                    type: CflsResponseType.diagnostics,
                    id: msg.id,
                    fsPath: diagnostics.fsPath,
                    diagnostics: diagnostics.diagnostics,
                    affectedDependents: diagnostics.affectedDependents,
                    batch: msg.batch
                };
            }
            break;
        }
        case CflsRequestType.completions: {
            const completions = languageTool.getCompletions(msg.fsPath, msg.targetIndex, msg.triggerCharacter);
            if (completions === NO_DATA) {
                break;
            }
            else if (completions === CANCELLED) {
                response = {type: CflsResponseType.cancelled, id: msg.id};
                break;
            }
            else {
                response = {type: CflsResponseType.completions, id: msg.id, fsPath: completions.fsPath, completionItems: completions.completionItems};
                break;
            }
        }
        case CflsRequestType.reset: {
            languageTool.reset(msg.config);
            break;
        }
        case CflsRequestType.definitionLocations: {
            const locations = languageTool.getDefinitionLocations(msg.fsPath, msg.targetIndex);
            if (locations === NO_DATA) {
                response = {type: CflsResponseType.definitionLocations, id: msg.id, locations: []}
            }
            else if (locations === CANCELLED) {
                response = {type: CflsResponseType.cancelled, id: msg.id};
                break;
            }
            else {
                response = {type: CflsResponseType.definitionLocations, id: msg.id, locations}
            }
            break;
        }
        case CflsRequestType.track: {
            languageTool.track(msg.fsPath);
            response = {type:CflsResponseType.track, id: msg.id};
            break;
        }
        case CflsRequestType.untrack: {
            languageTool.untrack(msg.fsPath);
            response = {type:CflsResponseType.untrack, id: msg.id};
            break;
        }
        default: {
            exhaustiveCaseGuard(msg);
        }
    }

    if (response) {
        send(response);
    }
});

type AbsPath = string;

function getClientAdapter() : ClientAdapter {
    // adapter module is expected to `export const adapter : ClientAdapter = ...`
    return require(REPLACED_AT_BUILD.ClientAdapterModule_StaticRequirePath).adapter;
}

function LanguageTool() {
    let config! : CflsConfig;
    let workspaceProjects! : Map<AbsPath, Project>;
    let workspaceRoots! : AbsPath[];
    let clientAdapter!: ClientAdapter;
    let cancellationToken: CancellationTokenConsumer;

    // we hold strong references to files we're tracking, otherwise they will be GC'd
    // a file open in the editor that has no dependents will have no strong references,
    // unless we take that strong reference; the Project only holds weakrefs to SourceFiles,
    // SourceFiles hold strong references to their immediate dependencies
    const trackedFiles = new Map<string, SourceFile | "PENDING-FIRST-LOAD">();

    function init(initArgs : InitArgs) {
        workspaceProjects = new Map();
        workspaceRoots = initArgs.workspaceRoots;
        clientAdapter = getClientAdapter();
        cancellationToken = CancellationTokenConsumer(initArgs.cancellationTokenId)
        
        reset(initArgs.config);
    }

    function getOwningProjectFromAbsPath(absPath: AbsPath) : Project | undefined {
        for (const [workspaceRoot, project] of workspaceProjects) {
            const effectiveAbsPath = project.canonicalizePath(absPath);
            if (effectiveAbsPath.startsWith(workspaceRoot)) {
                return project;
            }
        }
        return undefined;
    }

    /**
     * This method is assumed to be called on every document update event, so we infer "ah this source file changed" by being called
     * freshText of `null` means the text wasn't updated
     */
    function naiveGetDiagnostics(fsPath: AbsPath, freshText: string | Buffer | null) : Result<{sourceFile: SourceFile, fsPath: AbsPath, diagnostics: unknown[], affectedDependents: AbsPath[]}> {
        const project = getOwningProjectFromAbsPath(fsPath);
        if (!project) return NO_DATA;

        //
        // perhaps crytpically, `checking` updates sourcefile dependency information
        //
        if (freshText !== null) {
            /*const timing =*/ project.parseBindCheck(fsPath, freshText);
        }
        else {
            project.recheck(fsPath);
        }

        // does this leave the sourcefile in a possibly corrupt state? like we cleared it out ("reset in place"), but then hit a cancellation,
        // so it's half parsed, and not binded or checked? (there's no check cancellation token in binder / checker, only the parser)
        if (cancellationToken.cancellationRequested()) {
            return CANCELLED;
        }
        //connection.console.info(`${fsPath}\n\tparse ${timing.parse} // bind ${timing.bind} // check ${timing.check}`);

        const diagnostics = project.getDiagnostics(fsPath) ?? [];
        const sourceFile = project.getParsedSourceFile(fsPath) ?? null;
        if (!sourceFile) return NO_DATA;

        const shouldTrack = trackedFiles.get(fsPath);
        if (shouldTrack === "PENDING-FIRST-LOAD") {
            trackedFiles.set(fsPath, sourceFile);
        }

        return {
            sourceFile,
            fsPath,
            diagnostics: diagnostics.map((diagnostic) => clientAdapter.diagnostic(sourceFile.scanner.getAnnotatedChar, diagnostic)),
            affectedDependents: project.getTransitiveDependents(sourceFile).map(sourceFile => sourceFile.absPath)
        }
    }

    function getCompletions(fsPath: AbsPath, targetIndex: number, triggerCharacter: string | null) : Result<{fsPath: AbsPath, completionItems: unknown[]}> {
        const project = getOwningProjectFromAbsPath(fsPath);
        if (!project) return NO_DATA;

        const file = project.getParsedSourceFile(fsPath);
        if (!file) return NO_DATA;

        const completions = Completions.getCompletions(
        	project,
        	fsPath,
        	targetIndex,
        	triggerCharacter);

        return {
            fsPath,
            completionItems: completions.map((completionItem) => clientAdapter.completionItem(file.scanner.getAnnotatedChar, completionItem))
        };
    }

    const exactlyFirstCharRange = new SourceRange(0,0);
    type SourceLocation = {sourceFile: SourceFile, range: SourceRange};
    
    function getDefinitionLocations(fsPath: AbsPath, targetIndex: number) : Result<unknown[]> { // fixme: this isn't cancellable, so Result<T> is unnecessary?
        const result = __getDefinitionLocations();
        return result?.map(location => clientAdapter.sourceLocation(location.sourceFile.scanner.getAnnotatedChar, location.sourceFile.absPath, location.range));

        function __getDefinitionLocations() { // fixme: this isn't cancellable, so Result<T> is unnecessary?
            const project = getOwningProjectFromAbsPath(fsPath);
            if (!project) return undefined;

            const sourceFile = project.__unsafe_dev_getFile(fsPath);
            if (!sourceFile) return undefined;

            const targetNode = project.getInterestingNodeToLeftOfCursor(fsPath, targetIndex);
            if (!targetNode) return undefined;

            const checker = project.__unsafe_dev_getChecker();

            // someType function foo() {}
            // ^^^^^^^^
            if (targetNode.parent?.kind === NodeKind.functionDefinition && !targetNode.parent.fromTag && targetNode.parent.returnType === targetNode) {
                const symbol = checker.getSymbol(targetNode.parent, sourceFile)
                if (symbol && symbol.symTabEntry.lexicalType?.kind === TypeKind.functionSignature && symbol.symTabEntry.lexicalType.returns.kind === TypeKind.cfc) {
                    return [{
                        sourceFile: symbol.symTabEntry.lexicalType.returns.cfc,
                        range: exactlyFirstCharRange,
                    }];
                }
                return undefined;
            }

            if (targetNode.kind === NodeKind.simpleStringLiteral) {
                if (targetNode.parent?.kind === NodeKind.tagAttribute && targetNode.parent.canonicalName === "extends") {
                    if (targetNode.parent?.parent?.kind === NodeKind.block
                            && targetNode.parent.parent.subType === BlockType.scriptSugaredTagCallBlock
                            && targetNode.parent.parent.name?.token.text.toLowerCase() === "component") {
                            // component extends="..."
                            //                   ^^^^^
                            if (sourceFile.cfc?.extends) {
                                return [{
                                    sourceFile: sourceFile.cfc.extends,
                                    range: exactlyFirstCharRange,
                                }]
                            }

                    }
                }
                return undefined;
            }

            const newExpr = targetNode.kind === NodeKind.dottedPathRest
                && targetNode.parent?.kind === NodeKind.dottedPath
                && targetNode.parent.parent?.kind === NodeKind.callExpression
                && targetNode.parent.parent.parent?.kind === NodeKind.new
                ? targetNode.parent.parent.parent
                : targetNode.kind === NodeKind.dottedPath
                && targetNode.parent?.kind === NodeKind.callExpression
                && targetNode.parent.parent?.kind === NodeKind.new
                ? targetNode.parent.parent
                : undefined;

            if (newExpr) {
                const type = checker.getCachedEvaluatedNodeType(newExpr, sourceFile);
                if (type && type.kind === TypeKind.cfc) {
                    return [{
                        sourceFile: type.cfc,
                        range: exactlyFirstCharRange
                    }]
                }
                return undefined;
            }

            const symbol = checker.getSymbol(targetNode, sourceFile);
            if (!symbol || !symbol.symTabEntry.declarations) return undefined;

            const result : SourceLocation[] = [];

            for (const decl of symbol.symTabEntry.declarations) {
                const location = decl.kind === NodeKind.property
                    ? getPropertyDefinitionLocation(decl)
                    : decl.kind === NodeKind.functionDefinition
                    ? getFunctionDefinitionLocation(decl)
                    : decl.kind === NodeKind.binaryOperator
                    ? getAssignmentLocation(decl)
                    : undefined;

                if (!location) continue;

                result.push(location);
            }

            return result;

            function getAssignmentLocation(node: BinaryOperator) : SourceLocation | undefined {
                if (node.optype !== BinaryOpType.assign) return undefined;
                const declSourceFile = getSourceFile(node);
                if (!declSourceFile) return undefined;
                return {
                    sourceFile: declSourceFile,
                    range: node.left.range
                }
            }

            function getFunctionDefinitionLocation(node: FunctionDefinition) : SourceLocation | undefined {
                const sourceFile = getSourceFile(node);
                if (!sourceFile) return undefined;
                if (node.fromTag) {
                    if (!node.tagOrigin?.startTag?.range) return undefined;
                    return {
                        sourceFile,
                        range: node.tagOrigin.startTag.range
                    }
                }
                else {
                    if (!node.nameToken) return undefined;
                    return {
                        sourceFile,
                        range: node.nameToken.range
                    }
                }
            }

            function getPropertyDefinitionLocation(node: Property) : SourceLocation | undefined {
                const sourceFile = getSourceFile(node);
                if (!sourceFile) return undefined;
                if (node.fromTag) {
                    return {
                        sourceFile,
                        range: node.range
                    }
                }
                else {
                    const nameAttr = getAttribute(node.attrs, "name");
                    if (!nameAttr) return undefined;
                    return {
                        sourceFile,
                        range: nameAttr.name.range
                    }
                }
            }
        }
    }

    function reset(freshConfig: SerializableCflsConfig) {
        config = {...freshConfig, engineVersion: EngineVersions[freshConfig.engineVersion]};
        const fileSystem = FileSystem();
        
        workspaceProjects.clear();
        
        for (const workspace of workspaceRoots) {
            const rootAbsPath = fileSystem.caseSensitive ? workspace : workspace.toLowerCase();
            const project = Project(
                rootAbsPath,
                fileSystem,
                {
                    parseTypes: config.x_parseTypes,
                    debug: REPLACED_AT_BUILD.debug,
                    engineVersion: config.engineVersion,
                    withWireboxResolution: config.wireboxResolution,
                    cfConfigProjectRelativePath: config.cfConfigProjectRelativePath,
                    checkReturnTypes: config.x_checkReturnTypes,
                    checkFlowTypes: config.x_checkFlowTypes,
                    genericFunctionInference: config.x_genericFunctionInference,
                    cancellationToken
                },
            );

            if (config.engineLibAbsPath) {
                project.addEngineLib(config.engineLibAbsPath);
            }

            workspaceProjects.set(rootAbsPath, project);
        }
    }

    function track(fsPath: string) : void {
        const maybeAlreadyTracked = trackedFiles.get(fsPath);
        if (maybeAlreadyTracked) {
            return;
        }
        trackedFiles.set(fsPath, "PENDING-FIRST-LOAD");
    }

    function untrack(fsPath: string) : void {
        trackedFiles.delete(fsPath);
    }

    return {
        naiveGetDiagnostics,
        init,
        reset,
        getCompletions,
        getDefinitionLocations,
        track,
        untrack
    }
}