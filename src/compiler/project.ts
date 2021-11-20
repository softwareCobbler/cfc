import * as fs from "fs";
import * as path from "path";

import { Binder } from "./binder";
import { Checker } from "./checker";
import { EngineVersion } from "./engines";
import { BlockType, CallExpression, mergeRanges, Node, NodeId, NodeKind, SourceFile, StatementType, SymTabEntry, setDebug as setNodeFactoryDebug, DiagnosticKind } from "./node";
import { Parser } from "./parser";
import { CfFileType, SourceRange } from "./scanner";
import { CfcTypeWrapper, cfFunctionOverloadSet, cfFunctionSignatureParam, Interface, createLiteralType, _Type, Struct } from "./types";

import { cfmOrCfc, findNodeInFlatSourceMap, flattenTree, getAttributeValue, getComponentAttrs, getComponentBlock, getTriviallyComputableString, NodeSourceMap, visit } from "./utils";

import { CancellationException, CancellationTokenConsumer } from "./cancellationToken";

interface CachedFile {
    parsedSourceFile: SourceFile,
    flatTree: NodeSourceMap[],
    nodeMap: ReadonlyMap<NodeId, Node>
}

interface DevTimingInfo {
    parse: number,
    bind: number,
    check: number
}

export interface FileSystem {
    readFileSync: (path: string) => Buffer,
    readdirSync: (root: string) => fs.Dirent[],
    existsSync: (path: string) => boolean,
    lstatSync: (path: string) => {isFile: () => boolean, isDirectory: () => boolean},
    join: (...args: string[]) => string,
    normalize: (path: string) => string,
    pathSep: string,
    caseSensitive: boolean,
}

function swapAsciiCase(s: string) {
    const result : string[] = [];
    for (let i = 0; i < s.length; i++) {
        const c = s.charCodeAt(i);
        if (65 <= c && c <= 90) result.push(String.fromCharCode(c + 32)); // upper to lower
        else if (97 <= c && c <= 122) result.push(String.fromCharCode(c - 32)); // lower to upper
        else result.push(s[i]);
    }
    return result.join("");
}

export function FileSystem() : FileSystem {
    return {
        readFileSync: fs.readFileSync,
        readdirSync: (root: string) => fs.readdirSync(root, {withFileTypes: true}),
        existsSync: fs.existsSync,
        lstatSync: (path: string) => fs.lstatSync(path),
        join: path.join,
        normalize: path.normalize,
        pathSep: path.sep,
        caseSensitive: !fs.existsSync(swapAsciiCase(__filename)), // if this file exists when we ask for it with a different case, then we are not case sensitive
    }
}

const nativeSepPattern = /[\\/]/g;
export class DebugDirent extends fs.Dirent {
    constructor(
        public name: string,
        private _isDir: boolean,
        private _isFile: boolean,
    ) {
        super();
    }
    isFile() { return this._isFile; }
    isDirectory() { return this._isDir; }
    isBlockDevice() { return false; }
    isCharacterDevice() { return false; }
    isSymbolicLink() { return false; }
    isFIFO() { return false; }
    isSocket() { return false; }
}

export type FileSystemNode = {[dir: string]: string | Buffer | FileSystemNode}; // just for dev/debug

export function pushFsNode(fsNode: FileSystemNode, path: string, text: string | Buffer, pathSep = "/") { // just for dev/debug
    let workingNode : string | Buffer | FileSystemNode | undefined;

    if (path.startsWith(pathSep)) {
        path = path.slice(1);
        workingNode = fsNode[pathSep];
    }
    else {
        workingNode = fsNode;
    }

    const splitPath = path.split(pathSep);

    for (let i = 0; i < splitPath.length - 1; i++) {
        if (!workingNode || isLeaf(workingNode)) throw "bad path - " + path;
        const name = splitPath[i];
        
        if (!workingNode[name]) {
            workingNode[name] = {};
        }
        workingNode = workingNode[name];
    }

    if (!workingNode || isLeaf(workingNode)) throw "bad path - " + path;

    workingNode[splitPath[splitPath.length - 1]] = text;

    function isLeaf(v: string | Buffer | FileSystemNode) : v is string | Buffer {
        return (typeof v === "string") || (v instanceof Buffer);
    }
}

export function DebugFileSystem() : FileSystem;
export function DebugFileSystem(files: Readonly<FileSystemNode>, pathSepOfDebugFs?: string, isCaseSensitive?: boolean) : FileSystem;
export function DebugFileSystem(files?: Readonly<FileSystemNode>, pathSepOfDebugFs = "/", isCaseSensitive = true) : FileSystem {
    if (!files) files = {"/": {}};
    const maybeGet = (path: string) => {
        // we expect fileSystemPath to be rooted on "/" or "\"
        const pathComponents = [
            pathSepOfDebugFs,
            ...(path.split(pathSepOfDebugFs).filter(s => s !== ""))
        ];
        let working = files;
        while (pathComponents.length > 0) {
            const next = working?.[pathComponents.shift()!];
            if (!next) {
                return undefined;
            }
            else if (typeof next === "string" || next instanceof Buffer) {
                return next;
            }
            else {
                working = next;
            }
        }
        return working;
    }

    function isFile(v: string | Buffer | FileSystemNode | undefined) : v is string | Buffer {
        return (typeof v === "string") || v instanceof Buffer;
    }

    function isDir(v: string | Buffer | FileSystemNode | undefined) : v is FileSystemNode {
        return !!v && !isFile(v);
    }

    return {
        readFileSync: (path: string) : Buffer => {
            const fsObj = maybeGet(path);
            if (!fsObj) throw `Bad file lookup for path '${path}'`;
            if (isDir(fsObj)) throw `readFileSync called on directory '${path}'`;
            return typeof fsObj === "string" ? Buffer.from(fsObj, "utf-8") : fsObj;
        },
        readdirSync: (root: string) => {
            const target = maybeGet(root);
            if (!isDir(target)) throw `readdirSync called on non-directory '${root}'`;
            const result : DebugDirent[] = [];
            for (const key of Object.keys(target)) {
                const fsObj = target[key];
                result.push(new DebugDirent(key, isDir(fsObj), isFile(fsObj)));
            }
            return result;
        },
        existsSync: (path: string) => !!maybeGet(path),
        lstatSync: (path: string) => {
            const fsObj = maybeGet(path);
            if (!fsObj) throw "lstatSync called on non-existent path";
            return new DebugDirent(path, isDir(fsObj), isFile(fsObj));
        },
        join: (...args: string[]) => {
            // join using path.join, then replace the platform pathSep with the debug path sep
            const t = path.join(...args);
            return t.replace(nativeSepPattern, pathSepOfDebugFs);
        },
        normalize: (_path: string) => { return _path; /* fixme */ },
        pathSep: pathSepOfDebugFs,
        caseSensitive: isCaseSensitive
    }
}

export interface ProjectOptions {
    debug: boolean,
    parseTypes: boolean,
    engineVersion: EngineVersion,
    withWireboxResolution: boolean,
    wireboxConfigFileCanonicalAbsPath: string | null,
    genericFunctionInference: boolean,
    checkReturnTypes: boolean,
    cancellationToken: CancellationTokenConsumer,
}

export function Project(__const__projectRoot: string, fileSystem: FileSystem, options: ProjectOptions) {
    type AbsPath = string;
    
    const projectRoot = canonicalizePath(__const__projectRoot);
    const projectRootDirName = (() => {
        const t = path.parse(projectRoot).base.split(fileSystem.pathSep);
        return t.length > 0 ? t[t.length - 1] : "";
    })();

    const parser = Parser(options);
    const binder = Binder(options);
    const checker = Checker(options);
    const heritageCircularityDetector = new Set<string>();

    if (options.debug) {
        setNodeFactoryDebug(true);
        // checker.setDebug(true);
    }

    checker.install({CfcResolver, EngineSymbolResolver, LibTypeResolver});

    type FileCache = Map<AbsPath, CachedFile>;
    const files : FileCache = new Map();

    let engineLib : CachedFile | undefined;
    let wireboxLib : SourceFile | null = null;

    function tryAddFile(absPath: string) : CachedFile | undefined {
        if (!fileSystem.existsSync(absPath)) return undefined;
        return addFile(absPath);
    }

    function parseBindCheckWorker(sourceFile: SourceFile) : DevTimingInfo {
        try {
            parser.setSourceFile(sourceFile);
            const parseStart = new Date().getTime();
            parser.parse();
            const parseElapsed = new Date().getTime() - parseStart;

            if (engineLib && engineLib.parsedSourceFile !== sourceFile) {
                sourceFile.libRefs.set("<<engine>>", engineLib.parsedSourceFile);
            }

            const bindStart = new Date().getTime();
            binder.bind(sourceFile);
            const bindElapsed = new Date().getTime() - bindStart;

            // (nodeMap is mapping from nodeId to node, helpful for flat list of terminal nodes back to their tree position)
            // note that if we ascend into a parent cfc, we'll run another bind, which would destroy
            // the node map as it is now unless we get a ref to it now
            // fix this api
            const nodeMap = binder.getNodeMap();
            
            maybeFollowParentComponents(sourceFile);

            // do before checking so resolving a self-referential cfc in the checker works
            // e.g. in foo.cfc `public foo function bar() { return this; }`
            files.set(canonicalizePath(sourceFile.absPath), {
                parsedSourceFile: sourceFile,
                flatTree: flattenTree(sourceFile),
                nodeMap
            });

            const checkStart = new Date().getTime();

            if (options.withWireboxResolution && sourceFile.absPath === options.wireboxConfigFileCanonicalAbsPath) {
                const wireboxInterface = constructWireboxInterface(sourceFile);
                if (wireboxInterface) {
                    if (!wireboxLib) wireboxLib = SourceFile("<<magic/wirebox>>", CfFileType.dCfm, "");
                    wireboxLib.containedScope.typedefs.mergedInterfaces.set("Wirebox", wireboxInterface);
                }
                else {
                    wireboxLib = null;
                }
            }

            if (options.withWireboxResolution && wireboxLib) {
                sourceFile.libRefs.set("<<magic/wirebox>>", wireboxLib);
            }
            else {
                sourceFile.libRefs.delete("<<magic/wirebox>>");
            }

            checker.check(sourceFile);
            const checkElapsed = new Date().getTime() - checkStart;

            return { parse: parseElapsed, bind: bindElapsed, check: checkElapsed };
        }
        catch (error) {
            // swallow CancellationExceptions, everything else gets rethrown
            if (error instanceof CancellationException) {
                return { parse: NaN, bind: NaN, check: NaN };
            }
            throw error;
        }
    }

    function addFile(absPath: string) {
        absPath = canonicalizePath(absPath);

        if (!fileSystem.existsSync(absPath)) return undefined;
        const alreadyExists = getCachedFile(absPath);
        if (alreadyExists) return alreadyExists;
        
        const fileType = cfmOrCfc(absPath);
        if (!fileType) return;
        const bytes = fileSystem.readFileSync(absPath);
        const sourceFile = SourceFile(absPath, fileType, bytes);

        parseBindCheckWorker(sourceFile);

        if (fileType === CfFileType.dCfm) {
            
        }

        return files.get(absPath)!;
    }

    function constructWireboxInterface(sourceFile: SourceFile) {
        const mappings = buildWireboxMappings(sourceFile);
        if (!mappings) {
            return undefined;
        }

        const overloads : {params: cfFunctionSignatureParam[], returns: _Type}[] = [];
        const mappingsBuilder = new Map<string, SymTabEntry>();
        for (const mapping of mappings) {
            if (mapping.kind === "dir") {
                const dirTarget = fileSystem.join(projectRoot, ...mapping.target.split("."));
                if (!fileSystem.existsSync(dirTarget) || !fileSystem.lstatSync(dirTarget).isDirectory()) continue;
                workDir(dirTarget);

                function workDir(absPath: string) {
                    const targets = fileSystem.readdirSync(absPath);
                    for (const target of targets) {
                        if (target.isSymbolicLink()) continue;
                        if (target.isDirectory()) {
                            workDir(fileSystem.join(absPath, target.name));
                            continue;
                        }
                        if (!target.isFile()) continue;
                        if (cfmOrCfc(target.name) !== CfFileType.cfc) continue;
                        const file = tryAddFile(fileSystem.join(absPath, target.name));
                        if (!file) continue;

                        const instantiableName = target.name.replace(/\.cfc$/i, "");
                        const instantiableNameAsLiteralType = createLiteralType(instantiableName);

                        const param = cfFunctionSignatureParam(/*required*/true, instantiableNameAsLiteralType, "name")
                        var cfcTypeWrapper = CfcTypeWrapper(file.parsedSourceFile);
                        overloads.push({params: [param], returns: cfcTypeWrapper});
                        const canonicalName = instantiableName.toLowerCase();
                        mappingsBuilder.set(canonicalName, {
                            canonicalName,
                            uiName: instantiableName,
                            type: cfcTypeWrapper,
                            declarations: null
                        });
                    }
                }
            }
        }

        const wireboxGetInstanceSymbol = {
            uiName: "getInstance",
            canonicalName: "getinstance",
            declarations: null,
            type: cfFunctionOverloadSet("getInstance", overloads, [])
        };

        const wireboxMembers = new Map<string, SymTabEntry>([
            ["getinstance", wireboxGetInstanceSymbol],
            ["mappings", {
                canonicalName: "mappings",
                uiName: "mappings",
                declarations: null,
                type: Struct(mappingsBuilder)
            }]
        ]);
        
        return Interface("Wirebox", wireboxMembers);
    }

    function addEngineLib(absPath: string) {
        engineLib = addFile(absPath);
    }

    function maybeFollowParentComponents(sourceFile: SourceFile) {
        function reportCircularity() {
            const msg = "Circularity in inheritance chain.";
            const componentBlock = getComponentBlock(sourceFile);
            if (componentBlock) {
                if (componentBlock.subType === BlockType.fromTag) errorAtNode(sourceFile, componentBlock.tagOrigin.startTag!, msg);
                else errorAtRange(sourceFile, mergeRanges(componentBlock.name, componentBlock.leftBrace), msg);
            }
        }

        if (sourceFile.cfFileType === CfFileType.cfc) {
            let maybeParent : CachedFile | undefined = undefined;

            // if there is a circularity, report it and don't put attach the parent to this cfc file
            if (heritageCircularityDetector.has(sourceFile.absPath)) {
                reportCircularity();
            }
            else {
                heritageCircularityDetector.add(sourceFile.absPath);
                maybeParent = tryGetParentComponent(sourceFile);
                if (maybeParent && maybeParent.parsedSourceFile.cfc?.extends && heritageCircularityDetector.has(maybeParent.parsedSourceFile.cfc.extends.absPath)) {
                    reportCircularity();
                    maybeParent = undefined;
                }
                heritageCircularityDetector.delete(sourceFile.absPath);
            }

            sourceFile.cfc = {extends: maybeParent?.parsedSourceFile ?? null, implements: []};
        }
    }

    /**
     * Convert some path or path component into its canonical form, which is identity
     * on case-sensitive systems, and all lower case on case-sensitive
     */
    function canonicalizePath<T extends string | null | undefined>(path: T) : T extends string ? string : undefined {
        if (!path) return undefined as any;
        return fileSystem.caseSensitive ? path : path.toLowerCase() as any;
    }

    function getCachedFile(absPath: string) : CachedFile | undefined {
        return files.get(canonicalizePath(absPath));
    }

    // fixme: dedup/unify this with the ones in parser/binder/checker
    function errorAtNode(sourceFile: SourceFile, node: Node, msg: string) {
        errorAtRange(sourceFile, node.range, msg);
    }

    function errorAtRange(sourceFile: SourceFile, range: SourceRange, msg: string) {
        sourceFile.diagnostics.push({
            kind: DiagnosticKind.error,
            fromInclusive: range.fromInclusive,
            toExclusive: range.toExclusive,
            msg: msg
        });
    }

    // this assumes work has already been done to load the parent file
    function tryGetParentComponent(sourceFile: SourceFile) : CachedFile | undefined {
        if (sourceFile.cfFileType !== CfFileType.cfc) return undefined;
        const heritageInfo = getExtendsSpecifier(sourceFile);
        if (!heritageInfo) return undefined;
        const {extendsSpecifier, extendsAttr} = heritageInfo;
        if (!extendsSpecifier) return undefined;
        const noSelfExtendsSpecifier = extendsSpecifier.filter(specifier => specifier.path !== sourceFile.absPath);
        if (extendsSpecifier.length !== noSelfExtendsSpecifier.length && noSelfExtendsSpecifier.length === 0) {
            errorAtNode(sourceFile, extendsAttr, "A component may not extend itself.");
            return undefined;
        }

        let result : CachedFile | undefined = undefined;
        for (const specifier of noSelfExtendsSpecifier) {
            result = getCachedFile(specifier.path) ?? tryAddFile(specifier.path);
            if (result) break;
        }

        return result;
    }

    function getExtendsSpecifier(sourceFile: SourceFile) {
        if (sourceFile.cfFileType !== CfFileType.cfc) return undefined;
        const attrs = getComponentAttrs(sourceFile);
        if (!attrs) return undefined;
        const heritage = getAttributeValue(attrs, "extends");
        if (!heritage) return undefined;
        const heritageLiteral = getTriviallyComputableString(heritage);
        if (!heritageLiteral) return undefined;
        return {
            extendsAttr: heritage,
            extendsSpecifier: buildPossibleCfcResolutionPaths(sourceFile.absPath, heritageLiteral)
        }
    }

    // for a file that should already be in cache;
    // if for some reason it isn't, we try to add it
    // as a dev kludge, we return parse/bind/check times; this method otherwise returns void
    function parseBindCheck(absPath: AbsPath, newSource: string | Buffer) : DevTimingInfo {
        const cache = getCachedFile(absPath);
        if (!cache) {
            tryAddFile(absPath);
            return {parse: -1, bind: -1, check: -1};
        }

        const sourceFile = cache.parsedSourceFile;
        sourceFile.resetInPlaceWithNewSource(newSource);

        return parseBindCheckWorker(sourceFile);
    }

    function getDiagnostics(absPath: string) {
        return files.get(canonicalizePath(absPath))?.parsedSourceFile.diagnostics || [];
    }

    let cfcDepth = 0;
    const cfcDepthLimit = 16;
    function CfcResolver(args: {resolveFrom: string, cfcName: string}) {
        if (cfcDepth > cfcDepthLimit) {
            console.log(`hit cfc resolution depth limit of ${cfcDepthLimit} resolving from ` + args.resolveFrom);
            return undefined;
        }
        try {
            cfcDepth++;
            const specifiers = buildPossibleCfcResolutionPaths(args.resolveFrom, args.cfcName);
            if (!specifiers) return undefined;
            for (const specifier of specifiers) {
                const file = getCachedFile(specifier.path)?.parsedSourceFile;
                if (file) {
                    return {
                        sourceFile: file,
                        symbolTable: file.containedScope.this || new Map()
                    }
                }
            }
            for (const specifier of specifiers) {
                const file =  tryAddFile(specifier.path)?.parsedSourceFile;
                if (file) {
                    return {
                        sourceFile: file,
                        symbolTable: file.containedScope.this || new Map()
                    }
                }
            }

            return undefined;
        }
        finally {
            cfcDepth--;
        }
    }

    function getNodeToLeftOfCursor(absPath: string, targetIndex: number) : Node | undefined {
		const docCache = getCachedFile(absPath);
		if (!docCache) return undefined;
		return findNodeInFlatSourceMap(docCache.flatTree, docCache.nodeMap, targetIndex);
    }

    /**
     * get node to left of cursor, but return undefined on text spans and comments, and jump from the terminal node to it's parent construct
     * so instead of a terminal, return Identifier, or etc.
     */
    function getInterestingNodeToLeftOfCursor(absPath: string, targetIndex: number) : Node | undefined {
		const docCache = getCachedFile(absPath);
		if (!docCache) return undefined;
		const node = findNodeInFlatSourceMap(docCache.flatTree, docCache.nodeMap, targetIndex);
        if (!node
            || node.kind === NodeKind.comment
            || (node.kind === NodeKind.textSpan
                && node.parent?.kind !== NodeKind.simpleStringLiteral
                && node.parent?.kind !== NodeKind.interpolatedStringLiteral)) return undefined;
        
        // climb from terminal into production, or from textSpan into string literal
        if (node.kind === NodeKind.terminal || node.kind === NodeKind.textSpan) return node.parent ?? undefined;

        return node;
    }

    function getParsedSourceFile(absPath: string) {
        return getCachedFile(absPath)?.parsedSourceFile || undefined;
    }

    function findColdboxParentModulesFolders(canonicalBase: string) : string[] {
        const parsedPath = path.parse(canonicalBase);
        const components = parsedPath.dir.split(fileSystem.pathSep);
        const result : string[] = [];

        while (components.length > 0 && components[components.length-1] !== "" /* a removed, leading path separator turned to "" on split */) {
            const workingPath = fileSystem.join(parsedPath.root, ...components);
            
            if (!workingPath.startsWith(projectRoot)) break; // should never happen ? i.e. we've climbed out of project root?
            
            const maybeModulesPath = fileSystem.join(workingPath, "modules");
            if (fileSystem.existsSync(maybeModulesPath)) {
                result.push(maybeModulesPath);
            }
            if (components[components.length-1] === "modules_app") { // we'll treat some/path/modules_app/ as a root
                result.push(workingPath);
            }

            components.pop();
        }

        return result;
    }

    function buildPossibleCfcResolutionPaths(resolveFrom: string, inCodeCfcName: string) : ComponentSpecifier[] | undefined {
        const cfcComponents = inCodeCfcName.split(".");
        if (cfcComponents.length === 0) return undefined; // just to not crash; why would this happen?
        cfcComponents[cfcComponents.length - 1] = cfcComponents[cfcComponents.length - 1] + ".cfc";

        const parsedResolveFrom = path.parse(canonicalizePath(resolveFrom));
        resolveFrom = parsedResolveFrom.dir;

        const parentModulesFolders = findColdboxParentModulesFolders(resolveFrom);
        const canonicalCfcName = canonicalizePath(inCodeCfcName);

        // given:
        //   - project root = '/root/proj/'
        //   - project root dirname = "proj"
        //   - while checking file = "/root/proj/someFolder/child/file.cfc"
        //   - resolveFrom = '/root/proj/someFolder/child'
        //   - inCodeCfcName = "proj.foo.bar"
        // we want to try to search at least the following paths:
        //   - /root/proj/foo/bar.cfc
        //   - /root/proj/proj/foo/bar.cfc
        const cfcNameStartsWithProjectRootDirName = canonicalCfcName.startsWith(projectRootDirName);

        const common = {
            canonicalName: canonicalCfcName,
            uiName: inCodeCfcName,
        } as const;

        const result = [{
            ...common,    
            path: fileSystem.join(resolveFrom, ...cfcComponents)
        }];

        // magic coldbox/wirebox resolution, might want something to toggle this
        for (const coldboxModulePath of parentModulesFolders) {
            result.push({
                ...common,
                path: path.join(coldboxModulePath, ...cfcComponents)
            });    
        }

        if (cfcNameStartsWithProjectRootDirName) {
            result.push({
                ...common,
                path: fileSystem.join(projectRoot, ...(cfcComponents.slice(1)))
            });
        }

        result.push({
            ...common,
            path: fileSystem.join(projectRoot, ...cfcComponents)
        });

        return result;
    }

    function EngineSymbolResolver(canonicalName: string) : SymTabEntry | undefined {
        if (!engineLib) return undefined;
        if (!engineLib.parsedSourceFile.containedScope.typedefs.mergedInterfaces.has("__cfEngine")) return undefined;
        return engineLib.parsedSourceFile.containedScope.typedefs.mergedInterfaces.get("__cfEngine")!.members.get(canonicalName);
    }

    // resolve types (right now, just interfaces) from *the* lib file
    function LibTypeResolver(name: string) : _Type | undefined {
        if (!engineLib) return undefined;
        return engineLib.parsedSourceFile.containedScope.typedefs.mergedInterfaces.get(name);
    }

    return {
        addFile,
        addEngineLib,
        parseBindCheck,
        getDiagnostics,
        getNodeToLeftOfCursor,
        getInterestingNodeToLeftOfCursor,
        getParsedSourceFile,
        getFileListing: () => [...files.keys()],
        __unsafe_dev_getChecker: () => checker,
        __unsafe_dev_getFile: (fname: string) => files.get(canonicalizePath(fname))
    }
}

export type Project = ReturnType<typeof Project>;
export type CfcResolver = (args: {resolveFrom: string, cfcName: string}) => {sourceFile: SourceFile, symbolTable: ReadonlyMap<string, SymTabEntry>} | undefined;
export type EngineSymbolResolver = (name: string) => SymTabEntry | undefined;
export type LibTypeResolver = (name: string) => _Type | undefined;

export interface ComponentSpecifier {
    canonicalName: string,
    uiName: string,
    path: string,
}

interface WireboxMappingDef {
    kind: "file" | "dir",
    target: string,
}
export function buildWireboxMappings(root: SourceFile) {
    const mappings : WireboxMappingDef[] = [];
    const CONTINUE_VISITOR_DESCENT = false;

    const component = getComponentBlock(root);

    if (!component) return undefined;
    for (const node of component.stmtList) {
        // find the `configure` function, and descend into it, to extract mapping definitions
        if (node.kind === NodeKind.functionDefinition && !node.fromTag && node.canonicalName === "configure") {
            visit(node.body.stmtList, wireboxConfigureFunctionMappingExtractingVisitor);
            break;
        }
    }

    return mappings;

    function wireboxConfigureFunctionMappingExtractingVisitor(node: Node | null | undefined) : boolean {
        if (!node) return CONTINUE_VISITOR_DESCENT;
        // when we hit a top-level call expression
        // (from the current context, top-level should be "within the configure() definition", i.e.
        // configure() { /* here */ } )
        // visit it, possibly extracting a mapping definition
        if (node.kind === NodeKind.statement && node.subType === StatementType.expressionWrapper && node.expr?.kind === NodeKind.callExpression) {
            const mapping = tryMapOne(node.expr);
            if (mapping) mappings.push(mapping);
        }
        // keep going, find additional mappings
        return CONTINUE_VISITOR_DESCENT;
    }

    function tryMapOne(node: CallExpression) : WireboxMappingDef | undefined {
        let result : {kind: "file" | "dir", target: string} | undefined;
        function tryMapWorker(node: Node) {
            // recursive base case: hit bottom of call chain; if we don't get a mapper we're done
            if (node.kind === NodeKind.callExpression && node.left.kind === NodeKind.identifier) {
                let kind : "file" | "dir";
                if (node.left.canonicalName === "map") kind = "file";
                else if (node.left.canonicalName === "mapdirectory") kind = "dir";
                else return false;

                if (node.args.length === 0) return false;
                const target = getTriviallyComputableString(node.args[0].expr);
                if (!target) return false;
                result = {kind, target: target};
                return true;
            }
            else if (node.kind === NodeKind.callExpression && node.left.kind === NodeKind.indexedAccess && node.left.accessElements.length === 1) {
                if (!tryMapWorker(node.left.root)) return false;
                // here we could get "to" and etc.
                return true;
            }

            return false;
        }

        tryMapWorker(node);
        return result;
    }
}