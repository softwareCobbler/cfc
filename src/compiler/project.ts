import * as fs from "fs";
import * as path from "path";

import { Binder } from "./binder";
import { Checker } from "./checker";
import { EngineVersion } from "./engines";
import { BlockType, mergeRanges, Node, NodeKind, SourceFile, SymTabEntry, DiagnosticKind, resetSourceFileInPlace, NodeFlags, restoreSourceFileInPlace } from "./node";
import { Parser } from "./parser";
import { CfFileType, SourceRange } from "./scanner";
import { Interface, Type, createLiteralType, Struct } from "./types";

import { cfmOrCfc, findNodeInFlatSourceMap, flattenTree, getAttributeValue, getComponentAttrs, getComponentBlock, getTriviallyComputableString } from "./utils";

import { CancellationException, CancellationTokenConsumer } from "./cancellationToken";

import { setDebug as setNodeModuleDebug } from "./node";
import { setDebug as setTypeModuleDebug } from "./types";
import { Diagnostic } from ".";

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

/**
 * file system that mostly just wraps `fs`
 */
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
    cfConfigProjectRelativePath: string | null,
    genericFunctionInference: boolean,
    checkReturnTypes: boolean,
    checkFlowTypes: boolean,
    cancellationToken: CancellationTokenConsumer,
}

export function Project(__const__projectRoot: string, fileSystem: FileSystem, options: ProjectOptions) {
    type AbsPath = string;
    type Trie<T> = Map<T, Trie<T> | string>;
    interface ProjectMappings {
        cf: Trie<string>,
        wirebox?: Map<string, string>
    }

    if (options.debug) {
        setNodeModuleDebug();
        setTypeModuleDebug();
    }

    const projectRoot = canonicalizePath(__const__projectRoot); // the value passed in is inconvenient to name; the convenient name is immutable
    // project root abs path: "a/b/c"
    // project root dir name: "c"
    const projectRootDirName = (() => {
        const t = path.parse(projectRoot).base.split(fileSystem.pathSep);
        return t.length > 0 ? t[t.length - 1] : "";
    })();
    
    const ProjectMappings : ProjectMappings = loadPathMappingsFromDisk(path.join(projectRoot, options.cfConfigProjectRelativePath ?? "cfconfig.json"));
    const wireboxLib : SourceFile | undefined = constructWireboxLibFile(ProjectMappings);

    const parser = Parser(options);
    const binder = Binder(options);
    const checker = Checker(options);
    const heritageCircularityDetector = new Set<string>();

    checker.install({CfcResolver, EngineSymbolResolver, LibTypeResolver});

    /**
     * We hold weak refs to files, and assume at most places that a source file doesn't exist (in which case we'll try to load it)
     * The SourceFiles themselves hold strong refs to their dependencies (in their `extends`/`implements` clauses and in general in their `directDependencies` property,
     * which holds references to, for example, the sourcefiles for "Foo" and "Bar" after we see `Foo function doThing(Bar arg1) {}`)
     */
    type FileCache = Map<AbsPath, WeakRef<SourceFile>>;
    const files : FileCache = new Map();

    /**
     * dependencies for some sourcefile
     * so `A imports B`, `B imports C`
     * directDependencies.get(A).has(B) -> true
     * directDependencies.get(A).has(C) -> false (not a direct dependency)
     */
    const directDependencies = new WeakMap<SourceFile, Set<SourceFile>>(); // need to iterate over values, maybe WeakRef<SourceFile>[] instead?
    /**
     * direct dependents of some sourcefile ()
     * so `A imports B`, `B imports C`
     * dependents.get(B).has(A) -> true
     * dependents.get(B).has(C) -> false (C does not depend on B)
     */
    const directDependents = new WeakMap<SourceFile, Set<SourceFile>>();

    let engineLib : SourceFile | undefined;

    // mappings file should be configurable, relative to project root
    function loadPathMappingsFromDisk(path: string) : ProjectMappings {
        try {
            const maybeJson = JSON.parse(fileSystem.readFileSync(path).toString());
            const cfMappings = new Map() as Trie<string>;

            if (isObject(maybeJson) && isObject(maybeJson.cf)) {
                outer:
                for (const [mapping, path] of Object.entries(maybeJson.cf)) {
                    if (typeof mapping === "string" && typeof path === "string") {
                        let trieWalker : Trie<string> = cfMappings;
                        const components = mapping.split(".");
                        if (components.length === 0) {
                            continue outer;
                        }

                        while (components.length > 1) {
                            const component = components.shift()!;
                            if (trieWalker.has(component)) {
                                const next = trieWalker.get(component)!;
                                if (next instanceof Map) {
                                    trieWalker = next;
                                }
                                else {
                                    // bad def, like a.b.c = foo, a.b = bar; can "b" be a leaf and a directory?
                                    continue outer;
                                }
                            }
                        }

                        if (trieWalker instanceof Map) {
                            trieWalker.set(components[0]!, path);
                        }
                    }
                }
            }

            if (options.withWireboxResolution) {
                const wireboxMappings = new Map<string, string>();
                if (isObject(maybeJson) && isObject(maybeJson.wirebox)) {
                    for (const [name, mapping] of Object.entries(maybeJson.wirebox)) {
                        if (typeof name === "string" && typeof mapping === "string") {
                            wireboxMappings.set(name, mapping);
                        }
                    }
                }
                return {
                    cf: cfMappings,
                    wirebox: wireboxMappings
                }
            }
            else {
                return { cf: cfMappings };
            }
        }
        catch {
            return { cf: new Map(), wirebox: new Map() };
        }

        function isObject(v: any) : v is {[k: string]: any} {
            return typeof v === "object" && v !== null;
        }
    }

    function tryAddFile(absPath: string) : SourceFile | undefined {
        if (!fileSystem.existsSync(absPath)) return undefined;
        return addFile(absPath);
    }

    function parseBindCheckWorker(
        sourceFile: SourceFile,
        oldDirectDependencies: Set<SourceFile>,
        restoreOnFailure?: SourceFile
    ) : {cancellationRequested: true} | {cancellationRequested: false, devTimingInfo: DevTimingInfo} {
        try {
            const parseStart = new Date().getTime();
            parser.parse(sourceFile);
            const parseElapsed = new Date().getTime() - parseStart;

            if (engineLib && engineLib !== sourceFile) {
                sourceFile.libRefs.set("<<engine>>", engineLib);
            }

            const bindStart = new Date().getTime();
            binder.bind(sourceFile);
            const bindElapsed = new Date().getTime() - bindStart;

            sourceFile.flatTree = flattenTree(sourceFile);

            // do before checking so resolving a self-referential cfc in the checker works
            // e.g. in foo.cfc `public foo function bar() { return this; }`
            files.set(canonicalizePath(sourceFile.absPath), new WeakRef(sourceFile));

            const checkStart = new Date().getTime();
            checkWorker(sourceFile, oldDirectDependencies);
            const checkElapsed = new Date().getTime() - checkStart;


            return { cancellationRequested: false, devTimingInfo: {parse: parseElapsed, bind: bindElapsed, check: checkElapsed} };
        }
        catch (error) {
            // swallow CancellationExceptions, everything else gets rethrown
            if (error instanceof CancellationException) {
                if (restoreOnFailure) {
                    // if we failed to parse, we need to restore everything about the file,
                    // otherwise references to it across the project have assumptions about it, at the very least,
                    // containedScope.this existing in the case of CFC files
                    // an alternative would be to always include an empty `this`? either way seems like it will produce not great results
                    restoreSourceFileInPlace(sourceFile, restoreOnFailure);
                }
                return { cancellationRequested: true }
            }
            throw error;
        }
    }

    function recheck(sourceFile: SourceFile) : void {
        // we don't want to "resetSourceFileInPlace"
        // we can leave all the content and binding and etc untouched,
        // we just want to recheck the existing tree assuming possibly changed dependencies
        const oldDirectDependencies = sourceFile.directDependencies;
        sourceFile.directDependencies = new Set();
        sourceFile.diagnostics = []; // ah ha, would need to clear "checker diagnostics" only ...

        // should we update librefs here?

        // clear all checker flags from nodes
        for (const node of sourceFile.nodeMap.values()) {
            node.flags &= ~(NodeFlags.checked | NodeFlags.checkerError | NodeFlags.unreachable);
        }
        checkWorker(sourceFile, oldDirectDependencies);
    }

    function checkWorker(sourceFile: SourceFile, oldDirectDependencies: Set<SourceFile>) {
        maybeFollowParentComponents(sourceFile); // should be in checker, resolves extends="..." attrs

        if (options.withWireboxResolution && wireboxLib) {
            sourceFile.libRefs.set("<<magic/wirebox>>", wireboxLib);
        }
        else {
            sourceFile.libRefs.delete("<<magic/wirebox>>");
        }

        checker.check(sourceFile);

        const {uniqueInL: depAdded, uniqueInR: depRemoved} = setDiff(sourceFile.directDependencies, oldDirectDependencies);

        for (const removed of depRemoved) {
            directDependencies.get(sourceFile)?.delete(removed);
            directDependents.get(removed)?.delete(sourceFile);
        }
        for (const added of depAdded) {
            addDependencyToWeakMap(directDependencies, sourceFile, added);
            addDependencyToWeakMap(directDependents, added, sourceFile);
        }
    }

    function addDependencyToWeakMap(v: WeakMap<SourceFile, Set<SourceFile>>, key: SourceFile, value: SourceFile) : void {
        const freshValue = v.get(key) ?? new Set<SourceFile>();
        freshValue.add(value);
        v.set(key, freshValue);
    }

    function getTransitiveDependencies(sourceFile: SourceFile) : SourceFile[] {
        const seen = new Set<SourceFile>();
        const dependencies : SourceFile[] = (() => {
            const v = directDependencies.get(sourceFile);
            if (v) {
                return [...v];
            }
            else {
                return [];
            }
        })();

        while (dependencies.length > 0) {
            const dep = dependencies[0];
            dependencies.shift();
            if (seen.has(dep)) {
                continue;
            }
            seen.add(dep);
            const next = directDependencies.get(dep);
            if (next) {
                dependencies.push(...next);
            }
        }

        return [...seen];
    }

    function getTransitiveDependents(sourceFile: SourceFile) : SourceFile[] {
        const seen = new Set<SourceFile>();
        const dependents : SourceFile[] = (() => {
            const v = directDependents.get(sourceFile);
            if (v) {
                return [...v];
            }
            else {
                return [];
            }
        })();

        while (dependents.length > 0) {
            const dep = dependents[0];
            dependents.shift();
            if (seen.has(dep)) {
                continue;
            }
            seen.add(dep);
            const next = directDependents.get(dep);
            if (next) {
                dependents.push(...next);
            }
        }

        return [...seen];
    }

    function addFile(absPath: string) : SourceFile | undefined {
        absPath = canonicalizePath(absPath);

        if (!fileSystem.existsSync(absPath)) return undefined;
        const alreadyExists = getCachedFile(absPath);
        if (alreadyExists) return alreadyExists;
        
        const fileType = cfmOrCfc(absPath);
        if (!fileType) return;
        const bytes = fileSystem.readFileSync(absPath);
        const sourceFile = SourceFile(absPath, fileType, bytes);

        const result = parseBindCheckWorker(sourceFile, new Set());

        if (result.cancellationRequested) {
            return undefined;
        }
        else {
            return sourceFile;
        }
    }

    // function constructWireboxInterface(sourceFile: SourceFile) : Interface | undefined {
    //     const mappings = buildWireboxMappings(sourceFile);
    //     if (!mappings) {
    //         return undefined;
    //     }

    //     const overloads : {params: cfFunctionSignatureParam[], returns: Type}[] = [];
    //     const mappingsBuilder = new Map<string, SymTabEntry>();
    //     for (const mapping of mappings) {
    //         if (mapping.kind === "dir") {
    //             const dirTarget = fileSystem.join(projectRoot, ...mapping.target.split("."));
    //             if (!fileSystem.existsSync(dirTarget) || !fileSystem.lstatSync(dirTarget).isDirectory()) continue;
    //             workDir(dirTarget);

    //             function workDir(absPath: string) {
    //                 const targets = fileSystem.readdirSync(absPath);
    //                 for (const target of targets) {
    //                     if (target.isSymbolicLink()) continue;
    //                     if (target.isDirectory()) {
    //                         workDir(fileSystem.join(absPath, target.name));
    //                         continue;
    //                     }
    //                     if (!target.isFile()) continue;
    //                     if (cfmOrCfc(target.name) !== CfFileType.cfc) continue;

    //                     const instantiableName = target.name.replace(/\.cfc$/i, "");
    //                     const instantiableNameAsLiteralType = createLiteralType(instantiableName);

    //                     const param = cfFunctionSignatureParam(/*required*/true, instantiableNameAsLiteralType, "name")
    //                     var cfcRefType = CfcLookup(instantiableNameAsLiteralType, ComponentSpecifier(fileSystem.join(absPath, target.name)));
    //                     overloads.push({params: [param], returns: cfcRefType});
    //                     const canonicalName = instantiableName.toLowerCase();
    //                     mappingsBuilder.set(canonicalName, {
    //                         canonicalName,
    //                         uiName: instantiableName,
    //                         type: cfcRefType,
    //                         declarations: null,
    //                         symbolId: -1,
    //                     });
    //                 }
    //             }
    //         }
    //     }

    //     const wireboxGetInstanceSymbol : SymTabEntry = {
    //         uiName: "getInstance",
    //         canonicalName: "getinstance",
    //         declarations: null,
    //         type: cfFunctionOverloadSet("getInstance", overloads, []),
    //         symbolId: -1,
    //     };

    //     const wireboxMembers = new Map<string, SymTabEntry>([
    //         ["getinstance", wireboxGetInstanceSymbol],
    //         ["mappings", {
    //             canonicalName: "mappings",
    //             uiName: "mappings",
    //             declarations: null,
    //             type: Struct(mappingsBuilder),
    //             symbolId: -1,
    //         }]
    //     ]);
        
    //     return Interface("Wirebox", wireboxMembers);
    // }

    function constructWireboxLibFile(mappings: ProjectMappings) : SourceFile | undefined {
        if (!mappings.wirebox) {
            return undefined;
        }

        const wireboxMappings = new Map<string, SymTabEntry>();

        for (const [name, mapping] of mappings.wirebox) {
            wireboxMappings.set(name, {
                flags: 0,
                canonicalName: name,
                uiName: name,
                declarations: null,
                lexicalType: undefined,
                effectivelyDeclaredType: createLiteralType(mapping),
                symbolId: -1,
            })
        }

        const wireboxMembers = new Map<string, SymTabEntry>([
            ["mappings", {
                flags: 0,
                canonicalName: "mappings",
                uiName: "mappings",
                declarations: null,
                lexicalType: undefined,
                effectivelyDeclaredType: Struct(wireboxMappings),
                symbolId: -1
            }],
        ]);

        /**
         * we create the following out of the json wirebox mapping file
         * @!interface Wirebox { mappings: {foo: "bar", baz: "qux", ...} }
         * in code the type `Wirebox["mappings"]` refers to the mappings
         */
        const wireboxInterface = Interface("Wirebox", wireboxMembers);
        const libFile = SourceFile("<<magic/wirebox>>", CfFileType.dCfm, "");
        libFile.containedScope.typeinfo.mergedInterfaces.set("Wirebox", wireboxInterface);
        return libFile;
    }

    function addEngineLib(absPath: string) {
        engineLib = addFile(absPath);
    }

    // this should probalby be done in the checker
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
            let maybeParent : SourceFile | undefined = undefined;

            // if there is a circularity, report it and don't put attach the parent to this cfc file
            if (heritageCircularityDetector.has(sourceFile.absPath)) {
                reportCircularity();
            }
            else {
                heritageCircularityDetector.add(sourceFile.absPath);
                maybeParent = tryGetParentComponent(sourceFile);
                if (maybeParent && maybeParent.cfc?.extends && heritageCircularityDetector.has(maybeParent.cfc.extends.absPath)) {
                    reportCircularity();
                    maybeParent = undefined;
                }
                heritageCircularityDetector.delete(sourceFile.absPath);
            }

            sourceFile.cfc = {extends: maybeParent ?? null, implements: []};
            if (maybeParent) {
                sourceFile.directDependencies.add(maybeParent);
            }
        }
    }

    /**
     * Convert some path or path component into its canonical form, which is identity
     * on case-sensitive systems, and all lower case on case-sensitive
     */
    function canonicalizePath<T extends string | null | undefined>(path: T) : T extends string ? string : undefined {
        if (typeof path !== "string") return undefined as any;
        return fileSystem.caseSensitive ? path : path.toLowerCase() as any;
    }

    function getCachedFile(absPath: string) : SourceFile | undefined {
        return files.get(canonicalizePath(absPath))?.deref();
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
    function tryGetParentComponent(sourceFile: SourceFile) : SourceFile | undefined {
        if (sourceFile.cfFileType !== CfFileType.cfc) return undefined;
        const heritageInfo = getExtendsSpecifier(sourceFile);
        if (!heritageInfo) return undefined;
        const {extendsSpecifier, extendsAttr} = heritageInfo;
        if (!extendsSpecifier) return undefined;
        const noSelfExtendsSpecifier = extendsSpecifier.filter(specifier => specifier.absPath !== sourceFile.absPath);
        if (extendsSpecifier.length !== noSelfExtendsSpecifier.length && noSelfExtendsSpecifier.length === 0) {
            errorAtNode(sourceFile, extendsAttr, "A component may not extend itself.");
            return undefined;
        }

        let result : SourceFile | undefined = undefined;
        for (const specifier of noSelfExtendsSpecifier) {
            result = getCachedFile(specifier.absPath) ?? tryAddFile(specifier.absPath);
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
        const sourceFile = getCachedFile(absPath);
        if (!sourceFile) {
            tryAddFile(absPath);
            return {parse: -1, bind: -1, check: -1};
        }

        const savedSourceFile : SourceFile = {...sourceFile};

        resetSourceFileInPlace(sourceFile, newSource);

        const r = parseBindCheckWorker(sourceFile, savedSourceFile.directDependencies, savedSourceFile);

        // if a cancellation was requested the source file should have been restored to it's state prior to the parseBindCheckWorker run
        if (r.cancellationRequested) {
            return {parse: NaN, bind: NaN, check: NaN}
        }
        else {
            return r.devTimingInfo;
        }
    }

    function getDiagnostics(absPath: string) : Diagnostic[] {
        return files.get(canonicalizePath(absPath))?.deref()?.diagnostics || [];
    }

    /**
     * a "javalikeTypename" is just a series of 1 or more identifiers separated by a "."
     * "a"
     * "a.b"
     * "a.b.c"
     * etc.
     */
    function findCfFileMappingByJavalikeTypename(javalikeTypename: string) : AbsPath | undefined {
        const pathComponents = javalikeTypename.split(".");
        if (pathComponents.length === 0) {
            return undefined;
        }

        let trieWalker : Trie<string> | string | undefined = ProjectMappings.cf.get(pathComponents.shift()!);
        while (trieWalker && typeof trieWalker !== "string" && pathComponents.length > 0) {
            trieWalker = trieWalker.get(pathComponents.shift()!);
        }

        if (typeof trieWalker !== "string") {
            return undefined;
        }

        const mappedPath = trieWalker;

        return fileSystem.join(projectRoot, mappedPath, ...pathComponents) + ".cfc";
    }
    
    // function tryGetDirectMappingHit(name: string) : {sourceFile: SourceFile, symbolTable: SymbolTable} | undefined {
    //     const fileAbsPath = findCfFileMappingByTypelikeName(name);
    //     if (!fileAbsPath) {
    //         return undefined;
    //     }

    //     const maybeFile = tryAddFile(fileAbsPath);

    //     if (maybeFile) {
    //         return {
    //             sourceFile: maybeFile.parsedSourceFile,
    //             symbolTable: maybeFile.parsedSourceFile.containedScope.this || new Map()
    //         }
    //     }
    //     else {
    //         return undefined;
    //     }
    // }

    let cfcDepth = 0;
    const cfcDepthLimit = 16;
    function CfcResolver(args: ComponentResolutionArgs | ComponentSpecifier | ProjectRelativeImportLookup) {
        // if (args.type === ComponentResolutionArgType.lookup) {
        //     const directMappingHit = tryGetDirectMappingHit(args.cfcName);
        //     if (directMappingHit) {
        //         return directMappingHit;
        //     }
        // }

        const hasExplicitPath = args.type === ComponentResolutionArgType.explicit;
        const hasRelativePath = args.type === ComponentResolutionArgType.projectRelative;

        if (cfcDepth > cfcDepthLimit) {
            if (hasExplicitPath) {
                console.log(`hit cfc resolution depth limit of ${cfcDepthLimit} resolving ` + args.absPath);
            }
            else if (hasRelativePath) {
                console.log(`hit cfc resolution depth limit of ${cfcDepthLimit} resolving ` + args.relPath);
            }
            else {
                console.log(`hit cfc resolution depth limit of ${cfcDepthLimit} resolving from ` + args.resolveFrom);
            }
            return undefined;
        }

        try {
            cfcDepth++;

            if (hasExplicitPath || hasRelativePath) {
                const absPath = hasExplicitPath ? args.absPath : fileSystem.normalize(fileSystem.join(__const__projectRoot, args.relPath));

                if (cfcDepth > cfcDepthLimit) {
                    console.log(`hit cfc resolution depth limit of ${cfcDepthLimit} on attempt to resolve cfc at '${absPath}'`);
                    return undefined;
                }
    
                const file = tryAddFile(absPath);
    
                if (file) {
                    return {
                        sourceFile: file,
                        symbolTable: file.containedScope.this || new Map()
                    }
                }
                else {
                    return undefined;
                }
            }

            const specifiers = buildPossibleCfcResolutionPaths(args.resolveFrom, args.cfcName);
            if (!specifiers) return undefined;
            for (const specifier of specifiers) {
                const file = getCachedFile(specifier.absPath);
                if (file) {
                    return {
                        sourceFile: file,
                        symbolTable: file.containedScope.this || new Map()
                    }
                }
            }
            for (const specifier of specifiers) {
                const file =  tryAddFile(specifier.absPath);
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
		const sourceFile = getCachedFile(absPath);
		if (!sourceFile) return undefined;
		return findNodeInFlatSourceMap(sourceFile.flatTree, sourceFile.nodeMap, targetIndex);
    }

    /**
     * get node to left of cursor, but return undefined on text spans and comments, and jump from the terminal node to it's parent construct
     * so instead of a terminal, return Identifier, or etc.
     */
    function getInterestingNodeToLeftOfCursor(absPath: string, targetIndex: number) : Node | undefined {
		const sourceFile = getCachedFile(absPath);
		if (!sourceFile) return undefined;
		const node = findNodeInFlatSourceMap(sourceFile.flatTree, sourceFile.nodeMap, targetIndex);
        if (!node
            || node.kind === NodeKind.comment
            || (node.kind === NodeKind.textSpan
                && node.parent?.kind !== NodeKind.simpleStringLiteral
                && node.parent?.kind !== NodeKind.interpolatedStringLiteral)) return undefined;
        
        // climb from terminal into production, or from textSpan into string literal
        if (node.kind === NodeKind.terminal || node.kind === NodeKind.textSpan) return node.parent ?? undefined;

        return node;
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

        const result = [];

        const explicitMapping = findCfFileMappingByJavalikeTypename(inCodeCfcName);
        if (explicitMapping) {
            result.push(ComponentSpecifier(explicitMapping));
        }

        result.push(ComponentSpecifier(fileSystem.join(resolveFrom, ...cfcComponents)));

        // magic coldbox/wirebox resolution, might want something to toggle this
        for (const coldboxModulePath of parentModulesFolders) {
            result.push(ComponentSpecifier(path.join(coldboxModulePath, ...cfcComponents)));
        }

        if (cfcNameStartsWithProjectRootDirName) {
            result.push(ComponentSpecifier(fileSystem.join(projectRoot, ...(cfcComponents.slice(1)))));
        }

        result.push(ComponentSpecifier(fileSystem.join(projectRoot, ...cfcComponents)));

        return result;
    }

    function EngineSymbolResolver(canonicalName: string) : SymTabEntry | undefined {
        if (!engineLib) return undefined;
        if (!engineLib.containedScope.typeinfo.mergedInterfaces.has("__cfEngine")) return undefined;
        return engineLib.containedScope.typeinfo.mergedInterfaces.get("__cfEngine")!.members.get(canonicalName);
    }

    // resolve types (right now, just interfaces) from *the* lib file
    function LibTypeResolver(name: string) : Type | undefined {
        if (!engineLib) return undefined;
        return engineLib.containedScope.typeinfo.mergedInterfaces.get(name);
    }

    return {
        addFile,
        addEngineLib,
        parseBindCheck,
        getDiagnostics,
        getNodeToLeftOfCursor,
        getInterestingNodeToLeftOfCursor,
        getParsedSourceFile: (absPath: string) => getCachedFile(absPath),
        getFileListing: () => [...files.keys()],
        __unsafe_dev_getChecker: () => checker,
        __unsafe_dev_getFile: (fname: string) : SourceFile | undefined => files.get(canonicalizePath(fname))?.deref(),
        canonicalizePath,
        getTransitiveDependencies,
        getTransitiveDependents,
        recheck: (fsPath: string) : void => {
            const file = getCachedFile(fsPath);
            if (file) {
                recheck(file);
            }
        },
    }
}

export type Project = ReturnType<typeof Project>;
export interface CfcResolution {
    sourceFile: SourceFile,
    symbolTable: ReadonlyMap<string, SymTabEntry>
}
export type CfcResolver = (args: ComponentResolutionArgs | ComponentSpecifier | ProjectRelativeImportLookup) => CfcResolution | undefined;
export type EngineSymbolResolver = (name: string) => SymTabEntry | undefined;
export type LibTypeResolver = (name: string) => Type | undefined;

const enum ComponentResolutionArgType { explicit, lookup, projectRelative }

export interface ComponentSpecifier {
    type: ComponentResolutionArgType.explicit,
    absPath: string,
}
export function ComponentSpecifier(absPath: string) : ComponentSpecifier {
    return {
        type: ComponentResolutionArgType.explicit,
        absPath
    }
}

interface ComponentResolutionArgs {
    type: ComponentResolutionArgType.lookup,
    resolveFrom: string,
    cfcName: string
};
export function ComponentResolutionArgs(resolveFrom: string, cfcName: string) : ComponentResolutionArgs {
    return {
        type: ComponentResolutionArgType.lookup,
        resolveFrom,
        cfcName
    }
}

export interface ProjectRelativeImportLookup {
    type: ComponentResolutionArgType.projectRelative,
    relPath: string,
}
export function ProjectRelativeImportLookup(relPath: string) : ProjectRelativeImportLookup {
    return {
        type: ComponentResolutionArgType.projectRelative,
        relPath
    }
}

function setDiff<T>(l: Set<T>, r: Set<T>) {
    const result = {
        uniqueInL: new Set<T>(),
        uniqueInR: new Set<T>(),
        inBoth: new Set<T>()
    }
    for (const e of l) {
        if (r.has(e)) {
            result.inBoth.add(e);
        }
        else {
            result.uniqueInL.add(e);
        }
    }
    for (const e of r) {
        if (l.has(e)) {
            continue; // if L has it, we found it in L already, no need to re-add
        }
        else {
            result.uniqueInR.add(e);
        }
    }
    return result;
}
