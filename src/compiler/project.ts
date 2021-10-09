import * as fs from "fs";
import * as path from "path";

import { Binder } from "./binder";
import { Checker } from "./checker";
import { BlockType, CallExpression, mergeRanges, Node, NodeId, NodeKind, SourceFile, StatementType, SymTabEntry } from "./node";
import { Parser } from "./parser";
import { CfFileType, SourceRange } from "./scanner";
import { cfcAsType, cfFunctionOverloadSet, cfFunctionSignatureParam, createLiteralType, _Type } from "./types";

import { cfmOrCfc, findNodeInFlatSourceMap, flattenTree, getAttributeValue, getComponentAttrs, getComponentBlock, getTriviallyComputableString, NodeSourceMap, visit } from "./utils";

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
    pathSep: string,
}

export function FileSystem() : FileSystem {
    return {
        readFileSync: fs.readFileSync,
        readdirSync: (root: string) => fs.readdirSync(root, {withFileTypes: true}),
        existsSync: fs.existsSync,
        lstatSync: (path: string) => fs.lstatSync(path),
        join: path.join,
        pathSep: path.sep
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

export type FilesystemPath = {[dir: string]: string | Buffer | FilesystemPath};
export function DebugFileSystem(files: Readonly<FilesystemPath>, pathSepOfDebugFs: string) : FileSystem {
    const maybeGet = (path: string) => {
        // we expect fileSystemPath to be rooted on "/" or "\"
        const pathComponents = [
            pathSepOfDebugFs,
            ...(path.split(pathSepOfDebugFs).filter(s => s !== ""))
        ];
        let working = files;
        while (pathComponents.length > 0) {
            const next = working[pathComponents.shift()!];
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

    function isFile(v: string | Buffer | FilesystemPath | undefined) : v is string | Buffer {
        return (typeof v === "string") || v instanceof Buffer;
    }

    function isDir(v: string | Buffer | FilesystemPath | undefined) : v is FilesystemPath {
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
        pathSep: pathSepOfDebugFs
    }
}

export const enum LanguageVersion { acf2018 = 1, lucee5 };

interface ProjectOptions {
    debug: boolean,
    parseTypes: boolean,
    language: LanguageVersion
}

export function Project(absRoots: string[], fileSystem: FileSystem, options: ProjectOptions) {
    type AbsPath = string;
    
    const parser = Parser(options);
    const binder = Binder();
    const checker = Checker();
    const heritageCircularityDetector = new Set<string>();

    if (options.parseTypes) {
        parser.setParseTypes(true);
    }

    if (options.debug) {
        parser.setDebug(true);
        binder.setDebug(true);
        // checker.setDebug(true);
    }

    binder.setLang(options.language);
    checker.setLang(options.language);
    checker.install({CfcResolver, EngineSymbolResolver});

    type FileCache = Map<AbsPath, CachedFile>;
    const files : FileCache = new Map();

    let engineLib : CachedFile | undefined;
    let withWirebox = true;
    let wireboxLib : SourceFile | null = null;

    function tryAddFile(absPath: string) : CachedFile | undefined {
        if (!fileSystem.existsSync(absPath)) return undefined;
        return addFile(absPath);
    }

    function parseBindCheckWorker(sourceFile: SourceFile) : DevTimingInfo {
        parser.setSourceFile(sourceFile);
        const parseStart = new Date().getTime();
        parser.parse();
        const parseElapsed = new Date().getTime() - parseStart;
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
        files.set(sourceFile.absPath, {
            parsedSourceFile: sourceFile,
            flatTree: flattenTree(sourceFile),
            nodeMap
        });

        const checkStart = new Date().getTime();

        if (withWirebox && wireboxLib) {
            sourceFile.libRefs.set("<<magic/wirebox>>", wireboxLib);
        }
        else {
            sourceFile.libRefs.delete("<<magic/wirebox>>");
        }

        checker.check(sourceFile);
        const checkElapsed = new Date().getTime() - checkStart;


        return { parse: parseElapsed, bind: bindElapsed, check: checkElapsed };
    }

    function addFile(absPath: string) {
        const alreadyExists = getCachedFile(absPath);
        if (alreadyExists) return alreadyExists;
        
        const fileType = cfmOrCfc(absPath);
        if (!fileType) return;
        const bytes = fileSystem.readFileSync(absPath);
        const sourceFile = SourceFile(absPath, fileType, bytes);

        parseBindCheckWorker(sourceFile);

        if (fileType === CfFileType.dCfm) {
            
        }

        if (withWirebox && /Wirebox\.cfc$/i.test(sourceFile.absPath)) {
            const mappings = buildWireboxMappings(sourceFile);
            if (mappings) {
                const overloads : {params: cfFunctionSignatureParam[], returns: _Type}[] = [];
                for (const mapping of mappings) {
                    if (mapping.kind === "dir") {
                        const fsTarget = mapping.target.replace(".", fileSystem.pathSep);
                        if (!fileSystem.existsSync(fsTarget) || !fileSystem.lstatSync(fsTarget).isDirectory()) continue;
                        const targets = fileSystem.readdirSync(fsTarget);
                        for (const target of targets) {
                            if (!target.isFile()) continue;
                            if (cfmOrCfc(target.name) !== CfFileType.cfc) continue;
                            const file = tryAddFile(fileSystem.join(fsTarget, target.name));
                            if (!file) continue;
                            const name = target.name.replace(/\.cfc$/i, "");
                            const nameAsLiteralType = createLiteralType(fsTarget.replace(fileSystem.pathSep, ".") + "." + name);
                            const param = cfFunctionSignatureParam(/*required*/true, nameAsLiteralType, "name")
                            overloads.push({params: [param], returns: cfcAsType(file.parsedSourceFile)});
                        }
                    }
                }
                const wireboxGetInstanceSymbol = {
                    uiName: "getInstance",
                    canonicalName: "getinstance",
                    declarations: null,
                    type: cfFunctionOverloadSet("getInstance", overloads, [])
                };

                wireboxLib = SourceFile("<<CFLS-GENERATED-LIB>>", CfFileType.dCfm, "");
                wireboxLib.containedScope.__declaration = new Map([["getinstance", wireboxGetInstanceSymbol]]);
            }
        }

        return files.get(absPath)!;
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

    function getCachedFile(absPath: string) : CachedFile | undefined {
        return files.get(absPath);
    }

    // fixme: dedup/unify this with the ones in parser/binder/checker
    function errorAtNode(sourceFile: SourceFile, node: Node, msg: string) {
        errorAtRange(sourceFile, node.range, msg);
    }

    function errorAtRange(sourceFile: SourceFile, range: SourceRange, msg: string) {
        sourceFile.diagnostics.push({
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
            extendsSpecifier: getCfcSpecifier(absRoots, sourceFile.absPath, heritageLiteral)
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
        return files.get(absPath)?.parsedSourceFile.diagnostics || [];
    }

    function CfcResolver(args: {resolveFrom: string, cfcName: string}) {
        const specifiers = getCfcSpecifier(absRoots, args.resolveFrom, args.cfcName);
        if (!specifiers) return undefined;
        for (const specifier of specifiers) {
            const file = getCachedFile(specifier.path)?.parsedSourceFile;
            if (file && file.containedScope.this) return {
                sourceFile: file,
                symbolTable: file.containedScope.this
            }
        }

        return undefined;
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

    function getCfcSpecifier(absRoots: string[], resolveFrom: string, possiblyUnqualifiedCfc: string) : ComponentSpecifier[] | undefined {
        const isUnqualified = !/\./.test(possiblyUnqualifiedCfc);
        for (const root of absRoots) {
            const base = path.parse(root).base; // Z in X/Y/Z, assuming Z is some root we're interested in
            const rel = path.relative(root, resolveFrom);
            const {dir} = path.parse(rel); // A/B/C in X/Y/Z/A/B/C, where X/Y/Z is the root
            // if it is unqualifed, we prepend the full path from root and lookup from that
            // with root of "X/", a file of "X/Y/Z/foo.cfm" calling "new Bar()" looks up "X.Y.Z.Bar"
            if (isUnqualified) {
                if (resolveFrom.startsWith(root)) {
                    const cfcName = [base, ...dir.split(fileSystem.pathSep), possiblyUnqualifiedCfc].filter(e => e !== "").join("."); // filter out possibly empty base and dirs; e.g. root is '/' so path.parse(root).base is ''
                    const xname = [base, possiblyUnqualifiedCfc].filter(e => e !== "").join(".");
                    return [
                        {
                            canonicalName: cfcName.toLowerCase(),
                            uiName: cfcName,
                            path: fileSystem.join(root, dir, possiblyUnqualifiedCfc + ".cfc")
                        },
                        {
                            canonicalName: xname.toLowerCase(),
                            uiName: xname,
                            path: fileSystem.join(root, possiblyUnqualifiedCfc + ".cfc")
                        },
                    ];
                }
            }
            else {
                // otherwise, we have a qualified CFC
                // check that the base name matches with the current root, and then try to resolve

                // root is /X/Y/Z, cfc is "Z.foo.bar"
                // we want paths:
                // - "/X/Y/Z/foo/bar.cfc" 
                // - "/X/Y/Z/Z/foo/bar.cfc"
                const canonicalCfcName = possiblyUnqualifiedCfc.toLowerCase();
                const canonicalBase = base.toLowerCase();
                if (!canonicalCfcName.startsWith(canonicalBase)) continue;
                const cfcComponents = possiblyUnqualifiedCfc.split(".");
                
                if (cfcComponents.length === 0) continue; // just to not crash; why would this happen?
                
                cfcComponents[cfcComponents.length - 1] = cfcComponents[cfcComponents.length - 1] + ".cfc";

                return [{
                    canonicalName: canonicalCfcName,
                    uiName: possiblyUnqualifiedCfc,
                    path: fileSystem.join(root, ...(cfcComponents.slice(1))) // /X/Y/Z/foo/bar.cfc
                },
                {
                    canonicalName: canonicalCfcName,
                    uiName: possiblyUnqualifiedCfc,
                    path: fileSystem.join(root, ...cfcComponents) // /X/Y/Z/Z/foo/bar.cfc
                }]
            }
        }
        return undefined;
    }

    function EngineSymbolResolver(canonicalName: string) : SymTabEntry | undefined {
        if (!engineLib) return undefined;
        if (!engineLib.parsedSourceFile.containedScope.__declaration) return undefined;
        return engineLib.parsedSourceFile.containedScope.__declaration.get(canonicalName);
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
        __unsafe_dev_getFile: (fname: string) => files.get(fname)
    }
}

export type Project = ReturnType<typeof Project>;
export type CfcResolver = (args: {resolveFrom: string, cfcName: string}) => {sourceFile: SourceFile, symbolTable: ReadonlyMap<string, SymTabEntry>} | undefined;
export type EngineSymbolResolver = (name: string) => SymTabEntry | undefined;

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
        if (node.kind === NodeKind.statement && node.subType === StatementType.expressionWrapper && node.expr!.kind === NodeKind.callExpression) {
            const mapping = tryMapOne(node.expr!);
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