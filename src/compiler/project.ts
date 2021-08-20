import * as fs from "fs";
import * as path from "path";

import { Binder } from "./binder";
import { Checker } from "./checker";
import { BlockType, mergeRanges, Node, NodeId, SourceFile, SymTabEntry } from "./node";
import { Parser } from "./parser";
import { Scanner, CfFileType, SourceRange } from "./scanner";
import { cfmOrCfc, findNodeInFlatSourceMap, flattenTree, getAttributeValue, getComponentAttrs, getComponentBlock, getTriviallyComputableString, NodeSourceMap } from "./utils";

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

interface FileSystem {
    readFileSync: (path: string) => Buffer
    existsSync: (path: string) => boolean
    join: (...args: string[]) => string,
    pathSep: string,
}

export function FileSystem() : FileSystem {
    return {
        readFileSync: fs.readFileSync,
        existsSync: fs.existsSync,
        join: path.join,
        pathSep: path.sep
    }
}

export function DebugFileSystem(files: [absPath: string, text: string][], pathSep: string) : FileSystem {
    const fmap = new Map(files.map(([absPath, text]) => [absPath, Buffer.from(text, "utf-8")]));
    return {
        readFileSync: (path: string) => {
            const file = fmap.get(path);
            if (!file) throw "bad file lookup";
            return file;
        },
        existsSync: (path: string) => !!fmap.get(path),
        join: (...args: string[]) => {
            // join using path.join, then replace the platform pathSep with the debug path sep
            const t = path.join(...args);
            return t.replace(path.sep, pathSep);
        },
        pathSep: pathSep
    }
}

interface ProjectOptions {
    debug: boolean,
    parseTypes: boolean,
}

export function Project(absRoots: string[], fileSystem: FileSystem, options: ProjectOptions) {
    type AbsPath = string;
    
    const parser = Parser();
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

    checker.installCfcResolver(CfcResolver);

    type FileCache = Map<AbsPath, CachedFile>;
    const files : FileCache = new Map();

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

        const checkStart = new Date().getTime();
        checker.check(sourceFile);
        const checkElapsed = new Date().getTime() - checkStart;

        files.set(sourceFile.absPath, {
            parsedSourceFile: sourceFile,
            flatTree: flattenTree(sourceFile),
            nodeMap
        });

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
        return files.get(absPath)!;
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

    function tryGetParentComponent(sourceFile: SourceFile) : CachedFile | undefined {
        if (sourceFile.cfFileType !== CfFileType.cfc) return undefined;
        const heritageInfo = getExtendsSpecifier(sourceFile);
        if (!heritageInfo) return undefined;
        const {extendsSpecifier, extendsAttr} = heritageInfo;
        if (!extendsSpecifier) return undefined;
        if (extendsSpecifier.path === sourceFile.absPath) {
            errorAtNode(sourceFile, extendsAttr, "A component may not extend itself.");
            return undefined;
        }
        const cachedParent = getCachedFile(extendsSpecifier.path);
        if (cachedParent) return cachedParent;
        else return tryAddFile(extendsSpecifier.path);
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

        // need to formalize this; we want to reset the source file object but not move it in memory, so
        // that other files referencing it can continue to do so
        const sourceFile = cache.parsedSourceFile;
        sourceFile.scanner = Scanner(newSource);
        sourceFile.cfc = undefined;
        sourceFile.cachedFlowTypes = new Map();
        sourceFile.cachedNodeTypes = new Map();
        sourceFile.containedScope = {container: null, typedefs: new Map()};
        sourceFile.content = [];
        sourceFile.diagnostics = [];

        return parseBindCheckWorker(sourceFile);
    }

    function getDiagnostics(absPath: string) {
        return files.get(absPath)?.parsedSourceFile.diagnostics || [];
    }

    function CfcResolver(args: {resolveFrom: string, cfcName: string}) {
        const specifier = getCfcSpecifier(absRoots, args.resolveFrom, args.cfcName);
        if (!specifier) return undefined;
        const file = getCachedFile(specifier.path)?.parsedSourceFile;
        return file?.containedScope.this || undefined;
    }

    function getNodeToLeftOfCursor(absPath: string, targetIndex: number) : Node | undefined {
		const docCache = getCachedFile(absPath);
		if (!docCache) return undefined;
		return findNodeInFlatSourceMap(docCache.flatTree, docCache.nodeMap, targetIndex);
    }

    function getParsedSourceFile(absPath: string) {
        return getCachedFile(absPath)?.parsedSourceFile || undefined;
    }

    function getCfcSpecifier(absRoots: string[], resolveFrom: string, possiblyUnqualifiedCfc: string) : ComponentSpecifier | undefined {
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
                    return {
                        canonicalName: cfcName.toLowerCase(),
                        uiName: cfcName,
                        path: fileSystem.join(root, dir, possiblyUnqualifiedCfc + ".cfc")
                    }
                }
            }
            else {
                // otherwise, we have a qualified CFC
                // check that the base name matches with the current root, and then try to resolve
                const canonicalCfcName = possiblyUnqualifiedCfc.toLowerCase();
                const canonicalBase = base.toLowerCase();
                if (!canonicalCfcName.startsWith(canonicalBase)) continue;
                const cfcAsPathElidedBase = possiblyUnqualifiedCfc.split(".").slice(1);
                if (cfcAsPathElidedBase.length > 0) cfcAsPathElidedBase[cfcAsPathElidedBase.length - 1] = cfcAsPathElidedBase[cfcAsPathElidedBase.length - 1] + ".cfc";
                return {
                    canonicalName: canonicalCfcName,
                    uiName: possiblyUnqualifiedCfc,
                    path: fileSystem.join(root, ...cfcAsPathElidedBase)
                }
            }
        }
        return undefined;
    }

    return {
        addFile,
        parseBindCheck,
        getDiagnostics,
        getNodeToLeftOfCursor,
        getParsedSourceFile,
        __unsafe_dev_getChecker: () => checker
    }
}

export type Project = ReturnType<typeof Project>;
export type CfcResolver = (args: {resolveFrom: string, cfcName: string}) => ReadonlyMap<string, SymTabEntry> | undefined;

export interface ComponentSpecifier {
    canonicalName: string,
    uiName: string,
    path: string,
}

export interface ComponentNode {
    readonly specifier: ComponentSpecifier | undefined,
    readonly parent: ComponentNode | null,
    readonly children: ComponentNode[],
}

function ComponentNode(specifier: ComponentSpecifier | undefined) : ComponentNode {
    return {
        specifier,
        parent: null,
        children: []
    }
}