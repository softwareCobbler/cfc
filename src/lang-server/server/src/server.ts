/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	DidChangeConfigurationNotification,
	CompletionItem,
	CompletionItemKind,
	TextDocumentPositionParams,
	TextDocumentSyncKind,
	InitializeResult,
	ConnectionOptions,
	DocumentSymbolParams,
	SymbolInformation,
	SymbolKind,
	CompletionContext,
	CompletionParams,
	CompletionTriggerKind,
	SignatureInformation,
	ParameterInformation,
	DidChangeConfigurationParams,
	ConfigurationItem,
	WorkspaceFolder,
	ProgressType,
} from 'vscode-languageserver/node';

import { SignatureHelp, Position, Location, Range } from "vscode-languageserver-types"

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

import { URI } from "vscode-uri";

import * as fs from "fs";
import * as path from "path";

import { NodeId, SourceFile, Parser, Binder, Node as cfNode, binarySearch, CfFileType, Diagnostic as cfcDiagnostic, cfmOrCfc, flattenTree, NodeSourceMap, isExpressionContext, getTriviallyComputableString, Checker } from "compiler";
import { CfTag, isStaticallyKnownScopeName, NodeKind, ScopeDisplay, StaticallyKnownScopeName, FunctionDefinition, mergeRanges, CallExpression, Terminal, SymTabEntry, IndexedAccessChainElement } from '../../../compiler/node';
import { findAncestor, findNodeInFlatSourceMap, getAttributeValue, getComponentAttrs, getFunctionSignatureParamNames, getNearestConstruct, getNearestEnclosingScope, getSourceFile, isCfScriptTagBlock } from '../../../compiler/utils';

import { tagNames } from "./tagnames";
import { isCfc, isFunctionSignature, isStruct, _Type } from '../../../compiler/types';
import { FileSystem, Project } from "../../../compiler/project";
import { TokenType } from '../../../compiler/scanner';

type TextDocumentUri = string;

interface CflsConfig {
	parser: ReturnType<typeof Parser>,
	binder: ReturnType<typeof Binder>,
	checker: Checker,
	parseCache: Map<TextDocumentUri, {parsedSourceFile: SourceFile, flatTree: NodeSourceMap[], nodeMap: ReadonlyMap<NodeId, cfNode>}>,
	evictFile: (uri: TextDocumentUri) => void,
	lib: SourceFile | null,
	x_types: boolean
}

let project : Project; // init this before using it

let workspaceRoots : WorkspaceFolder[] = [];

let cflsConfig! : CflsConfig;

function naiveGetDiagnostics(uri: TextDocumentUri, freshText: string | Buffer) : cfcDiagnostic[] {
	if (!project) return [];

	const fsPath = URI.parse(uri).fsPath;

	const timing = project.parseBindCheck(fsPath, freshText);
	connection.console.info(`parse ${timing.parse} // bind ${timing.bind} // check ${timing.check}`);
	return project.getDiagnostics(fsPath) ?? [];

	/*

	if (!cflsConfig) return [];
	
	// how to tell if we were launched in debug mode ?
	const {parser,binder, parseCache} = cflsConfig;

	const cfFileType = cfmOrCfc(uri);
	if (!cfFileType) {
		return [];
	}

	const sourceFile = SourceFile(uri, cfFileType, text);
	if (cflsConfig.lib) {
		sourceFile.libRefs.push(cflsConfig.lib);
	}

    parser.setSourceFile(sourceFile).parse();
	binder.bind(sourceFile);
	
	function getQualifiedCfcPathName(uri: TextDocumentUri) {
		for (const root of workspaceRoots) {
			if (uri.startsWith(root.uri)) {
				const base = path.parse(root.uri).base;
				const rel = path.relative(root.uri, uri);
				const {dir, name} = path.parse(rel);
				return [
					base,
					...dir.split(path.sep),
					name].join(".");
			}
		}

		return path.parse(uri).name;
	}

	if (cfFileType === CfFileType.cfc && sourceFile.containedScope.this) {
		const cfcName = getQualifiedCfcPathName(uri);
		cflsConfig.checker.includeCfc(cfcName, sourceFile.containedScope.this);
		console.info("include cfc: " + cfcName);
	}

	// we need finer granularity of what the checker emits errors for
	//if (cflsConfig.x_types) {
		cflsConfig.checker.check(sourceFile);
	//}
	
	parseCache.set(uri, {
		parsedSourceFile: sourceFile,
		flatTree: flattenTree(sourceFile),
		nodeMap: binder.getNodeMap(),
	});

    return sourceFile.diagnostics;
	*/
}

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
let connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
let documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let hasConfigurationCapability: boolean = false;
let hasWorkspaceFolderCapability: boolean = false;
let hasDiagnosticRelatedInformationCapability: boolean = false;

connection.onInitialize((params: InitializeParams) => {
	let capabilities = params.capabilities;

	let roots = params.workspaceFolders;
	if (roots) {
		workspaceRoots = roots;
		connection.console.info("cflsp/initialize, workspaces:")
		connection.console.info(roots.map(e=>e.uri).join(","));
	}

	// Does the client support the `workspace/configuration` request?
	// If not, we fall back using global settings.
	hasConfigurationCapability = !!(
		capabilities.workspace && !!capabilities.workspace.configuration
	);
	hasWorkspaceFolderCapability = !!(
		capabilities.workspace && !!capabilities.workspace.workspaceFolders
	);
	hasDiagnosticRelatedInformationCapability = !!(
		capabilities.textDocument &&
		capabilities.textDocument.publishDiagnostics &&
		capabilities.textDocument.publishDiagnostics.relatedInformation
	);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			// Tell the client that this server supports code completion.
			completionProvider: {triggerCharacters: [".", " ", "("]},
			//signatureHelpProvider: {triggerCharacters: ["("]},
			//definitionProvider: true,
		}
	};

	if (params.initializationOptions?.libpath) {
		
	}
	/*
	if (hasWorkspaceFolderCapability) {
		result.capabilities.workspace = {
			workspaceFolders: {
				supported: true
			}
		};
	}*/
	return result;
});

interface ClientRequest {
	textDocument: {uri: TextDocumentUri},
	position: Position,
}

function getNodeTargetedByClientRequest(clientRequest : ClientRequest) : cfNode | undefined {
	const uri = clientRequest.textDocument.uri;
	const position = clientRequest.position;

	const document = documents.get(uri);
	if (!document) return undefined;

	const targetIndex = document.offsetAt(position);
	const docCache = cflsConfig.parseCache.get(uri);

	if (!docCache) return undefined;

	return findNodeInFlatSourceMap(docCache.flatTree, docCache.nodeMap, targetIndex);
}

connection.onDefinition((params) : Location | undefined  => {
	return undefined;
	/*
	const doc = documents.get(params.textDocument.uri);
	if (!doc) return;
	const targetNode = getNodeTargetedByClientRequest(params);
	if (!targetNode) return undefined;
	const node = getNearestConstruct(targetNode);
	if (!node || !isExpressionContext(node)) return undefined;

	if (node.kind === NodeType.callExpression) {
		return getFunctionDefinitionLocation(node.left, doc);
	}
	else if (node.parent?.kind === NodeType.callExpression && node === node.parent?.left) {
		return getFunctionDefinitionLocation(node, doc);
	}
	else {
		return undefined;
	}

	function getFunctionDefinitionLocation(node: cfNode, doc: TextDocument) : Location | undefined {
		if (node.kind === NodeType.identifier && node.canonicalName) {
			const sourceFile = getSourceFile(node);
			if (sourceFile?.cfFileType === CfFileType.cfc) {
				const symbol = sourceFile.containedScope!.this?.get(node.canonicalName);
				if (!symbol || !symbol.firstBinding) return undefined;
				const functionDef = symbol.firstBinding as FunctionDefinition;
				let range : Range;
				if (functionDef.fromTag) {
					const attrVal = getAttributeValue((node.tagOrigin.startTag as CfTag.Common).attrs, "name");
					if (!attrVal) {
						return undefined;
					}
					range = {
						start: doc.positionAt(attrVal.range.fromInclusive),
						end: doc.positionAt(attrVal.range.toExclusive)
					};
				}
				else {
					const mergedRange = mergeRanges(functionDef.accessModifier, functionDef.returnType, functionDef.functionToken, functionDef.rightParen, functionDef.attrs);
					range = {
						start: doc.positionAt(mergedRange.fromInclusive),
						end: doc.positionAt(mergedRange.toExclusive)
					};
				}
				return {
					uri: params.textDocument.uri,
					range
				}
			}		
		}
	}*/
});

function resetCflsp(x_types: boolean = false) {
	console.info("[reset]");
	cflsConfig = {
		parser: Parser().setDebug(true).setParseTypes(false),
		binder: Binder().setDebug(true),
		checker: Checker(),
		parseCache: new Map<TextDocumentUri, {parsedSourceFile: SourceFile, flatTree: NodeSourceMap[], nodeMap: ReadonlyMap<NodeId, cfNode>}>(),
		lib: cflsConfig?.lib ?? null, // carry forward lib
		evictFile: (uri: TextDocumentUri) : void => {
			if (cflsConfig.parseCache.has(uri)) {
				cflsConfig.parseCache.delete(uri);
			}
		},
		x_types: false,
	};
	//cflsConfig.checker.installCfcResolver(CfcResolver(workspaceRoots.map(root => root.uri)));

	project = Project(workspaceRoots.map(v => URI.parse(v.uri).fsPath), FileSystem(), {parseTypes: x_types, debug: true});
}

function reemitDiagnostics() {
	connection.console.info("reemit diagnositcs for " + cflsConfig.parseCache.size + " file URIs");
	const uris = [...cflsConfig.parseCache.keys()]; // parseCache is indirectly updated in-loop, so grab a copy of the values before iterating
	for (const uri of uris) {
		const textDocument = documents.get(uri);
		const text = textDocument?.getText();
		const fileType = cfmOrCfc(uri);

		if (text && fileType) {
			cflsConfig.evictFile(uri);
			connection.sendDiagnostics({
				uri: uri,
				diagnostics: cfcDiagnosticsToLspDiagnostics(textDocument!, naiveGetDiagnostics(uri, text))
			});
		}
	}
	connection.console.info("...done with diagnostic reemit");
}

connection.onInitialized(() => {
	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, undefined);

		// immediately configure ourselves
		resetCflsp();

		// ok, now we can wait to ask the client for the workspace configuration; this might take a while to complete
		// and completions requests and etc. can be arriving and getting serviced during the wait
		connection.workspace.getConfiguration("cflsp").then((config) => {
			if (config.x_types !== cflsConfig.x_types) {
				resetCflsp(config.x_types ?? false);
			}
		});
	}
	else {
		//resetCflsp(/*x_types*/false);
	}

	connection.sendNotification("cflsp/libpath");
	connection.sendNotification("cflsp/ready");

	/*
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			connection.console.log('Workspace folder change event received.');
		});
	}*/

	connection.console.info("cflsp server initialized");
});

// The example settings
interface ExampleSettings {
	maxNumberOfProblems: number;
}

// The global settings, used when the `workspace/configuration` request is not supported by the client.
// Please note that this is not the case when using this server with the client provided in this example
// but could happen with other clients.
const defaultSettings: ExampleSettings = { maxNumberOfProblems: 1000 };
let globalSettings: ExampleSettings = defaultSettings;

// Cache the settings of all open documents
let documentSettings: Map<string, Thenable<ExampleSettings>> = new Map();

connection.onDidChangeConfiguration(async change => {
	//let x_types : boolean;
	if (hasConfigurationCapability) {
		// Reset all cached document settings
		documentSettings.clear();
		connection.workspace.getConfiguration("cflsp").then((config) => {
			resetCflsp(config.x_types ?? false);
			documents.all().forEach(validateTextDocument);
		});
	}
	else {
		/*globalSettings = <ExampleSettings>(
			(change.settings.languageServerExample || defaultSettings)
		);*/
		resetCflsp(/*x_types*/false);
		// Revalidate all open text documents
		documents.all().forEach(validateTextDocument);
	}
});

connection.onDocumentSymbol((params: DocumentSymbolParams) => {
	return [];
	/*
	params.textDocument.uri;
	const textDocument = documents.get(params.textDocument.uri);

	if (!textDocument) return [];

	const v : SymbolInformation[] = [{
			name: "someSymbol",
			kind: SymbolKind.Variable,
			location: {
				uri: params.textDocument.uri,
				range: {
					start: textDocument?.positionAt(0),
					end: textDocument?.positionAt(4)
				}
			}
		}];

	return v;*/
});

function getDocumentSettings(resource: string): Thenable<ExampleSettings> {
	if (!hasConfigurationCapability) {
		return Promise.resolve(globalSettings);
	}
	let result = documentSettings.get(resource);
	if (!result) {
		result = connection.workspace.getConfiguration({
			scopeUri: resource,
			section: 'languageServerExample'
		});
		documentSettings.set(resource, result);
	}
	return result;
}

// Only keep settings for open documents
documents.onDidClose(e => {
	documentSettings.delete(e.document.uri);
	connection.sendDiagnostics({ uri: e.document.uri, diagnostics: [] });
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	validateTextDocument(change.document);
});

const cfmPattern = /cfml?$/i;
const cfcPattern = /cfc$/i;

function cfcDiagnosticsToLspDiagnostics(textDocument: TextDocument, cfDiagnostics: cfcDiagnostic[]) {
	let diagnostics: Diagnostic[] = [];

	for (const diagnostic of cfDiagnostics) {
		diagnostics.push({
			severity: DiagnosticSeverity.Error,
			range: {
				start: textDocument.positionAt(diagnostic.fromInclusive),
				end: textDocument.positionAt(diagnostic.toExclusive)
			},
			message: diagnostic.msg,
			source: "cfls"
		});
	}

	return diagnostics;
}

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
	// In this simple example we get the settings for every validate run.
	//let settings = await getDocumentSettings(textDocument.uri);
	
	let cfDiagnostics : cfcDiagnostic[] = [];

	if (cfmOrCfc(textDocument.uri) !== undefined) {
		cfDiagnostics = naiveGetDiagnostics(textDocument.uri, textDocument.getText());
	}

	const diagnostics = cfcDiagnosticsToLspDiagnostics(textDocument, cfDiagnostics);

	// Send the computed diagnostics to VSCode.
	connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

connection.onDidChangeWatchedFiles(_change => {
	// Monitored files have change in VSCode
	connection.console.log('We received an file change event');
});

// This handler provides the initial list of the completion items.
connection.onCompletion(
	(completionParams: CompletionParams): CompletionItem[] => {
		if (!project) return [];

		const document = documents.get(completionParams.textDocument.uri);
		if (!document) return [];

		const fsPath = URI.parse(document.uri).fsPath;
		const targetIndex = document.offsetAt(completionParams.position);
		const parsedSourceFile = project.getParsedSourceFile(fsPath);
		const node = project.getNodeToLeftOfCursor(fsPath, targetIndex);

		if (!parsedSourceFile || !node) return [];

		let callExpr : CallExpression | null = (node.parent?.parent?.kind === NodeKind.callArgument && !node.parent.parent.equals)
			? node.parent.parent.parent as CallExpression // inside a named argument `foo(a|)
			: (node.kind === NodeKind.terminal && node.token.type === TokenType.LEFT_PAREN && node.parent?.kind === NodeKind.callExpression)
			? node.parent as CallExpression // right on `foo(|`
			: (node.parent?.kind === NodeKind.terminal && node.parent.token.type === TokenType.LEFT_PAREN && node.parent.parent?.kind === NodeKind.callExpression)
			? node.parent.parent // on whitespace after `foo(   |`
			: (node.parent?.kind === NodeKind.terminal && node.parent.token.type === TokenType.COMMA && node.parent.parent?.parent?.kind === NodeKind.callExpression)
			? node.parent.parent.parent // after a comma `foo(arg0, |`
			: null;

		// a whitespace or left-paren trigger character is only used for showing named parameters inside a call argument list
		if ((completionParams.context?.triggerCharacter === " " || completionParams.context?.triggerCharacter === "(") && !callExpr) {
			if (!callExpr) return [];
		}

		const expressionContext = isExpressionContext(node);

		if (!expressionContext) {
			if (node.parent?.kind === NodeKind.tag && (node === node.parent.tagStart || node === node.parent.tagName)) {
				return tagNames.map((name) : CompletionItem => {
					return { 
						label: "cf" + name,
						kind: CompletionItemKind.Property,
						detail: "cflsp:<<taginfo?>>"
					}
				});
			}
			return [];
		}

		if (isCfScriptTagBlock(node)) {
			let justCfScriptCompletion = false;
			// if we got </cf then we are in an unfinished tag node
			if (node.parent?.kind === NodeKind.tag && node.parent?.which === CfTag.Which.end) {
				justCfScriptCompletion = true;
			}
			// if we got got an identifier but the previous text is "</" (not valid in any expression) then just provide a cfscript completion
			else if (node.parent?.kind === NodeKind.identifier && node.range.fromInclusive >= 2) {
				const text = document.getText();
				if (text[node.range.fromInclusive-2] === "<" && text[node.range.fromInclusive-1] === "/") {
					justCfScriptCompletion = true;
				}
			}

			if (justCfScriptCompletion) {
				return [{
					label: "cfscript",
					kind: CompletionItemKind.Property,
					detail: "cflsp:<<taginfo?>>",
					insertText: "cfscript>",
				}];
			}
		}
		
		const result : CompletionItem[] = [];

		// `foo(bar = baz, |)`
		// `foo(b|`
		// `foo( |)`
		// NOT `foo(bar = |`, that should be an expression completion
		if (callExpr) {
			const sig = cflsConfig.checker.getCachedEvaluatedNodeType(callExpr.left, parsedSourceFile);
			if (!sig || !isFunctionSignature(sig)) return [];

			const yetToBeUsedParams = new Set<string>(sig.params.map(param => param.canonicalName));
			for (const arg of callExpr.args) if (arg.name?.canonicalName) yetToBeUsedParams.delete(arg.name?.canonicalName)

			const detail = callExpr.parent?.kind === NodeKind.new ? "named constructor argument" : "named function argument";

			for (const param of sig.params) {
				if (!yetToBeUsedParams.has(param.canonicalName)) continue;
				result.push({
					label: param.uiName + "=",
					kind: CompletionItemKind.Variable,
					detail: detail,
					sortText: "000_" + param.uiName, // we'd like param name suggestions first
				});
			}
		}

		if (node.parent?.kind === NodeKind.indexedAccessChainElement) {
			// get the type one level before the current
			// x.y| -- we want the type of `x` for completions, not `y`

			const typeinfo = project.__unsafe_dev_getChecker().getCachedEvaluatedNodeType(node.parent.parent, parsedSourceFile);
			const result : CompletionItem[] = [];

			if (isStruct(typeinfo)) {
				for (const symTabEntry of typeinfo.members.values()) {
					if (symTabEntry.canonicalName === "init" && isCfc(typeinfo)) { continue };
					result.push({
						label: symTabEntry.uiName,
						kind: isFunctionSignature(symTabEntry.type)
							? CompletionItemKind.Function
							: CompletionItemKind.Field,
						detail: ""
					})
				}
			}

			return result;
		}

		// we're in a primary expression context, where we need to do symbol lookup in all visible scopes
		// e.g,. `x = | + y`
		const allVisibleNames = (function (node: cfNode | null) {
			const result = new Map<string, [StaticallyKnownScopeName, CompletionItemKind, number]>();
			let scopeDistance = 0; // keep track of "how far away" some name is, in terms of parent scopes; we can then offer closer names first
			while (node) {
				if (node.containedScope) {
					for (const searchScope of ["local", "arguments", "variables"] as StaticallyKnownScopeName[]) {
						const symTab = node.containedScope[searchScope];
						if (!symTab) continue;
						for (const symTabEntry of symTab.values()) {
							const completionKind = isFunctionSignature(symTabEntry.type)
								? CompletionItemKind.Function
								: CompletionItemKind.Variable;
							result.set(symTabEntry.uiName, [searchScope, completionKind, scopeDistance]);
						}
					}
					scopeDistance++;
				}
				node = node.containedScope?.container || node.parent;
			}
			return result;
		})(node);

		for (const [varName, [_, completionKind, scopeDistance]] of allVisibleNames) { 
			result.push({
				label: varName,
				kind: completionKind,
				// sort the first two scopes to the top of the list; the rest get lexically sorted as one agglomerated scope
				sortText: (scopeDistance === 0 ? 'a' : scopeDistance === 1 ? 'b' : 'c') + scopeDistance
			});
		}

		return result;
	}
);

/*connection.onSignatureHelp((params) : SignatureHelp => {
	params;
	const x : ParameterInformation[] = [];
	x.push(ParameterInformation.create("someparam1", "1111 where does type info go"));
	x.push(ParameterInformation.create("someparam2", "2222 where does type info go"));
	const siginfo = SignatureInformation.create("foo", "docstring goes here\nmaybe a newline?", ...x);
	return {
		signatures: [siginfo],
		activeSignature: null,
		activeParameter: 0 // 0 indexed
	}
})*/

connection.onNotification("cflsp/libpath", (libAbsPath: string) => {
	/*connection.console.info("received cflsp/libpath notification, path=" + libAbsPath);
	if (!libAbsPath) return;
	const path = libAbsPath;
	const sourceFile = SourceFile(path, CfFileType.dCfm, fs.readFileSync(path));
	cflsConfig.parser.setSourceFile(sourceFile).parse();
	cflsConfig.binder.bind(sourceFile);
	cflsConfig.lib = sourceFile;
	reemitDiagnostics();*/
});

connection.onNotification("cflsp/cache-cfcs", (cfcAbsPaths: string[]) => {
	for (const absPath of cfcAbsPaths) {
		const start = new Date().getTime();
		project.addFile(absPath);
		connection.sendNotification("cflsp/cached-cfc"); // just saying "hey we're done with one more"
		const elapsed = (new Date().getTime()) - start;
		connection.console.info(absPath + "\n\t" + elapsed);
	}
})

// This handler resolves additional information for the item selected in
// the completion list.
/*connection.onCompletionResolve(
	(item: CompletionItem): CompletionItem => {
		if (item.data === 1) {
			item.detail = 'TypeScript details';
			item.documentation = 'TypeScript documentation';
		} else if (item.data === 2) {
			item.detail = 'JavaScript details';
			item.documentation = 'JavaScript documentation';
		}
		return item;
	}
);*/

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
