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
	CompletionTriggerKind
} from 'vscode-languageserver/node';

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

import { NodeId, SourceFile, Parser, Binder, Node as cfNode, binarySearch, CfFileType, Diagnostic as cfcDiagnostic, cfmOrCfc, flattenTree, getScopeContainedNames, NodeSourceMap, isExpressionContext, getTriviallyComputableString } from "compiler";
import { CfTag, isStaticallyKnownScopeName, NodeType, ScopeDisplay, StaticallyKnownScopeName } from '../../../compiler/node';
import { findNodeInFlatSourceMap, getNearestEnclosingScope, isCfScriptTagBlock } from '../../../compiler/utils';

import { tagNames } from "./tagnames";
import { TokenType } from '../../../compiler/scanner';

const parser = Parser().setDebug(true);
const binder = Binder().setDebug(true);
type TextDocumentUri = string;
const parseCache = new Map<TextDocumentUri, {parsedSourceFile: SourceFile, flatTree: NodeSourceMap[], nodeMap: ReadonlyMap<NodeId, cfNode>}>();

function naiveGetDiagnostics(uri: TextDocumentUri, text: string, fileType: CfFileType) : readonly cfcDiagnostic[] {
	// how to tell if we were launched in debug mode ?

	const cfFileType = cfmOrCfc(uri);
	if (!cfFileType) {
		return [];
	}

	const sourceFile = SourceFile(uri, cfFileType, text);
    parser.setSourceFile(sourceFile);

    parser.parse(fileType);
	binder.bind(sourceFile, parser.getScanner(), parser.getDiagnostics());
	
	parseCache.set(uri, {
		parsedSourceFile: sourceFile,
		flatTree: flattenTree(sourceFile),
		nodeMap: binder.getNodeMap(),
	});

    return parser.getDiagnostics();
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
			completionProvider: {triggerCharacters: ["."]},
		}
	};
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

connection.onInitialized(() => {
	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, undefined);
	}
	/*
	if (hasWorkspaceFolderCapability) {
		connection.workspace.onDidChangeWorkspaceFolders(_event => {
			connection.console.log('Workspace folder change event received.');
		});
	}*/
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

connection.onDidChangeConfiguration(change => {
	if (hasConfigurationCapability) {
		// Reset all cached document settings
		documentSettings.clear();
	} else {
		globalSettings = <ExampleSettings>(
			(change.settings.languageServerExample || defaultSettings)
		);
	}

	// Revalidate all open text documents
	documents.all().forEach(validateTextDocument);
});

connection.onDocumentSymbol((params: DocumentSymbolParams) => {
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

	return v;
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

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
	// In this simple example we get the settings for every validate run.
	//let settings = await getDocumentSettings(textDocument.uri);
	
	let cfDiagnostics : readonly cfcDiagnostic[];

	if (cfmPattern.test(textDocument.uri)) {
		cfDiagnostics = naiveGetDiagnostics(textDocument.uri, textDocument.getText(), CfFileType.cfm);
	}
	else if (cfcPattern.test(textDocument.uri)) {
		cfDiagnostics = naiveGetDiagnostics(textDocument.uri, textDocument.getText(), CfFileType.cfc);
	}
	else {
		// didn't match a cf file type, send an empty diagnostics list and we're done
		connection.sendDiagnostics({ uri: textDocument.uri, diagnostics: [] });
		return;
	}

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
	/*
	while ((m = pattern.exec(text)) && problems < settings.maxNumberOfProblems) {
		problems++;
		let diagnostic: Diagnostic = {
			severity: DiagnosticSeverity.Warning,
			range: {
				start: textDocument.positionAt(m.index),
				end: textDocument.positionAt(m.index + m[0].length)
			},
			message: `${m[0]} is all uppercase.`,
			source: 'ex'
		};
		if (hasDiagnosticRelatedInformationCapability) {
			diagnostic.relatedInformation = [
				{
					location: {
						uri: textDocument.uri,
						range: Object.assign({}, diagnostic.range)
					},
					message: 'Spelling matters'
				},
				{
					location: {
						uri: textDocument.uri,
						range: Object.assign({}, diagnostic.range)
					},
					message: 'Particularly for names'
				}
			];
		}
		diagnostics.push(diagnostic);
	}*/

	// Send the computed diagnostics to VSCode.
	connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

connection.onDidChangeWatchedFiles(_change => {
	// Monitored files have change in VSCode
	connection.console.log('We received an file change event');
});

// This handler provides the initial list of the completion items.
connection.onCompletion(
	(textDocumentPosition: CompletionParams): CompletionItem[] => {
		
		const document = documents.get(textDocumentPosition.textDocument.uri);
		if (!document) return [];

		const targetIndex = document.offsetAt(textDocumentPosition.position);
		const docCache = parseCache.get(textDocumentPosition.textDocument.uri)!;

		const node = findNodeInFlatSourceMap(docCache.flatTree, docCache.nodeMap, targetIndex);

		if (!node) return [];

		const expressionContext = isExpressionContext(node);

		if (!expressionContext) {
			if (node.parent?.kind === NodeType.tag && (node === node.parent.tagStart || node === node.parent.tagName)) {
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

		// if we got an indexed access chain, we only want to provide completions for the first dot
		if (node.parent?.kind === NodeType.indexedAccessChainElement) {
			if (node.parent?.parent?.kind === NodeType.indexedAccess) {
				// if so, try to get the identifier used as the root of the chain
				// if that name is a known scope, try to find the names in that scope
				const scopeName = getTriviallyComputableString(node.parent.parent.root)?.toLowerCase();
				if (scopeName && isStaticallyKnownScopeName(scopeName)) {
					const scope = getNearestEnclosingScope(node, scopeName);
					if (scope) {
						const detailLabel = "scope:" + scopeName;
						return getScopeContainedNames(scope).map(name => ({
							label: name,
							kind: CompletionItemKind.Field,
							detail: detailLabel
						}));
					}
				}
			}

			return [];
		}
		
		const nearestConstruct = getNearestConstruct(node);
		if (nearestConstruct?.kind === NodeType.functionParameter || nearestConstruct?.kind === NodeType.comment || nearestConstruct?.kind === NodeType.sourceFile) {
			// don't offer completions in function parameter lists `f(a, b, c|)`
			// don't offer completions outside of any construct (in cfm's this is html output space; in cfcs this void space where only comments should go)
			//          actually at root we could offer <cfcomponent> tag completion, and `component ` sugared tag block completion, and that's it
			// don't offer completions inside comments
			return [];
		}
		else if (isExpressionContext(node)) {
			if (isCfScriptTagBlock(node)) {
				let justCfScriptCompletion = false;
				// if we got </cf then we are in an unfinished tag node
				if (node.parent?.kind === NodeType.tag && node.parent?.which === CfTag.Which.end) {
					justCfScriptCompletion = true;
				}
				// if we got got an identifier but the previous text is "</" (not valid in any expression) then just provide a cfscript completion
				else if (node.parent?.kind === NodeType.identifier && node.range.fromInclusive >= 2) {
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

			const allVisibleNames = (function x(node: cfNode | null) {
				const result = new Map<string, number>();
				let scopeDistance = 0; // keep track of "how far away" some name is, in terms of parent scopes; we can then offer closer names first
				while (node) {
					if (node.containedScope) {
						const names = getAllNamesOfScopeDisplay(node.containedScope, "variables", "local", "arguments");
						for (const name of names ){
							result.set(name, scopeDistance);
						}
						scopeDistance++;
					}
					node = node.containedScope?.container || node.parent;
				}
				return result;
			})(node);

			const result : CompletionItem[] = [];
			for (const [key, scopeDistance] of allVisibleNames) { 
				result.push({
					label: key,
					kind: CompletionItemKind.Field,
					// sort the first two scopes to the top of the list; the rest get lexically sorted as one agglomerated scope
					sortText: (scopeDistance === 0 ? 'a' : scopeDistance === 1 ? 'b' : 'c') + scopeDistance
				});
			}

			return result;
		}

		// find the nearest construct - a binary operator, function parameter, etc.
		// 
		function getNearestConstruct(node: cfNode | null) : cfNode | null {
			while (node) {
				switch (node.kind) {
					case NodeType.textSpan:
					case NodeType.terminal:
					case NodeType.identifier:
					case NodeType.indexedAccessChainElement:
						node = node.parent;
						continue;
					default:
						return node;
				}
			}
			return null;
		}

		function getAllNamesOfScopeDisplay(scopeDisplay : ScopeDisplay, ...keys: StaticallyKnownScopeName[]) : string[] {
			const result : string[] = [];
			const targetKeys = keys.length > 0 ? keys : Object.keys(scopeDisplay) as (keyof ScopeDisplay)[];
			for (const key of targetKeys) {
				if (key === "container" || !(key in scopeDisplay)) continue;
				result.push(...getScopeContainedNames(scopeDisplay[key]!));
			}
			return result;
		}

		return [];
	}
);

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
