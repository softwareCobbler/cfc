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

import { SourceFile, Parser, Binder, Node as cfNode, binarySearch, CfFileType, Diagnostic as cfcDiagnostic, cfmOrCfc, flattenTree, getScopeContainedNames, NodeSourceMap, isExpressionContext, getTriviallyComputableString } from "compiler";
import { NodeType } from '../../../compiler/node';

const parser = Parser().setDebug(true);
const binder = Binder().setDebug(true);
type TextDocumentUri = string;
const lastParse = new Map<TextDocumentUri, {parsedSourceFile: SourceFile, flatTree: NodeSourceMap[]}>();

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
	
	lastParse.set(uri, {parsedSourceFile: sourceFile, flatTree: flattenTree(sourceFile)});

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
		const docCache = lastParse.get(textDocumentPosition.textDocument.uri)!;
		let match = binarySearch(
			docCache.flatTree,
			(v) => {
				if (v.range.fromInclusive <= targetIndex && targetIndex < v.range.toExclusive) {
					// match: on or in the target index
					return 0;
				}
				else if (v.range.toExclusive < targetIndex) {
					return -1;
				}
				else {
					return 1;
				}
			});

		match = match < 0 ? ~match : match;
		const node = binder.NodeMap.get(docCache.flatTree[match].nodeId);

		if (!node) return [];

		if (textDocumentPosition.context?.triggerKind === CompletionTriggerKind.TriggerCharacter && textDocumentPosition.context.triggerCharacter === ".") {
			if (node.type === NodeType.indexedAccessChainElement && node.parent?.type === NodeType.indexedAccess) {
				const name = getTriviallyComputableString(node.parent.root)?.toLowerCase();
				if (name === "cgi") {
					if (docCache.parsedSourceFile.containedScope?.cgi) {
						return getScopeContainedNames(docCache.parsedSourceFile.containedScope.cgi).map(name => ({
							label: name,
							kind: CompletionItemKind.Field,
							detail: "scope:url"
						}));
					}
				}
			}
		}

		// The pass parameter contains the position of the text document in
		// which code complete got requested. For the example we ignore this
		// info and always provide the same completion items.
		return [
			{
				label: 'TypeScript',
				kind: CompletionItemKind.Variable,
				detail: "scope:variables"
				//data: 1
			},
			{
				label: 'JavaScript',
				kind: CompletionItemKind.Text,
				detail: "scope:url",
				//data: 2
			},
			{
				label: 'encodeForHTML',
				kind: CompletionItemKind.Function,
				detail: "scope:cf-builtin"
				//data: 2
			}
		];
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
