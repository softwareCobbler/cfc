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
	HoverParams,
	Hover,
} from 'vscode-languageserver/node';

import { SignatureHelp, Position, Location, Range } from "vscode-languageserver-types"

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

import { URI } from "vscode-uri";

import { NodeId, SourceFile, Parser, Binder, Node, Diagnostic as cfcDiagnostic, cfmOrCfc, NodeSourceMap, Checker, NodeKind } from "compiler";

import { _Type } from '../../../compiler/types';
import { FileSystem, LanguageVersion, Project } from "../../../compiler/project";
import * as cfls from "../../../services/completions";
import { getAttribute, getAttributeValue, getSourceFile, getTriviallyComputableString } from '../../../compiler/utils';
import { FunctionDefinition, NodeFlags, Property } from '../../../compiler/node';
import { SourceRange, Token } from '../../../compiler/scanner';

type TextDocumentUri = string;

interface CflsConfig {
	engineLibAbsPath: string | null
	x_types: boolean,
	languageVersion: LanguageVersion
}

let project : Project; // init this before using it

let workspaceRoots : WorkspaceFolder[] = [];

let cflsConfig! : CflsConfig;

function naiveGetDiagnostics(uri: TextDocumentUri, freshText: string | Buffer) : cfcDiagnostic[] {
	if (!project) return [];

	const fsPath = URI.parse(uri).fsPath;

	const timing = project.parseBindCheck(fsPath, freshText);
	connection.console.info(`${uri}\n\tparse ${timing.parse} // bind ${timing.bind} // check ${timing.check}`);
	return project.getDiagnostics(fsPath) ?? [];
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
			definitionProvider: true,
			//hoverProvider: true,
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

// show where a symbol is defined at
connection.onDefinition((params) : Location[] | undefined  => {
	const document = documents.get(params.textDocument.uri);
	if (!document) return undefined;
	const fsPath = URI.parse(document.uri).fsPath;
	const targetIndex = document.offsetAt(params.position);

	const doc = documents.get(params.textDocument.uri);
	if (!doc) return;

	const sourceFile = project.__unsafe_dev_getFile(fsPath)?.parsedSourceFile;
	if (!sourceFile) return undefined;
	
	const targetNode = project.getInterestingNodeToLeftOfCursor(fsPath, targetIndex);
	if (!targetNode) return undefined;

	if (targetNode.kind === NodeKind.indexedAccessChainElement) {
		const checker = project.__unsafe_dev_getChecker();
		const symbol = checker.getSymbol(targetNode, sourceFile);
		if (!symbol || !symbol.symTabEntry.declarations) return undefined;

		const result : Location[] = [];
		for (const decl of symbol.symTabEntry.declarations) {
			const declFile = getSourceFile(decl);
			if (!declFile) continue;
			const declFileUri = URI.file(declFile.absPath);

			switch (decl.kind) {
				case NodeKind.property: {
					const location = getPropertyDefinitionLocation(decl, declFileUri.toString());
					if (!location) continue;
					result.push(location);
					break;
				}
				case NodeKind.functionDefinition: {
					const location = getFunctionDefinitionLocation(decl, declFileUri.toString());
					if (!location) continue;
					result.push(location);
					break;
				}
			}
		}

		return result;
	}

	function cfRangeToVsRange(sourceRange: SourceRange) : Range {
		return {
			start: {line: sourceRange.fromLineInclusive!, character: sourceRange.fromColInclusive!},
			end: {line: sourceRange.toLineInclusive!, character: sourceRange.toColInclusive!},
		}
	}

	function getFunctionDefinitionLocation(node: FunctionDefinition, uri: string) : Location | undefined {
		if (node.fromTag) {
			if (!node.tagOrigin?.startTag?.range) return undefined;
			return {
				uri: uri,
				range: cfRangeToVsRange(node.tagOrigin.startTag.range)
			}
		}
		else {
			if (!node.nameToken) return undefined;
			return {
				uri: uri,
				range: cfRangeToVsRange(node.nameToken.range)
			}
		}
	}

	function getPropertyDefinitionLocation(node: Property, uri: string) : Location | undefined {
		if (node.fromTag) {
			return {
				uri: uri,
				range: cfRangeToVsRange(node.range)
			}
		}
		else {
			const nameAttr = getAttribute(node.attrs, "name");
			if (!nameAttr) return undefined;
			return {
				uri: uri,
				range: cfRangeToVsRange(nameAttr.name.range)
			}
		}
	}
});

function resetCflsp(config: CflsConfig, why: string) {
	connection.console.info("[reset] -- " + why);
	connection.console.info("[reset] libPath is: " + cflsConfig.engineLibAbsPath ?? "null");

	project = Project(
		workspaceRoots.map(v => URI.parse(v.uri).fsPath),
		FileSystem(),
		{parseTypes: config.x_types, debug: true, language: config.languageVersion});

	if (cflsConfig.engineLibAbsPath) project.addEngineLib(cflsConfig.engineLibAbsPath);
}

function reemitDiagnostics() {
	const absPaths = project.getFileListing();
	connection.console.info("reemit diagnositcs for " + absPaths.length + " file URIs");
	for (const absPath of absPaths) {
		const uri = URI.parse(absPath).toString();
		const textDocument = documents.get(uri);
		const text = textDocument?.getText();
		const fileType = cfmOrCfc(uri);

		if (text && fileType) {
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
		cflsConfig = {
			engineLibAbsPath: null,
			x_types: false,
			languageVersion: LanguageVersion.lucee5
		};
		resetCflsp(cflsConfig, "onInitialized"); // how to get init'd value instead of waiting on it below ?

		// ok, now we can wait to ask the client for the workspace configuration; this might take a while to complete
		// and completions requests and etc. can be arriving and getting serviced during the wait
		connection.workspace.getConfiguration("cflsp").then((config) => {
			const languageVersion = languageConfigToEnum(config.languageVersion);
			if (languageVersion !== cflsConfig.languageVersion || config.x_types !== cflsConfig.x_types) {
				resetCflsp(config, "onInitialized and after config");
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

function languageConfigToEnum(s: string) {
	switch (s) {
		case "Adobe": return LanguageVersion.acf2018;
		case "Lucee": return LanguageVersion.lucee5;
		default: return LanguageVersion.lucee5; // shouldn't hit this
	}
}

connection.onDidChangeConfiguration(async change => {
	if (hasConfigurationCapability) {
		// Reset all cached document settings
		documentSettings.clear();
		connection.workspace.getConfiguration("cflsp").then((config) => {
			cflsConfig.languageVersion = languageConfigToEnum(config.languageVersion);
			resetCflsp(cflsConfig, "onDidChangeConfiguration");
			documents.all().forEach(validateTextDocument);
		});
	}
	else {
		cflsConfig = {
			languageVersion: LanguageVersion.lucee5,
			x_types: false,
			engineLibAbsPath: cflsConfig.engineLibAbsPath
		};
		resetCflsp(cflsConfig, "onDidChangeConfiguration");

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

function assertExhaustiveCase(_: never) : never {
	throw "non-exhaustive case or unintentional fallthrough";
}

function mapCflsCompletionItemKindToVsCodeCompletionItemKind(kind: cfls.CompletionItemKind) : CompletionItemKind {
	switch (kind) {
		case cfls.CompletionItemKind.function: return CompletionItemKind.Function;
		case cfls.CompletionItemKind.structMember: return CompletionItemKind.Field;
		case cfls.CompletionItemKind.tagName: return CompletionItemKind.Property;
		case cfls.CompletionItemKind.variable: return CompletionItemKind.Variable;
		default: assertExhaustiveCase(kind);
	}
}

function mapCflsCompletionToVsCodeCompletion(completion: cfls.CompletionItem) : CompletionItem {
	const result : Partial<CompletionItem> = {};
	result.label = completion.label;
	result.kind = mapCflsCompletionItemKindToVsCodeCompletionItemKind(completion.kind);
	if (completion.detail) result.detail = completion.detail;
	if (completion.insertText) result.insertText = completion.insertText;
	if (completion.sortText) result.sortText = completion.sortText;
	return result as CompletionItem;
}

// This handler provides the initial list of the completion items.
connection.onCompletion((completionParams: CompletionParams): CompletionItem[] => {
	const document = documents.get(completionParams.textDocument.uri);
	if (!document) return [];
	const fsPath = URI.parse(document.uri).fsPath;
	const targetIndex = document.offsetAt(completionParams.position);
	const completions = cfls.getCompletions(
		project,
		fsPath,
		targetIndex,
		completionParams.context?.triggerCharacter ?? null);
	return completions.map(mapCflsCompletionToVsCodeCompletion);
});

/*
connection.onHover((hoverParams: HoverParams) : Hover | null => {
	const document = documents.get(hoverParams.textDocument.uri);
	if (!document) return null;
	const fsPath = URI.parse(document.uri).fsPath;
	const targetIndex = document.offsetAt(hoverParams.position);

	const targetNode = project.getInterestingNodeToLeftOfCursor(fsPath, targetIndex);
	if (!targetNode) return null;
	if (targetNode.kind === NodeKind.textSpan || targetNode.kind === NodeKind.comment) return null;
	
	const checker = project.__unsafe_dev_getChecker();
	const symbol = checker.getSymbol(targetNode, project.getParsedSourceFile(fsPath)!);
	if (symbol) {
		if (symbol.declarations) {
			const decl = Array.isArray(symbol.declarations)
				? symbol.declarations.length === 0
				? symbol.declarations[0]
				: null
				: symbol.declarations;
			if (!decl || decl.kind !== NodeKind.functionDefinition) return null;
			const attrVal = getAttributeValue(decl.attrs, "hint");
			let text;
			if (attrVal && attrVal.kind === NodeKind.textSpan) {
				text = attrVal.text;
			}
			if (attrVal && attrVal.kind === NodeKind.simpleStringLiteral) {
				text = getTriviallyComputableString(attrVal)!;
			}
			
			if (!text) return null;

			return {
				contents: {
					kind: "plaintext"
					value: text
				}
			}
		}
	}

	return null;
})
*/

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
	connection.console.info("received cflsp/libpath notification, path=" + libAbsPath);
	if (!project) {
		connection.console.warn("Aborting engine library load: `project` was not yet initialized.");
		return;
	}

	cflsConfig.engineLibAbsPath = libAbsPath;

	project.addEngineLib(libAbsPath);
	reemitDiagnostics();

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
	const start = new Date().getTime();
	let addFileTime = 0;
	for (const absPath of cfcAbsPaths) {
		const start = new Date().getTime();
		project.addFile(absPath);
		const elapsed = new Date().getTime() - start;
		connection.console.log(`${absPath} (cachetime): ${elapsed}`);
		addFileTime += elapsed;
		connection.sendNotification("cflsp/cached-cfc"); // just saying "hey we're done with one more"
	}
	const elapsed = (new Date().getTime()) - start;
	connection.console.info("Cached " + cfcAbsPaths.length + " CFCs in " + elapsed + "ms, time spend in addfile: " + addFileTime + "ms");
});

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
