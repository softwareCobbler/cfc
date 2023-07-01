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
	TextDocumentContentChangeEvent,
} from 'vscode-languageserver/node';

import * as child_process from "child_process"
import { SignatureHelp, Position, Location, Range } from "vscode-languageserver-types"
import {
	TextDocument
} from 'vscode-languageserver-textdocument';


import { URI } from "vscode-uri";

import * as path from "path";
//import { NodeId, SourceFile, Parser, Binder, Node, Diagnostic as cfcDiagnostic, cfmOrCfc, NodeSourceMap, Checker, NodeKind } from "compiler";

// import { isCfcTypeWrapper, isFunctionSignature, _Type } from '../../../compiler/types';
// import { FileSystem, Project } from "../../../compiler/project";
import { EngineVersions, EngineVersion } from "../../../compiler/engines";
// import * as cfls from "../../../services/completions";
// import { Scanner, SourceRange, Token } from '../../../compiler/scanner';
import { LanguageService } from "../../../services/languageService"
import { LanguageTool } from "../../../services/languageTool"
import { adapter as VsClientAdapter } from "./vscode-adapter";
import { CflsInitArgs, SerializableCflsConfig } from "../../../services/cflsTypes"
import { SourceRange } from '../../../compiler/scanner';
import { CancellationToken } from '../../../compiler/cancellationToken';

const languageTool = LanguageTool();
const knownDocs = new Map<string, TextDocument>();
const cancellationToken = CancellationToken();

// unwrap is required to make it harder to accidentally use in a position where we wanted a local with a similar name
const initArgs = Object.freeze((() => {
	const value = CflsInitArgs();
	return {unwrap: () => value};
})());

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
let connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.

let workspaceRoots : WorkspaceFolder[] = [];

let hasConfigurationCapability: boolean = false;
let hasWorkspaceFolderCapability: boolean = false;
let hasDiagnosticRelatedInformationCapability: boolean = false;

connection.onInitialize(async (params: InitializeParams) => {
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
			completionProvider: {triggerCharacters: ["\"", "'", ".", " ", "("]},
			//signatureHelpProvider: {triggerCharacters: ["("]},
			definitionProvider: true,
			//hoverProvider: true,
		}
	};

	if (typeof params.initializationOptions?.libAbsPath === "string") {
		initArgs.unwrap().engineLibAbsPath = params.initializationOptions?.libAbsPath;
	}
	
	//
	// init with no config, because it's impossible to ask for workspace/configuration from within onInitialize?
	// we get `rejected promise not handled within 1 second: Error: Unhandled method workspace/configuration` if we do
	// `await connection.connection.workspace.getConfiguration("cflsp")` here, but it's ok in "onInitialized" and elsewhere?
	//
	const freshInitArgs = mungeConfig(null);
	languageTool.init({
		config: freshInitArgs,
		cancellationTokenId: cancellationToken.getId(),
		workspaceRoots: workspaceRoots.map((root) => URI.parse(root.uri).fsPath)
	});

	return result;
});
// show where a symbol is defined at
connection.onDefinition(async (params) : Promise<Location[] | undefined> => {
	const document = knownDocs.get(params.textDocument.uri);
	if (!document) return undefined;

	const fsPath = URI.parse(document.uri).fsPath;
	const targetIndex = document.offsetAt(params.position);

	const result = languageTool.getDefinitionLocations(fsPath, targetIndex);
	return (result as any) ?? []

});

function mungeConfig(config: Record<string, any> | null) : SerializableCflsConfig {
	// engineLibAbsPath doesn't come from config, so we just carry it forward if it exists
	return {
		engineLibAbsPath: initArgs.unwrap().engineLibAbsPath ?? null,
		// the rest of these are supplied via config
		x_parseTypes: !!config?.x_parseTypes,
		x_genericFunctionInference: !!config?.x_genericFunctionInference,
		x_checkReturnTypes: !!config?.x_checkReturnTypes,
		x_checkFlowTypes: !!config?.x_checkFlowTypes,
		engineVersion: config?.engineVersion ?? "lucee.5",
		wireboxResolution: config?.wireboxResolution ?? false,
		cfConfigProjectRelativePath: config?.cfConfigProjectRelativePath ?? null,
	}
}

connection.onInitialized(async (x) => {
	knownDocs.clear();

	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, {section: "cflsp"});
		
		const freshInitArgs = mungeConfig(await connection.workspace.getConfiguration("cflsp"));
		languageTool.init({
			config: freshInitArgs,
			cancellationTokenId: cancellationToken.getId(),
			workspaceRoots: workspaceRoots.map((root) => URI.parse(root.uri).fsPath)
		});
	}
	else {
		const freshInitArgs = mungeConfig(null);
		languageTool.init({
			config: freshInitArgs,
			cancellationTokenId: cancellationToken.getId(),
			workspaceRoots: workspaceRoots.map((root) => URI.parse(root.uri).fsPath)
		});
	}

	connection.console.info("cflsp server initialized");

	// will fire in response to connection.workspace.getConfiguration (??)
	// so don't register until after we've first run that at least once, otherwise, we end up in it,
	// and call reset, before we've ever called init, and invariants don't hold.

	for (const doc of knownDocs.values()) {
		runDiagonstics(doc.uri, doc.getText());
	}
});

connection.onDidChangeConfiguration(async cflsConfig => {
	const freshInitArgs = mungeConfig(cflsConfig.settings.cflsp);
	languageTool.reset(freshInitArgs);
});

function runDiagonstics(uri: string, textDocument: string, sourceRangeIfIncremental?: SourceRange) {
	const fsPath = URI.parse(uri).fsPath;
	const diagnostics = languageTool.naiveGetDiagnostics(fsPath, textDocument, sourceRangeIfIncremental);
	if (diagnostics) {
		connection.sendDiagnostics({
			uri: URI.file(fsPath).toString(),
			diagnostics: diagnostics.diagnostics as any
		});
	}
}

connection.onDidOpenTextDocument(v => {
	if (v.textDocument.languageId === 'cfml') {
		knownDocs.set(v.textDocument.uri, TextDocument.create(v.textDocument.uri, v.textDocument.languageId, v.textDocument.version, v.textDocument.text));
		runDiagonstics(v.textDocument.uri, v.textDocument.text);
	}
})

connection.onDidChangeTextDocument(changes => {
	const doc = knownDocs.get(changes.textDocument.uri)
	
	if (!doc) {
		return;
	}
	
	const maybeSingleEditSourceRange = changes.contentChanges.length === 1
		&& TextDocumentContentChangeEvent.isIncremental(changes.contentChanges[0])
		? new SourceRange(doc.offsetAt(changes.contentChanges[0].range.start), doc.offsetAt(changes.contentChanges[0].range.end))
		: undefined;

	const freshDoc = TextDocument.update(doc, changes.contentChanges, doc.version + 1);

	// Are the updates performed in-place? If so this is unnecessary.
	knownDocs.set(doc.uri, doc);

	runDiagonstics(changes.textDocument.uri, freshDoc.getText(), maybeSingleEditSourceRange);
})

connection.onDidCloseTextDocument(v => {
	knownDocs.delete(v.textDocument.uri);
	connection.sendDiagnostics({ uri: v.textDocument.uri, diagnostics: [] });
})


connection.onCompletion((completionParams: CompletionParams): CompletionItem[] => {
	const document = knownDocs.get(completionParams.textDocument.uri);
	if (!document) {
		return [];
	}
	
	const fsPath = URI.parse(document.uri).fsPath;
	const targetIndex = document.offsetAt(completionParams.position);
	const triggerCharacter = completionParams.context?.triggerCharacter ?? null;

	const items = languageTool.getCompletions(fsPath, targetIndex, triggerCharacter)?.completionItems
	return (items as any) ?? []
});

connection.listen();
