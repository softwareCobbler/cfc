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
import { adapter as VsClientAdapter } from "./vscode-adapter";
import { CflsInitArgs, SerializableCflsConfig } from "../../../services/cflsTypes"

type TextDocumentUri = string;
type AbsPath = string;

let languageService! : LanguageService<typeof VsClientAdapter>;

// unwrap is required to make it harder to accidentally use in a position where we wanted a local with a similar name
const initArgs = Object.freeze((() => {
	const value = CflsInitArgs();
	return {unwrap: () => value};
})());

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
let connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
let documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
let workspaceRoots : WorkspaceFolder[] = [];

let hasConfigurationCapability: boolean = false;
let hasWorkspaceFolderCapability: boolean = false;
let hasDiagnosticRelatedInformationCapability: boolean = false;
let didForkCfls = false;

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
			completionProvider: {triggerCharacters: ["\"", "'", ".", " ", "("]},
			//signatureHelpProvider: {triggerCharacters: ["("]},
			definitionProvider: true,
			//hoverProvider: true,
		}
	};

	if (typeof params.initializationOptions?.libAbsPath === "string") {
		initArgs.unwrap().engineLibAbsPath = params.initializationOptions?.libAbsPath;
	}
	
	languageService = LanguageService<typeof VsClientAdapter>();
	languageService.on("diagnostics", (fsPath: AbsPath, diagnostics: unknown[]) => {
		connection.sendDiagnostics({
			uri: URI.file(fsPath).toString(),
			diagnostics: diagnostics as Diagnostic[]
		});
	})

	return result;
});

// show where a symbol is defined at
connection.onDefinition(async (params) : Promise<Location[] | undefined> => {
	const document = documents.get(params.textDocument.uri);
	if (!document) return undefined;

	const fsPath = URI.parse(document.uri).fsPath;
	const targetIndex = document.offsetAt(params.position);

	return languageService.getDefinitionLocations(fsPath, targetIndex);
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
	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, {section: "cflsp"});
		
		const freshInitArgs = mungeConfig(await connection.workspace.getConfiguration("cflsp"));
		await languageService.fork(freshInitArgs, workspaceRoots.map((root) => URI.parse(root.uri).fsPath));
	}
	else {
		const freshInitArgs = mungeConfig(null);
		await languageService.fork(freshInitArgs, workspaceRoots.map((root) => URI.parse(root.uri).fsPath));
	}

	didForkCfls = true;
	connection.console.info("cflsp server initialized");

	for (const doc of documents.all()) {
		if (doc.languageId === "cfml") {
			languageService.trackFile(URI.parse(doc.uri).fsPath);
			runDiagonstics(doc);
		}
	}
});

function runDiagonstics(textDocument: TextDocument) {
	if (didForkCfls) {
		const fsPath = URI.parse(textDocument.uri).fsPath;
		languageService.emitDiagnostics(fsPath, textDocument.getText());
	}
}

connection.onDidChangeConfiguration(async cflsConfig => {
	if (didForkCfls) {
		const freshInitArgs = mungeConfig(cflsConfig.settings.cflsp);
		languageService.reset(freshInitArgs);
	}
});

// Only keep settings for open documents
documents.onDidClose(e => {
	if (didForkCfls) {
		const fsPath = URI.parse(e.document.uri).fsPath;
		languageService.untrackFile(fsPath);
		connection.sendDiagnostics({ uri: e.document.uri, diagnostics: [] });
	}
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	if (change.document.languageId === "cfml") {
		const fsPath = URI.parse(change.document.uri).fsPath;
		languageService.trackFile(fsPath);
		runDiagonstics(change.document);
	}
});

connection.onCompletion(async (completionParams: CompletionParams): Promise<CompletionItem[]> => {
	const document = documents.get(completionParams.textDocument.uri);
	if (!document) return [];
	
	if (didForkCfls) {
		const fsPath = URI.parse(document.uri).fsPath;
		const targetIndex = document.offsetAt(completionParams.position);
		const triggerCharacter = completionParams.context?.triggerCharacter ?? null;

		return languageService.getCompletions(fsPath, targetIndex, triggerCharacter);
	}
	else {
		return [];
	}
});

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

// connection.onNotification("cflsp/cache-cfcs", (cfcAbsPaths: string[]) => {
// 	const start = new Date().getTime();
// 	let addFileTime = 0;
// 	let i = 0;
// 	for (const absPath of cfcAbsPaths) {
// 		connection.console.log(`Staring ${absPath}...`);
// 		const project = getOwningProjectFromAbsPath(path.parse(absPath).dir);
// 		if (!project) continue;
// 		const start = new Date().getTime();
// 		project.addFile(absPath);
// 		const elapsed = new Date().getTime() - start;
// 		connection.console.log(`${absPath} (cachetime): ${elapsed}`);
// 		addFileTime += elapsed;
// 		connection.sendNotification("cflsp/cached-cfc"); // just saying "hey we're done with one more"
// 	}
// 	const elapsed = (new Date().getTime()) - start;
// 	connection.console.info("Cached " + cfcAbsPaths.length + " CFCs in " + elapsed + "ms, time spend in addfile: " + addFileTime + "ms");
// });

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
