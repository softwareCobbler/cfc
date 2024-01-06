/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import {
	createConnection,
	ProposedFeatures,
	InitializeParams,
	DidChangeConfigurationNotification,
	CompletionItem,
	TextDocumentSyncKind,
	InitializeResult,
	CompletionParams,
	WorkspaceFolder,
	TextDocumentContentChangeEvent,
} from 'vscode-languageserver/node';

import { Location } from "vscode-languageserver-types"
import {
	TextDocument
} from 'vscode-languageserver-textdocument';


import { URI } from "vscode-uri";

import { LanguageTool } from "../../../services/languageTool"
import { CflsInitArgs, SerializableCflsConfig } from "../../../services/cflsTypes"
import { SourceRange } from '../../../compiler/scanner';
import { CancellationToken } from '../../../compiler/cancellationToken';

import { adapter as resultsAdapter } from "./vscode-direct-adapter"

const languageTool = LanguageTool();
const knownDocs = new Map</*URI*/string, TextDocument>();
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

	const config = params.initializationOptions.config;
	
	//
	// init with no config, because it's impossible to ask for workspace/configuration from within onInitialize?
	// we get `rejected promise not handled within 1 second: Error: Unhandled method workspace/configuration` if we do
	// `await connection.connection.workspace.getConfiguration("cflsp")` here, but it's ok in "onInitialized" and elsewhere?
	//
	const freshInitArgs = mungeConfig(config);
	languageTool.init({
		config: freshInitArgs,
		cancellationTokenId: cancellationToken.getId(),
		workspaceRoots: workspaceRoots.map((root) => URI.parse(root.uri).fsPath)
	});

	return result;
});
// show where a symbol is defined at
connection.onDefinition(async (params) : Promise<Location[] | undefined> => {
	const doc = knownDocs.get(params.textDocument.uri);
	if (!doc) {
		return undefined;
	}

	const fsPath = URI.parse(doc.uri).fsPath;
	const targetIndex = doc.offsetAt(params.position);

	const maybeDefLocations = languageTool.getDefinitionLocations(fsPath, targetIndex);
	if (maybeDefLocations) {
		const mapped = maybeDefLocations.map(defLoc => {
			const sourceDocUri = URI.parse(defLoc.sourceFile.absPath).toString();
			//
			// we might not know about the doc here, because the langauge service opened it off the file system as dependency, but the
			// user doesn't have it open, so we don't know about it in the list of "open vscode docs".
			//
			const sourceDoc = knownDocs.get(sourceDocUri) || TextDocument.create("", "cfml", -1, defLoc.sourceFile.scanner.getSourceText());
			const posMapper = resultsAdapter.cfPositionToVsPosition(sourceDoc);
			return resultsAdapter.sourceLocation(posMapper, defLoc);
		});
		return mapped;
	}
	else {
		return undefined;
	}
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

connection.onDidChangeConfiguration(async cflsConfig => {
	const freshInitArgs = mungeConfig(cflsConfig.settings.cflsp);
	languageTool.reset(freshInitArgs);
});

function runDiagonstics(uri: string, textDocument: string, sourceRangeIfIncremental?: {sourceRange: SourceRange, changeSize: number}) {
	const doc = knownDocs.get(uri);
	if (!doc) {
		// shouldn't happen
		return;
	}

	const fsPath = URI.parse(uri).fsPath;
	const diagnostics = languageTool.naiveGetDiagnostics(fsPath, textDocument, sourceRangeIfIncremental);
	if (diagnostics) {
		const posMapper = resultsAdapter.cfPositionToVsPosition(doc)
		connection.sendDiagnostics({
			uri,
			diagnostics: diagnostics.diagnostics.map(diagnostic => resultsAdapter.diagnostic(posMapper, diagnostic))
		});
	}
}

connection.onDidOpenTextDocument(v => {
	if (v.textDocument.languageId === 'cfml') {
		knownDocs.set(v.textDocument.uri, TextDocument.create(v.textDocument.uri, v.textDocument.languageId, v.textDocument.version, v.textDocument.text));
		try {
			runDiagonstics(v.textDocument.uri, v.textDocument.text);
		}
		catch (err) {
			console.error("onDidOpen --", err);
		}
	}
})

/**
 * A vscode TextDocumentContentChangeEvent converted into offsets and size info.
 * The [from,to) range is the location of the mutation in the **old document**. A range here can be zero length, meaning some edit
 * directly on a cursor, like a typical character insertion or deletion, e.g.
 *  - "a|b" -> "axb"
 *  - "a|bc" -> "ac"
 *
 *                                                        vvvvvvvvvvvvv ---- toExclusive? or some computed thing as a function of from/to/size?
 * The "size delta" is the distance all characters after `fromInclusive` have moved, in the **new document**, e.g.
 *  - "abc|d" -> "abcxd"  sizeDelta=+1
 *  - "abc|xd" -> "abcd"  sizeDelta=-1
 *
 * ab|cd|ef --> abCDCDef (fromInc=2, toExc=4, sizeDelta=+2)
 *    ^^ paste CDCD
 *
 * ab|cd|ef --> abef (fromInc=2, toExc=4, sizeDelta=-2)
 *    ^^ delete
 *
 */
interface MungedTextDocumentContentChangeEvent {
	fromInclusive: number,
	toExclusive: number,
	sizeDelta: number
}

function contentChangeToXContentChange(doc: TextDocument, incrementalChange: TextDocumentContentChangeEvent) : MungedTextDocumentContentChangeEvent {
	if (!TextDocumentContentChangeEvent.isIncremental(incrementalChange)) {
		throw "should have been filtered prior to getting here";
	}

	const fromInclusive = doc.offsetAt(incrementalChange.range.start);
	const toExclusive = doc.offsetAt(incrementalChange.range.end);
	const sizeDelta = incrementalChange.text.length - (toExclusive - fromInclusive)
	return {fromInclusive, toExclusive, sizeDelta}
}

function mergeContentChangeRange(changes: readonly MungedTextDocumentContentChangeEvent[]) : {affectedTextRange: SourceRange, changeSize: number} | undefined {
	if (changes.length === 0) {
		return undefined;
	}
	else if (changes.length === 1) {
		const change = changes[0];
		return {
			affectedTextRange: new SourceRange(change.fromInclusive, change.toExclusive),
			changeSize: change.sizeDelta
		}
	}
	else {
		const fromInclusive = Math.min(...changes.map(v => v.fromInclusive));
		const toExclusive = Math.max(...changes.map(v => v.toExclusive));

		// change size is different from "affected range",
		// ex. we changed range 100-200, by changeSize=-50, by deleting half of it
		const changeSize = (() => {
			let c = 0;
			changes.forEach(change => { c += change.sizeDelta; })
			return c;
		})();

		return {
			affectedTextRange: new SourceRange(fromInclusive, toExclusive),
			changeSize
		}
	}
}

connection.onDidChangeTextDocument(changes => {
	const doc = knownDocs.get(changes.textDocument.uri)
	
	if (!doc) {
		return;
	}
	
	const x = changes
		.contentChanges
		.filter(TextDocumentContentChangeEvent.isIncremental)
		.map(change => contentChangeToXContentChange(doc, change));
	const y = mergeContentChangeRange(x);

	// works in-place on `doc`
	TextDocument.update(doc, changes.contentChanges, doc.version + 1);

	try {
		runDiagonstics(changes.textDocument.uri, doc.getText(), y  ? {sourceRange: y.affectedTextRange, changeSize: y.changeSize} : undefined);
	}
	catch (err) {
		console.error("onDidChange --", err);
	}
})

connection.onDidCloseTextDocument(v => {
	knownDocs.delete(v.textDocument.uri);
	connection.sendDiagnostics({ uri: v.textDocument.uri, diagnostics: [] });
})


connection.onCompletion((completionParams: CompletionParams): CompletionItem[] => {
	const doc = knownDocs.get(completionParams.textDocument.uri);
	if (!doc) {
		return [];
	}
	
	const fsPath = URI.parse(doc.uri).fsPath;
	const targetIndex = doc.offsetAt(completionParams.position);
	const triggerCharacter = completionParams.context?.triggerCharacter ?? null;

	const items = languageTool.getCompletions(fsPath, targetIndex, triggerCharacter)?.completions
	if (items) {
		const posMapper = resultsAdapter.cfPositionToVsPosition(doc)
		return items.map(item => resultsAdapter.completionItem(posMapper, item));
	}
	else {
		return [];
	}
});

connection.listen();
