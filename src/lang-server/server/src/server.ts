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
import { NodeId, SourceFile, Parser, Binder, Node, Diagnostic as cfcDiagnostic, cfmOrCfc, NodeSourceMap, Checker, NodeKind } from "compiler";

import { isCfcTypeWrapper, isFunctionSignature, _Type } from '../../../compiler/types';
import { FileSystem, Project } from "../../../compiler/project";
import { EngineVersions, EngineVersion } from "../../../compiler/engines";
import * as cfls from "../../../services/completions";
import { getAttribute, getAttributeValue, getSourceFile, getTriviallyComputableString, exhaustiveCaseGuard } from '../../../compiler/utils';
import { BinaryOpType, BlockType, DiagnosticKind, FunctionDefinition, NodeFlags, Property } from '../../../compiler/node';
import { Scanner, SourceRange, Token } from '../../../compiler/scanner';
import { LanguageService } from "../../../services/languageService"
import { CflsConfig } from "../../../services/cflsTypes"

type TextDocumentUri = string;
type AbsPath = string;

let languageService! : LanguageService;
const cflsConfig = CflsConfig();

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
		cflsConfig.engineLibAbsPath = params.initializationOptions?.libAbsPath;
	}
	
	languageService = LanguageService(path.join(__dirname, "/cfls-service.js"), path.join(__dirname, "/vscode-adapter.js"));
	languageService.on("diagnostics", (fsPath: AbsPath, diagnostics: unknown[]) => {
		connection.sendDiagnostics({
			uri: URI.file(fsPath).toString(),
			diagnostics: diagnostics as Diagnostic[]
		});
	})

	return result;
});



/** @deprecated use cfRangeToVsRange_scanner */
function cfRangeToVsRange(sourceRange: SourceRange) : Range {
	return {
		start: {line: sourceRange.fromLineInclusive!, character: sourceRange.fromColInclusive!},
		end: {line: sourceRange.toLineInclusive!, character: sourceRange.toColInclusive!},
	}
}

function cfRangeToVsRange_viaScanner(scanner: Scanner, sourceRange: SourceRange) {
	const from = scanner.getAnnotatedChar(sourceRange.fromInclusive);
	const to = scanner.getAnnotatedChar(sourceRange.toExclusive);
	return {
		start: {line: from.line, character: from.col},
		end: {line: to.line, character: to.col}
	}
}

// show where a symbol is defined at
connection.onDefinition((params) : Location[] | undefined  => {
	return undefined;
	// const document = documents.get(params.textDocument.uri);
	// if (!document) return undefined;

	// const fsPath = URI.parse(document.uri).fsPath;
	// const targetIndex = document.offsetAt(params.position);

	// return hoistable(fsPath);

	// function hoistable(absPath: AbsPath) {
	// 	const project = getOwningProjectFromAbsPath(absPath);
	// 	if (!project) return undefined;

	// 	const doc = documents.get(params.textDocument.uri);
	// 	if (!doc) return;

	// 	const sourceFile = project.__unsafe_dev_getFile(fsPath)?.parsedSourceFile;
	// 	if (!sourceFile) return undefined;
		
	// 	const targetNode = project.getInterestingNodeToLeftOfCursor(fsPath, targetIndex);
	// 	if (!targetNode) return undefined;

	// 	const checker = project.__unsafe_dev_getChecker();

	// 	if (targetNode.parent?.kind === NodeKind.functionDefinition && !targetNode.parent.fromTag && targetNode.parent.returnType === targetNode) {
	// 		const symbol = checker.getSymbol(targetNode.parent, sourceFile)
	// 		if (symbol && isFunctionSignature(symbol.symTabEntry.type) && isCfcTypeWrapper(symbol.symTabEntry.type.returns)) {
	// 			return [{
	// 				uri: URI.file(symbol.symTabEntry.type.returns.cfc.absPath).toString(),
	// 				range: {
	// 					start: {line: 0, character: 0},
	// 					end: {line: 0, character: 0},
	// 				}
	// 			}];
	// 		}
	// 		return undefined;
	// 	}

	// 	if (targetNode.kind === NodeKind.simpleStringLiteral) {
	// 		if (targetNode.parent?.kind === NodeKind.tagAttribute && targetNode.parent.canonicalName === "extends") {
	// 			if (targetNode.parent?.parent?.kind === NodeKind.block
	// 					&& targetNode.parent.parent.subType === BlockType.scriptSugaredTagCallBlock
	// 					&& targetNode.parent.parent.name?.token.text.toLowerCase() === "component") {
	// 					if (sourceFile.cfc?.extends) {
	// 						return [{
	// 							uri: URI.file(sourceFile.cfc.extends.absPath).toString(),
	// 							range: {
	// 								start: {line: 0, character: 0},
	// 								end: {line: 0, character: 0},
	// 							}
	// 						}]
	// 					}

	// 			}
	// 		}
	// 		return undefined;
	// 	}

	// 	const newExpr = targetNode.kind === NodeKind.dottedPathRest
	// 		&& targetNode.parent?.kind === NodeKind.dottedPath
	// 		&& targetNode.parent.parent?.kind === NodeKind.callExpression
	// 		&& targetNode.parent.parent.parent?.kind === NodeKind.new
	// 		? targetNode.parent.parent.parent
	// 		: targetNode.kind === NodeKind.dottedPath
	// 		&& targetNode.parent?.kind === NodeKind.callExpression
	// 		&& targetNode.parent.parent?.kind === NodeKind.new
	// 		? targetNode.parent.parent
	// 		: undefined;

	// 	if (newExpr) {
	// 		const type = checker.getCachedEvaluatedNodeType(newExpr, sourceFile);
	// 		if (type && isCfcTypeWrapper(type)) {
	// 			return [{
	// 				uri: URI.file(type.cfc.absPath).toString(),
	// 				range: {
	// 					start: {line: 0, character: 0},
	// 					end: {line: 0, character: 0},
	// 				}
	// 			}]
	// 		}
	// 		return undefined;
	// 	}
		
	// 	const symbol = checker.getSymbol(targetNode, sourceFile);
	// 	if (!symbol || !symbol.symTabEntry.declarations) return undefined;

	// 	const result : Location[] = [];
	// 	for (const decl of symbol.symTabEntry.declarations) {
	// 		const declFile = getSourceFile(decl);
	// 		if (!declFile) continue;
	// 		const declFileUri = URI.file(declFile.absPath);

	// 		switch (decl.kind) {
	// 			case NodeKind.property: {
	// 				const location = getPropertyDefinitionLocation(decl, declFileUri.toString());
	// 				if (!location) continue;
	// 				result.push(location);
	// 				break;
	// 			}
	// 			case NodeKind.functionDefinition: {
	// 				const location = getFunctionDefinitionLocation(decl, declFileUri.toString());
	// 				if (!location) continue;
	// 				result.push(location);
	// 				break;
	// 			}
	// 			case NodeKind.binaryOperator: {
	// 				if (decl.optype !== BinaryOpType.assign) continue;
	// 				const declSourceFile = getSourceFile(decl);
	// 				if (!declSourceFile) continue;
	// 				result.push({
	// 					uri: declFileUri.toString(),
	// 					range: cfRangeToVsRange_viaScanner(declSourceFile.scanner, decl.left.range)
	// 				});
	// 			}
	// 		}
	// 	}

	// 	return result;

		

	// 	function getFunctionDefinitionLocation(node: FunctionDefinition, uri: string) : Location | undefined {
	// 		if (node.fromTag) {
	// 			if (!node.tagOrigin?.startTag?.range) return undefined;
	// 			return {
	// 				uri: uri,
	// 				range: cfRangeToVsRange(node.tagOrigin.startTag.range)
	// 			}
	// 		}
	// 		else {
	// 			if (!node.nameToken) return undefined;
	// 			return {
	// 				uri: uri,
	// 				range: cfRangeToVsRange(node.nameToken.range)
	// 			}
	// 		}
	// 	}

	// 	function getPropertyDefinitionLocation(node: Property, uri: string) : Location | undefined {
	// 		if (node.fromTag) {
	// 			return {
	// 				uri: uri,
	// 				range: cfRangeToVsRange(node.range)
	// 			}
	// 		}
	// 		else {
	// 			const nameAttr = getAttribute(node.attrs, "name");
	// 			if (!nameAttr) return undefined;
	// 			return {
	// 				uri: uri,
	// 				range: cfRangeToVsRange(nameAttr.name.range)
	// 			}
	// 		}
	// 	}
	// }
});

function engineVersionConfigToEngineVersion(key: any) {
	if (typeof key === "string" && EngineVersions.hasOwnProperty(key)) return EngineVersions[key as keyof typeof EngineVersions];
	else return EngineVersions["lucee.5"];
}

function extractConfig(config: Record<string, any> | null) : CflsConfig {
	// engineLibAbsPath doesn't come from config, so we just carry it forward if it exists
	return {
		engineLibAbsPath: cflsConfig?.engineLibAbsPath ?? null,
		// the rest of these are supplied via config
		x_parseTypes: !!config?.x_parseTypes,
		x_genericFunctionInference: !!config?.x_genericFunctionInference,
		x_checkReturnTypes: !!config?.x_checkReturnTypes,
		engineVersion: engineVersionConfigToEngineVersion(config?.engineVersion),
		wireboxConfigFile: config?.wireboxConfigFile ?? null,
		wireboxResolution: config?.wireboxResolution ?? false,
	}
}

connection.onInitialized(async () => {
	if (hasConfigurationCapability) {
		// Register for all configuration changes.
		connection.client.register(DidChangeConfigurationNotification.type, undefined);
		
		const config = await connection.workspace.getConfiguration("cflsp");
		const freshConfig = extractConfig(config);
		languageService.fork(freshConfig, workspaceRoots.map((root) => URI.parse(root.uri).fsPath));
		connection.sendNotification("cflsp/ready");
	}
	else {
		const freshConfig = CflsConfig();
		languageService.fork(freshConfig, workspaceRoots.map((root) => URI.parse(root.uri).fsPath));
		connection.sendNotification("cflsp/ready");
	}

	didForkCfls = true;
	connection.console.info("cflsp server initialized");
});


connection.onDidChangeConfiguration(async change => {
	if (hasConfigurationCapability) {
		// Reset all cached document settings
		const config = await connection.workspace.getConfiguration("cflsp");
		languageService.reset(config);
	}
	else {
		// ? is this reachable if we don't have configurationCapability
	}
});

// Only keep settings for open documents
documents.onDidClose(e => {
	//languageService.closeFile(e.document.uri);
	connection.sendDiagnostics({ uri: e.document.uri, diagnostics: [] });
});

// The content of a text document has changed. This event is emitted
// when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	if (didForkCfls) {
		const textDocument = change.document;
		if (cfmOrCfc(textDocument.uri) !== undefined) {
			const fsPath = URI.parse(textDocument.uri).fsPath;
			languageService.emitDiagnostics(fsPath, textDocument.getText());
		}
	}
});

// function mapCflsCompletionItemKindToVsCodeCompletionItemKind(kind: cfls.CompletionItemKind) : CompletionItemKind {
// 	switch (kind) {
// 		case cfls.CompletionItemKind.function: return CompletionItemKind.Function;
// 		case cfls.CompletionItemKind.structMember: return CompletionItemKind.Field;
// 		case cfls.CompletionItemKind.tagName: return CompletionItemKind.Property;
// 		case cfls.CompletionItemKind.variable: return CompletionItemKind.Variable;
// 		case cfls.CompletionItemKind.stringLiteral: return CompletionItemKind.Constant; // not value
// 		default: exhaustiveCaseGuard(kind);
// 	}
// }

// function mapCflsCompletionToVsCodeCompletion(completion: cfls.CompletionItem) : CompletionItem {
// 	const result : Partial<CompletionItem> = {};
// 	result.label = completion.label;
// 	result.kind = mapCflsCompletionItemKindToVsCodeCompletionItemKind(completion.kind);
// 	if (completion.detail) result.detail = completion.detail;
// 	if (completion.insertText) result.insertText = completion.insertText;
// 	if (completion.sortText) result.sortText = completion.sortText;
// 	if (completion.textEdit) {
// 		result.textEdit = {
// 			insert: cfRangeToVsRange(completion.textEdit.range),
// 			newText: completion.textEdit.newText,
// 			replace: cfRangeToVsRange(completion.textEdit.replace),
// 		}
// 	}
// 	return result as CompletionItem;
// }

// This handler provides the initial list of the completion items.
connection.onCompletion(async (completionParams: CompletionParams): Promise<CompletionItem[]> => {
	return [];
	// const document = documents.get(completionParams.textDocument.uri);
	// if (!document) return [];
	
	// const project = getOwningProjectFromUri(document.uri);
	// if (!project) return [];

	// const fsPath = URI.parse(document.uri).fsPath;
	// const targetIndex = document.offsetAt(completionParams.position);
	// const completions = cfls.getCompletions(
	// 	project,
	// 	fsPath,
	// 	targetIndex,
	// 	completionParams.context?.triggerCharacter ?? null);
	// return completions.map(mapCflsCompletionToVsCodeCompletion);
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
