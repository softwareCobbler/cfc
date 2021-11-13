/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import * as fs from "fs";
import { window, ProgressLocation, workspace, ExtensionContext } from 'vscode';
import { URI } from "vscode-uri";

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;
let libAbsPath : string | null = null;

export function activate(context: ExtensionContext) {
	libAbsPath = context.asAbsolutePath("./out/lib.cf2018.d.cfm");
	
	// The server is implemented in node
	let serverModule = context.asAbsolutePath("./out/server.js"); 
	
	// The debug options for the server
	// --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
	let debugOptions = { execArgv: ['--nolazy', '--inspect=6011', '--inspect-brk'] };

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	let serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
			options: debugOptions
		},
	};

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		// Register the server for plain text documents
		documentSelector: [
			//{ scheme: 'file', language: 'plaintext' },
			{ scheme: 'file', language: 'cfml' }
		],
		synchronize: {
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
		},
		initializationOptions: {
			libAbsPath
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		"cflsp",
		"cflsp",
		serverOptions,
		clientOptions
	);

	
	// Start the client. This will also launch the server
	client.start();
}

function recursiveGetFiles(root: string, pattern: RegExp) : string [] {
	const result : string[] = [];
	const fds = fs.readdirSync(root, {withFileTypes: true});
	for (const fd of fds) {
		if (fd.isDirectory()) result.push(...recursiveGetFiles(path.resolve(root, fd.name), pattern));
		else if (pattern.test(fd.name)) {
			const fspath = path.resolve(root, fd.name);
			result.push(fspath);
		}
	}
	return result;
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
