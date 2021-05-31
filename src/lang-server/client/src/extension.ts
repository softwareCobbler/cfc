/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	const libAbsPath = "c:/Users/anon/dev/cfc/src/lang-server/server/src/runtimelib/lib.cf2018.d.cfm"//context.asAbsolutePath("./out/lib.cf2018.d.cfm");
	
	// The server is implemented in node
	let serverModule = context.asAbsolutePath("./out/server.js"); 
	
	// The debug options for the server
	// --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
	let debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	let serverOptions: ServerOptions = {
		run: { module: serverModule, transport: TransportKind.ipc },
		debug: {
			module: serverModule,
			transport: TransportKind.ipc,
			options: debugOptions
		}
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
		initializationOptions: "some-data-init-here",
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
	client.onReady().then(() => {
		client.sendNotification("cflsp/load-lib", libAbsPath);
	})
	
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
