/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
// tslint:disable
'use strict';

import { workspace, Disposable, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, SettingMonitor, ServerOptions, TransportKind, InitializeParams } from 'vscode-languageclient/node';
import { Trace } from 'vscode-jsonrpc';

export function activate(context: ExtensionContext) {

    // The server is implemented in node
    let serverExe = 'dotnet';

    // If the extension is launched in debug mode then the debug server options are used
    // Otherwise the run options are used
    let serverOptions: ServerOptions = {
        run: { command: serverExe, args: ['C:\\Users\\atadi\\source\\repos\\Juniper\\juniper-repo\\Juniper.LanguageServer\\bin\\Debug\\net6.0\\Juniper.LanguageServer.dll'] },
        debug: { command: serverExe, args: ['C:\\Users\\atadi\\source\\repos\\Juniper\\juniper-repo\\Juniper.LanguageServer\\bin\\Debug\\net6.0\\Juniper.LanguageServer.dll'] }
    }

    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        // Register the server for plain text documents
        documentSelector: [
            {
                pattern: '**/*.jun',
            }
        ],
        synchronize: {
            // Synchronize the setting section 'languageServerExample' to the server
            configurationSection: 'languageServerExampleJuniper',
            fileEvents: workspace.createFileSystemWatcher('**/*.jun')
        },
    }

    // Create the language client and start the client.
    const client = new LanguageClient('languageServerExampleJuniper', 'Juniper Language Server Example', serverOptions, clientOptions);
    client.registerProposedFeatures();
    //client.trace = Trace.Verbose;
    let disposable = client.start();

    // Push the disposable to the context's subscriptions so that the
    // client can be deactivated on extension deactivation
    context.subscriptions.push({ dispose: function() {return disposable;}});
}