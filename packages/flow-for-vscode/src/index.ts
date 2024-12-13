/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';

import FlowClients from './FlowClients';

import PluginCommands from './PluginCommands';
import * as handlers from './handlers';

import checkRelativePatternSupported from './utils/checkRelativePatternSupported';
import Logger from './utils/Logger';

export function activate(context: vscode.ExtensionContext): void {
  const outputChannel = vscode.window.createOutputChannel('Flow');
  const logger = new Logger('', outputChannel, 'error');
  const clients = new FlowClients(logger);
  const commands = new PluginCommands(clients, outputChannel);
  const canUseRelativePattern = checkRelativePatternSupported(context);

  logger.info('Open javascript or flowconfig to start flow.');

  context.subscriptions.push(
    clients,
    // handlers
    vscode.workspace.onDidOpenTextDocument((document) => {
      handlers.onDidOpenTextDocument(
        clients,
        document,
        outputChannel,
        canUseRelativePattern,
        logger,
      );
    }),
    vscode.window.onDidChangeActiveTextEditor((editor) => {
      handlers.onDidChangeActiveTextEditor(clients, editor);
    }),
    vscode.workspace.onDidChangeWorkspaceFolders((event) => {
      // NOTE: as we are lazily starting flow clients
      // so no need to handle 'added' case
      if (event.removed.length > 0) {
        handlers.onDidRemoveWorkspaceFolders(clients, event.removed);
      }
    }),
    vscode.workspace.onDidChangeConfiguration((config) => {
      if (config.affectsConfiguration('flow')) {
        handlers.onDidChangeConfiguration();
      }
    }),
    commands,
    outputChannel,
  );

  // create flow clients for currently opened documents
  vscode.workspace.textDocuments.forEach((document) => {
    handlers.onDidOpenTextDocument(
      clients,
      document,
      outputChannel,
      canUseRelativePattern,
      logger,
    );
  });
}
