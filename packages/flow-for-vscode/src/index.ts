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
import {
  AnsiDecorationProvider,
  DIAGNOSTICS_URI_SCHEME,
  ErrorLinkProvider,
  TextDocumentProvider,
} from './FlowLanguageClient/DetailedDiagnostics';
import { SemanticDecorationProvider } from './FlowLanguageClient/SemanticDecorations';
import FlowconfigCache from './utils/FlowconfigCache';

export function activate(context: vscode.ExtensionContext): void {
  const outputChannel = vscode.window.createOutputChannel('Flow');
  const logger = new Logger('', outputChannel, 'error');
  const clients = new FlowClients(logger);
  const flowconfigCache = new FlowconfigCache('.flowconfig');
  const commands = new PluginCommands(clients, outputChannel);
  const canUseRelativePattern = checkRelativePatternSupported(context);

  logger.info('Open javascript or flowconfig to start flow.');

  const textDocumentProvider = new TextDocumentProvider(
    clients,
    flowconfigCache,
  );
  const errorLinkProvider = new ErrorLinkProvider(clients, flowconfigCache);
  const ansiDecorationProvider = new AnsiDecorationProvider(
    clients,
    flowconfigCache,
  );
  const semanticDecorationProvider = new SemanticDecorationProvider(
    clients,
    flowconfigCache,
  );

  const decorateVisibleEditors = (document: vscode.TextDocument) => {
    for (const editor of vscode.window.visibleTextEditors) {
      if (document === editor.document) {
        ansiDecorationProvider.provideDecorations(editor);
        semanticDecorationProvider.provideDecorations(editor);
      }
    }
  };

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

    vscode.languages.registerDocumentLinkProvider(
      { scheme: DIAGNOSTICS_URI_SCHEME },
      errorLinkProvider,
    ),
    vscode.workspace.registerTextDocumentContentProvider(
      DIAGNOSTICS_URI_SCHEME,
      textDocumentProvider,
    ),
    vscode.workspace.onDidChangeTextDocument((event) =>
      decorateVisibleEditors(event.document),
    ),
    vscode.workspace.onDidOpenTextDocument(decorateVisibleEditors),
    vscode.window.onDidChangeActiveTextEditor((editor) => {
      if (editor) {
        textDocumentProvider.triggerUpdate(editor.document.uri);
        decorateVisibleEditors(editor.document);
      }
    }),
    vscode.window.onDidChangeVisibleTextEditors((visibleEditors) => {
      visibleEditors.forEach(async (editor) => {
        textDocumentProvider.triggerUpdate(editor.document.uri);
        await ansiDecorationProvider.provideDecorations(editor);
        await semanticDecorationProvider.provideDecorations(editor);
      });
    }),
    textDocumentProvider,
    ansiDecorationProvider,
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
