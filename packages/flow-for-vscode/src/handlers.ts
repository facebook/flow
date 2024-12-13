/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import path from 'path';
import * as vscode from 'vscode';

import { findFlowconfig, findDocumentFlowconfig } from './utils';
import FlowLanguageClient from './FlowLanguageClient';
import FlowClients from './FlowClients';
import toDiagnosticSeverity from './utils/toDiagnosticSeverity';
import Logger from './utils/Logger';
import { FlowLanguageClientConfig } from './FlowLanguageClient/FlowLanguageClient';

const activationDocumentSelector = [
  { scheme: 'file', language: 'javascript' },
  { scheme: 'file', language: 'javascriptreact' },
  { scheme: 'file', pattern: '**/.flowconfig' },
];

export function onDidChangeActiveTextEditor(
  clients: FlowClients,
  activeTextEditor: vscode.TextEditor | null | undefined,
): void {
  clients.setActive(null);

  if (!activeTextEditor) {
    return;
  }

  // ignore if not valid document
  if (
    !vscode.languages.match(
      activationDocumentSelector,
      activeTextEditor.document,
    )
  ) {
    return;
  }

  findDocumentFlowconfig('.flowconfig', activeTextEditor.document).then(
    (flowconfigPath) => {
      if (
        flowconfigPath &&
        // activeTextEditor can change in b/w
        vscode.window.activeTextEditor === activeTextEditor
      ) {
        clients.setActive(clients.get(flowconfigPath));
      }
    },
  );
}

export function onDidOpenTextDocument(
  clients: FlowClients,
  document: vscode.TextDocument,
  outputChannel: vscode.OutputChannel,
  canUseRelativePattern: boolean,
  logger: Logger,
): void {
  if (!vscode.languages.match(activationDocumentSelector, document)) {
    return;
  }

  const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
  if (!workspaceFolder) {
    return;
  }

  const docPath = document.uri.fsPath;
  const rootPath = workspaceFolder.uri.fsPath;
  const startDir = path.dirname(docPath);

  // flow also supports custom flowconfigName
  // not hardcoding '.flowconfig' in code so that we can later add support easily
  const flowconfigName = '.flowconfig';

  logger.trace(`File opened ${document.uri.fsPath}.`);
  logger.trace(`Searching flowconfig for file ${docPath}`);

  // NOTE: currently I'm not caching "findFlowconfig" result to avoid bugs due to caching (say user moves .flowconfig)
  // if this becomes bottleneck in future we can cache findFlowConfig result with some cache invalidation
  findFlowconfig(flowconfigName, startDir, rootPath).then((flowconfigPath) => {
    if (!flowconfigPath) {
      logger.info(
        `Not starting flow client. No ${flowconfigName} found for file ${docPath}.`,
      );
      return;
    }

    logger.trace(`Found flowconfig ${flowconfigPath} for file ${docPath}`);

    if (clients.has(flowconfigPath)) {
      logger.trace(
        `Flow client already exists for flowconfig ${flowconfigPath}.`,
      );
      return;
    }

    logger.trace(`Creating flow client for flowconfig ${flowconfigPath}`);

    const client = new FlowLanguageClient({
      flowconfigPath,
      workspaceRoot: workspaceFolder.uri.fsPath,
      outputChannel,
      canUseRelativePattern,
      // NOTE: passing config as getFunction instead of plain object
      // to add support of handling config change without the requirement of vscode restart
      getConfig: (): FlowLanguageClientConfig => {
        const pluginConfig = vscode.workspace.getConfiguration(
          'flow',
          workspaceFolder.uri,
        );
        return {
          useNPMPackagedFlow: pluginConfig.get('useNPMPackagedFlow')!,
          pathToFlow: pluginConfig.get('pathToFlow')!,
          useBundledFlow: pluginConfig.get('useBundledFlow')!,
          stopFlowOnExit: pluginConfig.get('stopFlowOnExit')!,
          useCodeSnippetOnFunctionSuggest: pluginConfig.get(
            'useCodeSnippetOnFunctionSuggest',
          )!,
          logLevel: pluginConfig.get('logLevel')!,
          lazyMode: pluginConfig.get('lazyMode')!,
          coverage: {
            showUncovered: pluginConfig.get('showUncovered')!,
            diagnosticSeverity: toDiagnosticSeverity(
              pluginConfig.get('coverageSeverity'),
              vscode.DiagnosticSeverity.Information,
            ),
          },
        };
      },
    });

    clients.add(flowconfigPath, client);
    // if document is active also mark client active
    if (
      vscode.window.activeTextEditor &&
      vscode.window.activeTextEditor.document === document
    ) {
      clients.setActive(client);
    }
  });
}

export function onDidRemoveWorkspaceFolders(
  clients: FlowClients,
  folders: ReadonlyArray<vscode.WorkspaceFolder>,
): void {
  folders.forEach((folder) => {
    clients.disposeByWorkspaceFolder(folder);
  });
}

export function onDidChangeConfiguration(): void {
  // @todo instead of asking user to restart vscode
  // handle different config change cases. In most cases
  // we dont need to restart vscode.
  vscode.window
    // $FlowFixMe
    .showInformationMessage(
      'Flow settings changed, reload vscode to apply changes.',
      'Reload',
    )
    .then((selected) => {
      if (selected === 'Reload') {
        vscode.commands.executeCommand('workbench.action.reloadWindow');
      }
    });
}
