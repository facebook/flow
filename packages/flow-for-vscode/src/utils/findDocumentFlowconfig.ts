/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';
import path from 'path';
import findFlowconfig from './findFlowconfig';

export default function findDocumentFlowconfig(
  flowconfigName: string,
  document: vscode.TextDocument,
): Promise<null | string> {
  const workspace = vscode.workspace.getWorkspaceFolder(document.uri);
  if (!workspace) {
    return Promise.resolve(null);
  }
  const startDir = path.dirname(document.uri.fsPath);
  const rootPath = workspace.uri.fsPath;
  return findFlowconfig(flowconfigName, startDir, rootPath);
}
