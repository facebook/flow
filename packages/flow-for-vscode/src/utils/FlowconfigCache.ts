/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';
import findDocumentFlowconfig from './findDocumentFlowconfig';

export default class FlowconfigCache {
  _cache: Map<string, null | string> = new Map();
  _flowconfigName: string;

  constructor(flowconfigName: string) {
    this._flowconfigName = flowconfigName;
  }

  async get(document: vscode.TextDocument): Promise<null | string> {
    return this.getWithUri(document.uri);
  }

  async getWithUri(uri: vscode.Uri): Promise<null | string> {
    const docPath = uri.fsPath;
    const val = this._cache.get(docPath);
    if (val !== undefined) {
      return Promise.resolve(val);
    }
    // compute
    const flowconfigPath = await findDocumentFlowconfig(
      this._flowconfigName,
      uri,
    );
    this._cache.set(docPath, flowconfigPath);
    return flowconfigPath;
  }

  delete(document: vscode.TextDocument): void {
    this._cache.delete(document.uri.fsPath);
  }
}
