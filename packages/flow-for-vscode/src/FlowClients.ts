/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';
import FlowLanguageClient from './FlowLanguageClient';
import Logger from './utils/Logger';

export default class FlowClients {
  _clients: Map<string, FlowLanguageClient> = new Map();
  _logger: Logger;

  _activeClient: FlowLanguageClient | null | undefined = undefined;

  constructor(logger: Logger) {
    this._logger = logger;
  }

  add(flowconfig: string, client: FlowLanguageClient): void {
    this._clients.set(flowconfig, client);
  }

  has(flowconfig: string): boolean {
    return this._clients.has(flowconfig);
  }

  get(flowconfig: string): FlowLanguageClient | undefined {
    return this._clients.get(flowconfig);
  }

  // will dispose all clients
  dispose(): Promise<void> {
    this._logger.trace('Disposing all clients');
    const promises: Promise<void>[] = [];
    this._clients.forEach((client) => {
      promises.push(this._disposeClient(client));
    });
    return Promise.all(promises).then(() => undefined);
  }

  // will dispose all clients under given workspaceFolder
  disposeByWorkspaceFolder(folder: vscode.WorkspaceFolder): void {
    const workspaceClients = this._getClientByWorkspaceFolder(folder);
    this._logger.trace(
      `Disposing all clients of workspaceFolder '${folder.uri.fsPath}'`,
    );
    workspaceClients.forEach((client) => {
      this._disposeClient(client);
    });
  }

  setActive(client: FlowLanguageClient | null | undefined): void {
    if (this._activeClient) {
      this._activeClient.setActive(false);
    }

    this._activeClient = client;
    if (this._activeClient) {
      this._activeClient.setActive(true);
    }
  }

  getActive(): FlowLanguageClient | null | undefined {
    return this._activeClient;
  }

  pick(placeHolder: string): PromiseLike<FlowLanguageClient | null> {
    const items: Array<{
      label: string;
      description: string;
      client: FlowLanguageClient;
    }> = [];
    this._clients.forEach((client) => {
      const isActiveClient = client === this._activeClient;
      const item = {
        label: client.getName(),
        description: isActiveClient ? 'active editor client' : '',
        client,
      };
      if (isActiveClient) {
        // always keep active client on top
        items.unshift(item);
      } else {
        items.push(item);
      }
    });

    if (items.length === 0) {
      vscode.window.showErrorMessage('No flow client found');
      return Promise.resolve(null);
    }

    // if only one client present directly pick
    if (items.length === 1) {
      return Promise.resolve(items[0].client);
    }

    return vscode.window
      .showQuickPick(items, { placeHolder })
      .then((selectedItem) => {
        if (selectedItem) {
          return selectedItem.client;
        }
        return null;
      });
  }

  _disposeClient(client: FlowLanguageClient): Promise<void> {
    if (this._activeClient === client) {
      this.setActive(null);
    }
    this._clients.delete(client.getFlowconfig());
    client.getLogger().info('disposing client');
    return client.dispose();
  }

  _getClientByWorkspaceFolder(
    workspaceFolder: vscode.WorkspaceFolder,
  ): Array<FlowLanguageClient> {
    const clients: Array<FlowLanguageClient> = [];
    this._clients.forEach((client) => {
      if (client.getWorkspaceRoot() === workspaceFolder.uri.fsPath) {
        clients.push(client);
      }
    });
    return clients;
  }
}
