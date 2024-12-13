/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';
import FlowClients from './FlowClients';

export default class PluginCommands {
  _clients: FlowClients;
  _outputChannel: vscode.OutputChannel;

  _subscriptions: Array<vscode.Disposable> = [];

  constructor(clients: FlowClients, outputChannel: vscode.OutputChannel) {
    this._clients = clients;
    this._outputChannel = outputChannel;
    this._registerCommands();
  }

  dispose(): void {
    this._subscriptions.forEach((item) => {
      item.dispose();
    });
  }

  _registerCommands(): void {
    /* prettier-ignore */
    this._subscriptions.push(
      vscode.commands.registerCommand('flow.toggleCoverage', () => this.toggleCoverage(), this),
      vscode.commands.registerCommand('flow.showStatus', () => this.showStatus(), this),
      vscode.commands.registerCommand('flow.restartClient', () => this.restartClient(), this),
      vscode.commands.registerCommand('flow.logClientDebugInfo', () => this.logClientDebugInfo(), this),
      vscode.commands.registerCommand('flow.showOutputChannel', () => this.showOutputChannel(), this),
    );
  }

  showStatus(): void {
    this._clients.pick('Select a client to show status').then((client) => {
      if (client) {
        vscode.commands.executeCommand(client.commands.showStatus);
      }
    });
  }

  toggleCoverage(): void {
    this._clients.pick('Select a client to toggle coverage').then((client) => {
      if (client) {
        vscode.commands.executeCommand(client.commands.toggleCoverage);
      }
    });
  }

  restartClient(): void {
    this._clients.pick('Select a client to restart').then((client) => {
      if (client) {
        vscode.commands.executeCommand(client.commands.restartClient);
      }
    });
  }

  logClientDebugInfo(): void {
    this._clients.pick('Select a client to log debug info').then((client) => {
      if (client) {
        vscode.commands.executeCommand(client.commands.logDebugInfo);
      }
    });
  }

  showOutputChannel(): void {
    this._outputChannel.show(true);
  }
}
