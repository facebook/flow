/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';
import * as lsp from 'vscode-languageclient';
import {
  type TypeCoverageResult,
  type TypeCoverageParams,
  type ConnectionStatusParams,
} from './types';
import { type ILanguageClient } from '../types';

type TypeCoverageRequestType = lsp.RequestType<
  TypeCoverageParams,
  TypeCoverageResult | void,
  void,
  lsp.TextDocumentRegistrationOptions
>;

export const TypeCoverageRequest = {
  type: new lsp.RequestType<
    TypeCoverageParams,
    TypeCoverageResult | void,
    void,
    lsp.TextDocumentRegistrationOptions
  >('textDocument/typeCoverage') as TypeCoverageRequestType,
};

const ConnectionStatusNotification = {
  type: new lsp.NotificationType<ConnectionStatusParams, void>(
    'telemetry/connectionStatus',
  ),
};

type ConnectionStatusListener = (params: ConnectionStatusParams) => void;

export default class TypeCoverageProvider {
  _client: ILanguageClient;
  _listeners: Array<ConnectionStatusListener> = [];

  constructor(client: ILanguageClient) {
    this._client = client;
    this._client.onNotification(
      ConnectionStatusNotification.type,
      this._handleConnectionStatus,
    );
  }

  onConnectionStatus = (
    listener: ConnectionStatusListener,
  ): vscode.Disposable => {
    this._listeners.push(listener);
    // dispose
    return {
      dispose: () => {
        const index = this._listeners.findIndex(
          (_listener) => listener === _listener,
        );
        if (index !== -1) {
          this._listeners.splice(index, 1);
        }
      },
    };
  };

  provideTypeCoverage = (
    document: vscode.TextDocument,
  ): vscode.ProviderResult<TypeCoverageResult | null | void> => {
    const { middleware } = this._client.clientOptions;
    return middleware && middleware.provideTypeCoverage
      ? middleware.provideTypeCoverage(document, this._provideTypeCoverage)
      : this._provideTypeCoverage(document);
  };

  _provideTypeCoverage = (
    document: vscode.TextDocument,
  ): vscode.ProviderResult<TypeCoverageResult | null> => {
    const client = this._client;

    return client
      .sendRequest(TypeCoverageRequest.type, {
        textDocument:
          client.code2ProtocolConverter.asTextDocumentIdentifier(document),
      })
      .then(
        (coverage) => coverage ?? null,
        (error) => {
          client.logFailedRequest(TypeCoverageRequest.type, error);
          return Promise.resolve(null);
        },
      );
  };

  _handleConnectionStatus = (params: ConnectionStatusParams): void => {
    this._listeners.forEach((listener) => {
      listener(params);
    });
  };
}
