/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// add support using "textDocument/typeCoverage" lsp extension
import * as lsp from 'vscode-languageclient';
import * as vscode from 'vscode';
import * as UUID from 'vscode-languageclient/lib/utils/uuid';

import TypeCoverage from './TypeCoverage';
import { type ILanguageClient } from '../types';
import TypeCoverageProvider, {
  TypeCoverageRequest,
} from './TypeCoverageProvider';

export default class TypeCoverageFeature extends lsp.TextDocumentFeature<lsp.TextDocumentRegistrationOptions> {
  readonly _client: ILanguageClient;

  constructor(client: ILanguageClient) {
    super(client, TypeCoverageRequest.type);
    this._client = client;
  }

  fillClientCapabilities(capabilities: lsp.ClientCapabilities): void {
    // @ts-ignore
    capabilities.telemetry = capabilities.telemetry || {};
    // @ts-ignore
    capabilities.telemetry.connectionStatus = { dynamicRegistration: false };
  }

  initialize(
    capabilities: lsp.ServerCapabilities,
    documentSelector: lsp.DocumentSelector | null | undefined,
  ): void {
    // @ts-ignore
    if (!capabilities.typeCoverageProvider || !documentSelector) {
      return;
    }

    this.register(this.messages, {
      id: UUID.generateUuid(),
      registerOptions: { documentSelector },
    });
  }

  registerLanguageProvider(
    options: lsp.TextDocumentRegistrationOptions,
  ): vscode.Disposable {
    const provider = new TypeCoverageProvider(this._client);
    return new TypeCoverage(
      options.documentSelector,
      provider,
      this._client.clientOptions.extensions.typeCoverage,
    );
  }
}
