/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// add support using "textDocument/typeCoverage" lsp extension
import * as lsp from 'vscode-languageclient/node';
import * as vscode from 'vscode';
import * as UUID from 'vscode-languageclient/lib/common/utils/uuid';
import TypeCoverage from './TypeCoverage';
import { type ILanguageClient } from '../types';
import TypeCoverageProvider from './TypeCoverageProvider';

export default class TypeCoverageFeature extends lsp.DynamicDocumentFeature<
  lsp.TextDocumentRegistrationOptions,
  {}
> {
  readonly _client: ILanguageClient;
  private coverage: TypeCoverage | null = null;

  constructor(client: ILanguageClient) {
    super(client);
    this._client = client;
  }
  registrationType: lsp.RegistrationType<lsp.TextDocumentRegistrationOptions> =
    new lsp.RegistrationType('textDocument/didSave');

  register(
    data: lsp.RegistrationData<lsp.TextDocumentRegistrationOptions>,
  ): void {
    const provider = new TypeCoverageProvider(this._client);
    this.coverage = new TypeCoverage(
      data.registerOptions.documentSelector,
      provider,
      this._client.clientOptions.extensions.typeCoverage,
    );
  }

  unregister(): void {
    this.clear();
  }

  protected getDocumentSelectors(): IterableIterator<vscode.DocumentSelector> {
    const selector = this.coverage?._documentSelector;
    const selectors = selector == null ? [] : [selector];
    return selectors.values();
  }

  getState(): lsp.FeatureState {
    return {
      kind: 'document',
      id: 'completion-snippet',
      registrations: this.coverage != null,
      matches: true,
    };
  }

  clear(): void {
    this.coverage?.dispose();
    this.coverage = null;
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

    this.register({
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
