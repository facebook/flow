/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* eslint-disable no-empty-function */
import {
  FeatureState,
  RegistrationType,
  type ClientCapabilities,
  type DynamicFeature,
} from 'vscode-languageclient/node';
import { type ILanguageClient } from './types';

class ConfigureCompletionSnippetSupport implements DynamicFeature<void> {
  _enableSnippetSupport: boolean;

  constructor(client: ILanguageClient) {
    const { initializationOptions } = client.clientOptions;
    this._enableSnippetSupport = initializationOptions
      ? Boolean(initializationOptions.useCodeSnippetOnFunctionSuggest)
      : false;
  }

  getState(): FeatureState {
    return {
      kind: 'document',
      id: 'completion-snippet',
      registrations: false,
      matches: false,
    };
  }

  registrationType: RegistrationType<void> = new RegistrationType(
    'textDocument/completion',
  );

  fillClientCapabilities(capabilities: ClientCapabilities): void {
    if (
      capabilities.textDocument &&
      capabilities.textDocument.completion &&
      capabilities.textDocument.completion.completionItem
    ) {
      capabilities.textDocument.completion.completionItem.snippetSupport =
        this._enableSnippetSupport;
    }
  }

  register(): void {}
  unregister(): void {}
  initialize(): void {}
  clear(): void {}
  dispose(): void {}
}

export default ConfigureCompletionSnippetSupport;
