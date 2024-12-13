/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* eslint-disable no-empty-function */
import {
  type ClientCapabilities,
  type DynamicFeature,
  type RPCMessageType,
} from 'vscode-languageclient';
import { type ILanguageClient } from './types';

class ConfigureCompletionSnippetSupport implements DynamicFeature<void> {
  _enableSnippetSupport: boolean;

  messages: Array<RPCMessageType> = [];

  constructor(client: ILanguageClient) {
    const { initializationOptions } = client.clientOptions;
    this._enableSnippetSupport = initializationOptions
      ? Boolean(initializationOptions.useCodeSnippetOnFunctionSuggest)
      : false;
  }

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
  dispose(): void {}
}

export default ConfigureCompletionSnippetSupport;
