/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// add support using "window/showStatus" lsp extension
import * as lsp from 'vscode-languageclient';
import StatusProvider from './StatusProvider';
import Status from './Status';
import { type ILanguageClient } from '../types';

type StaticFeature = lsp.StaticFeature;

export default class StatusFeature implements StaticFeature {
  _client: ILanguageClient;

  constructor(client: ILanguageClient) {
    this._client = client;
  }

  fillClientCapabilities(capabilities: lsp.ClientCapabilities): void {
    // @ts-ignore
    capabilities.window = capabilities.window || {};
    // @ts-ignore
    capabilities.window.status = { dynamicRegistration: false };
  }

  initialize(): lsp.Disposable {
    const statusProvider = new StatusProvider(this._client);
    return new Status(
      statusProvider,
      this._client.clientOptions.extensions.status,
    );
  }
}
