/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Extend VscodeLanguageClient to add support for
// 1) TypeCoverage
// 2) Status
// 3) ConfigureCompletionSnippetSupport

import { LanguageClient as VscodeLanguageClient } from 'vscode-languageclient';
import { type ServerOptions, type LanguageClientOptions } from './types';

import TypeCoverageFeature from './TypeCoverageFeature';
import StatusFeature from './StatusFeature';
import ConfigureCompletionSnippetSupport from './ConfigureCompletionSnippetSupport';

export default class LanguageClientEx extends VscodeLanguageClient {
  constructor(
    id: string,
    name: string,
    serverOptions: ServerOptions,
    clientOptions: LanguageClientOptions,
    forceDebug?: boolean,
  ) {
    super(id, name, serverOptions, clientOptions, forceDebug);
    // @ts-ignore: hack BaseLanguageClient removes extra properties from clientOptions so adding them back below
    this._clientOptions.extensions = clientOptions.extensions;
    this._registerExtraFeatures();
  }

  // @ts-ignore: bad getter
  get clientOptions(): LanguageClientOptions {
    // @ts-ignore: type clientOptions correctly
    return this._clientOptions;
  }

  _registerExtraFeatures(): void {
    this.registerFeature(new TypeCoverageFeature(this));
    this.registerFeature(new StatusFeature(this));
    this.registerFeature(new ConfigureCompletionSnippetSupport(this));
  }
}
