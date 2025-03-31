/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

export * from 'vscode-languageclient/node';

import * as lsp from 'vscode-languageclient/node';
import * as vscode from 'vscode';

import {
  type TypeCoverageResult,
  type TypeCoverageOptions,
  type CoverageReport,
} from './TypeCoverageFeature/types';
import { type StatusReport, type StatusOptions } from './StatusFeature/types';

export type { StatusReport, CoverageReport };

export type ProvideTypeCoverageSignature = (
  document: vscode.TextDocument,
) => vscode.ProviderResult<TypeCoverageResult>;

export type TypeCoverageMiddleware = {
  readonly provideTypeCoverage?: (
    document: vscode.TextDocument,
    next: ProvideTypeCoverageSignature,
  ) => vscode.ProviderResult<TypeCoverageResult>;
};

export type Middleware = lsp.Middleware & TypeCoverageMiddleware;

export type LanguageClientOptions = lsp.LanguageClientOptions & {
  middleware: Middleware;
  extensions: {
    status: StatusOptions;
    typeCoverage: TypeCoverageOptions;
  };
};

export interface ILanguageClient extends lsp.LanguageClient {
  readonly clientOptions: LanguageClientOptions;
}
