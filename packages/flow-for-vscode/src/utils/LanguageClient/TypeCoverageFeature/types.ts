/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as lsp from 'vscode-languageclient';
import * as vscode from 'vscode';

export type TypeCoverageParams = {
  textDocument: lsp.TextDocumentIdentifier;
};

export type UncoveredRange = {
  range: vscode.Range;
  message?: string;
};
export type TypeCoverageResult = {
  coveredPercent: number;
  uncoveredRanges: Array<UncoveredRange>;
  defaultMessage: string;
};

export type ConnectionStatusParams = { isConnected: boolean };

export type CoverageReport = {
  computing: boolean;
  coveredPercent: number | null;
  showingUncovered: boolean;
};

export type TypeCoverageOptions = {
  onChange: (coverage: CoverageReport | null) => void;
  command: string;
  diagnosticSeverity: vscode.DiagnosticSeverity;
  defaultShowUncovered: boolean;
};
