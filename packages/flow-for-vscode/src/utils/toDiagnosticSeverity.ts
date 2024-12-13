/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';

export default function toDiagnosticSeverity(
  val: string | null | undefined,
  defaultVal: vscode.DiagnosticSeverity,
): vscode.DiagnosticSeverity {
  switch (val) {
    case 'error':
      return vscode.DiagnosticSeverity.Error;
    case 'warn':
      return vscode.DiagnosticSeverity.Warning;
    case 'info':
      return vscode.DiagnosticSeverity.Information;
    default:
      return defaultVal;
  }
}
