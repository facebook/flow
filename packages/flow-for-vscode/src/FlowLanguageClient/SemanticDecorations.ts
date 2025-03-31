/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';
import * as lsp from '../utils/LanguageClient';
import type FlowClients from '../FlowClients';
import FlowconfigCache from '../utils/FlowconfigCache';

const REFINED_VALUE_DECORATION = vscode.window.createTextEditorDecorationType({
  backgroundColor: new vscode.ThemeColor('diffEditor.insertedLineBackground'),
});

function refinedValueRangesFromDiagostics(
  diagnosticList: readonly vscode.Diagnostic[],
): Array<vscode.Range> {
  const refinedValueRanges: Array<vscode.Range> = [];
  diagnosticList.forEach((diag) => {
    const semanticDecorationType = (
      diag as unknown as { data?: { semanticDecorationType: unknown } }
    ).data?.semanticDecorationType;
    if (semanticDecorationType === 'refined-value') {
      refinedValueRanges.push(diag.range);
    }
    return semanticDecorationType == null;
  });

  return refinedValueRanges;
}

async function refinedValueRanges(
  clients: FlowClients,
  flowconfigCache: FlowconfigCache,
  uri: vscode.Uri,
): Promise<Array<vscode.Range>> {
  const flowconfig = await flowconfigCache.getWithUri(uri);
  if (!flowconfig) {
    return [];
  }
  const diagnosticList =
    clients.get(flowconfig)?.getHiddenDiagnosticsForFile(uri) || [];
  return refinedValueRangesFromDiagostics(diagnosticList);
}

export class SemanticDecorationProvider {
  public constructor(
    private clients: FlowClients,
    private flowconfigCache: FlowconfigCache,
  ) {}

  async provideDecorations(editor: vscode.TextEditor): Promise<void> {
    editor.setDecorations(
      REFINED_VALUE_DECORATION,
      await refinedValueRanges(
        this.clients,
        this.flowconfigCache,
        editor.document.uri,
      ),
    );
  }
}

export function getFilterSemanticDecorationsMiddleWare(
  hiddenDiagnostics: Map<string, vscode.Diagnostic[]>,
): lsp.Middleware {
  return {
    handleDiagnostics(uri, diagnosticList, next) {
      const hidden: vscode.Diagnostic[] = [];
      const filtered = diagnosticList.filter((diag) => {
        if (
          (diag as unknown as { data?: { semanticDecorationType: unknown } })
            .data?.semanticDecorationType == null
        ) {
          return true;
        }
        hidden.push(diag);
        return false;
      });
      hiddenDiagnostics.set(uri.toString(), hidden);
      vscode.window.visibleTextEditors.forEach((editor) => {
        if (editor.document.uri.toString() === uri.toString()) {
          editor.setDecorations(
            REFINED_VALUE_DECORATION,
            refinedValueRangesFromDiagostics(hidden),
          );
        }
      });
      return next(uri, filtered);
    },
  };
}
