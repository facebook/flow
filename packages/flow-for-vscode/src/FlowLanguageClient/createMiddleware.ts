/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Middleware:
// 1) Prevent duplicate results in nested .flowconfig directory structure
// - root
//   -.flowconfig [lsp running for all files under root (including project-a files) [client-root]]
//   - fileA.js
//   - project-a
//     -.flowconfig [lsp running for all files under project-a [client-A]]
//     - fileB.js  [client-root and client-A both serve lsp request for this file]
// Below middleware will noop fileB.js request in client-root

import * as vscode from 'vscode';
import * as lsp from '../utils/LanguageClient';
import FlowconfigCache from '../utils/FlowconfigCache';
import {
  InterceptDiagnosticsMiddleware,
  OpenDetailedDiagnosticsCodeActionMiddleware,
} from './DetailedDiagnostics';
import { getFilterSemanticDecorationsMiddleWare } from './SemanticDecorations';

function chainHandleDiagnosticsMiddleware(
  hiddenDiagnostics: Map<string, vscode.Diagnostic[]>,
): lsp.Middleware {
  const { handleDiagnostics: handle1 } = InterceptDiagnosticsMiddleware;
  const { handleDiagnostics: handle2 } =
    getFilterSemanticDecorationsMiddleWare(hiddenDiagnostics);

  if (handle1 == null || handle2 == null) {
    throw 'Invalid middlewares';
  }

  return {
    handleDiagnostics(uri, diagnostics, next) {
      handle1(uri, diagnostics, (uri, diagnostics) =>
        handle2(uri, diagnostics, next),
      );
    },
  };
}

export default function createMiddleware(
  clientFlowconfig: string,
  hiddenDiagnostics: Map<string, vscode.Diagnostic[]>,
): lsp.Middleware {
  const flowconfigCache = new FlowconfigCache('.flowconfig');

  return {
    ...chainHandleDiagnosticsMiddleware(hiddenDiagnostics),
    ...OpenDetailedDiagnosticsCodeActionMiddleware,
    provideTypeCoverage(document, next) {
      return flowconfigCache.get(document).then((docFlowconfig) => {
        if (docFlowconfig === clientFlowconfig) {
          return next(document);
        }
        return null;
      });
    },
  };
}
