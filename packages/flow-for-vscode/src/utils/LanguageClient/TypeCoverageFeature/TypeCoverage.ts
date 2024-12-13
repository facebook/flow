/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import * as vscode from 'vscode';

import {
  type TypeCoverageResult,
  type TypeCoverageOptions,
  type UncoveredRange,
} from './types';
import TypeCoverageProvider from './TypeCoverageProvider';

type Request = {
  cancel: () => void;
};

type State = {
  showUncovered: boolean;
  activeDocument: null | vscode.TextDocument;
  coverage: null | TypeCoverageResult;
  pendingRequest: null | Request;
  isConnected: boolean;
};

export default class TypeCoverage {
  _subscriptions: Array<vscode.Disposable> = [];
  _diagnostics: vscode.DiagnosticCollection;
  _documentSelector: vscode.DocumentSelector | null;
  _provider: TypeCoverageProvider;
  _options: TypeCoverageOptions;
  _state: State;

  constructor(
    documentSelector: vscode.DocumentSelector | null,
    provider: TypeCoverageProvider,
    options: TypeCoverageOptions,
  ) {
    this._provider = provider;
    this._options = options;
    this._documentSelector = documentSelector;
    this._state = {
      showUncovered: options.defaultShowUncovered,
      activeDocument: null,
      coverage: null,
      pendingRequest: null,
      isConnected: false,
    };

    this._diagnostics =
      vscode.languages.createDiagnosticCollection('flow_coverage');

    this._subscriptions.push(
      this._provider.onConnectionStatus(this._handleConnectionStatus),
      this._diagnostics,
      vscode.commands.registerCommand(options.command, () => {
        this._setState({ showUncovered: !this._state.showUncovered });
      }),
      vscode.workspace.onDidSaveTextDocument((document) =>
        this._computeCoverage(document),
      ),
      vscode.window.onDidChangeActiveTextEditor((editor) => {
        this._computeCoverage(editor ? editor.document : null);
      }),
    );
    if (vscode.window.activeTextEditor) {
      this._computeCoverage(vscode.window.activeTextEditor.document);
    }
  }

  dispose(): void {
    this._subscriptions.forEach((item) => item.dispose());
    if (this._state.pendingRequest) {
      this._state.pendingRequest.cancel();
    }
  }

  render(): void {
    this._renderCoverage();
    this._renderDiagnostics();
  }

  _renderCoverage(): void {
    const { _state: state } = this;
    const { coverage } = state;

    if (state.activeDocument && state.isConnected) {
      // computing coverage for first time
      if (!coverage && state.pendingRequest) {
        this._options.onChange({
          computing: true,
          coveredPercent: null,
          showingUncovered: state.showUncovered,
        });
        return;
      }

      // update covearge
      if (coverage) {
        const computing = Boolean(state.pendingRequest);
        this._options.onChange({
          computing,
          coveredPercent: coverage.coveredPercent,
          showingUncovered: state.showUncovered,
        });

        return;
      }
    }

    this._options.onChange(null);
  }

  _renderDiagnostics(): void {
    const { coverage, showUncovered, activeDocument, pendingRequest } =
      this._state;

    this._diagnostics.clear();
    if (!showUncovered || !activeDocument || pendingRequest) {
      return;
    }

    if (coverage && coverage.uncoveredRanges.length > 0) {
      const diagnostics = coverage.uncoveredRanges.map((uncoveredRange) =>
        uncoveredRangeToDiagnostic(
          uncoveredRange,
          this._options.diagnosticSeverity,
        ),
      );
      this._diagnostics.set(activeDocument.uri, diagnostics);
    }
  }

  _setState(partialState: Partial<State>): void {
    this._state = {
      ...this._state,
      ...partialState,
    };
    this.render();
  }

  _handleConnectionStatus = (params: { isConnected: boolean }): void => {
    this._setState({ isConnected: params.isConnected });
    if (params.isConnected && vscode.window.activeTextEditor) {
      this._computeCoverage(vscode.window.activeTextEditor.document);
    }
  };

  _computeCoverage(document: null | vscode.TextDocument): void {
    if (
      !this._state.isConnected ||
      !document ||
      !this._documentSelector ||
      !vscode.languages.match(this._documentSelector, document)
    ) {
      this._setState({
        activeDocument: null,
        pendingRequest: null,
        coverage: null,
      });
      return;
    }

    if (this._state.pendingRequest) {
      this._state.pendingRequest.cancel();
    }

    const pendingRequest = requestTypeCoverage(
      this._provider,
      document,
      (coverage) => {
        this._setState({ pendingRequest: null, coverage });
      },
    );

    this._setState({
      activeDocument: document,
      pendingRequest,
      // reset coverage when document changed
      coverage:
        this._state.activeDocument !== document ? null : this._state.coverage,
    });
  }
}

function requestTypeCoverage(
  provider: TypeCoverageProvider,
  document: vscode.TextDocument,
  callback: (coverage: null | TypeCoverageResult) => void,
): Request {
  let isCancelled = false;

  Promise.resolve(provider.provideTypeCoverage(document)).then((coverage) => {
    if (!isCancelled) {
      return callback(coverage || null);
    }
    return null;
  });

  return {
    cancel: () => {
      isCancelled = true;
    },
  };
}

function uncoveredRangeToDiagnostic(
  uncoveredRange: UncoveredRange,
  severity: vscode.DiagnosticSeverity,
): vscode.Diagnostic {
  const diagnostic = new vscode.Diagnostic(
    uncoveredRange.range,
    uncoveredRange.message || 'Not covered by flow',
    severity,
  );
  diagnostic.source = 'Type Coverage';
  return diagnostic;
}
