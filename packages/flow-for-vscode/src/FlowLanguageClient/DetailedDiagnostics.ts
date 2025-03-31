/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Inspired by https://github.com/rust-lang/rust-analyzer/blob/1a5bb27c018c947dab01ab70ffe1d267b0481a17/editors/code/src/diagnostics.ts

import * as path from 'path';
import * as vscode from 'vscode';
import * as lsp from '../utils/LanguageClient';
import type FlowClients from '../FlowClients';
import FlowconfigCache from '../utils/FlowconfigCache';

export const DIAGNOSTICS_URI_SCHEME = 'vscode-flow-diagnostics-view';
export const OPEN_DOCUMENT_CLIENT_COMMAND = 'vscode-flow-open-document-command';

function encodeDiagnosticUri(documentUri: vscode.Uri, idx: number): vscode.Uri {
  return vscode.Uri.from({
    scheme: DIAGNOSTICS_URI_SCHEME,
    path: `/diagnostic message [${idx.toString()}]`,
    fragment: documentUri.toString(),
    query: idx.toString(),
  });
}

export const InterceptDiagnosticsMiddleware: lsp.Middleware = {
  handleDiagnostics(uri, diagnosticList, next) {
    diagnosticList.forEach((diag, idx) => {
      const rendered = (diag as unknown as { data?: { rendered?: object } })
        .data?.rendered;
      if (rendered) {
        diag.message = `${diag.message} [${diag.code as string}] `;
        diag.code = {
          target: encodeDiagnosticUri(uri, idx),
          value: `Click for full error`,
        };
      }
    });
    return next(uri, diagnosticList);
  },
};

type AnsiStyleType =
  | 'normal'
  | 'bold'
  | 'dim'
  | 'underline'
  | 'bold-underline'
  | 'dim-underline';

type AnsiColor =
  | 'default'
  | 'black'
  | 'red'
  | 'green'
  | 'yellow'
  | 'blue'
  | 'magenta'
  | 'cyan'
  | 'white';

type AnsiStyle = { readonly type: AnsiStyleType; readonly color: AnsiColor };

type Rendered = readonly { readonly style: AnsiStyle; readonly text: string }[];

function validateRendered(unvalidated: unknown): Rendered | null {
  if (!Array.isArray(unvalidated)) {
    return null;
  }
  const validated: Array<Rendered[number]> = [];
  for (const unvalidatedItem of unvalidated) {
    if (typeof unvalidatedItem !== 'object' || unvalidatedItem == null) {
      return null;
    }
    const partiallyValidatedItem = unvalidatedItem as unknown as {
      style?: unknown;
      text?: unknown;
    };
    const text = partiallyValidatedItem.text;
    if (typeof text !== 'string') {
      return null;
    }
    if (
      typeof partiallyValidatedItem.style !== 'object' ||
      partiallyValidatedItem.style == null
    ) {
      return null;
    }
    const partiallyValidatedStyle = partiallyValidatedItem.style as {
      type?: unknown;
      color?: unknown;
    };
    switch (partiallyValidatedStyle.type) {
      case 'normal':
      case 'bold':
      case 'dim':
      case 'underline':
      case 'bold-underline':
      case 'dim-underline':
        break;
      default:
        return null;
    }
    const styleType = partiallyValidatedStyle.type;
    switch (partiallyValidatedStyle.color) {
      case 'default':
      case 'black':
      case 'red':
      case 'green':
      case 'yellow':
      case 'blue':
      case 'magenta':
      case 'cyan':
      case 'white':
        break;
      default:
        return null;
    }
    const color = partiallyValidatedStyle.color;
    validated.push({ style: { type: styleType, color }, text });
  }
  return validated;
}

function validatedRenderedFromDiagnostic(
  diag: vscode.Diagnostic,
): Rendered | null {
  // We do the same trick from rust-analyzer.
  // See https://github.com/rust-lang/rust-analyzer/blob/21ec8f523812b88418b2bfc64240c62b3dd967bd/editors/code/src/client.ts#L195-L204
  // This structure is not in the public API of vscode.Diagnostic, but it's critical for the detailed
  // error view to work. The data comes from the additional data from Flow LSP.
  return validateRendered(
    (diag as unknown as { data?: { rendered?: unknown } }).data?.rendered,
  );
}

type ErrorRenderingResult =
  | { readonly type: 'rendered'; readonly rendered: Rendered }
  | {
      readonly type:
        | 'missing-client'
        | 'missing-diagnostics-in-diagnostic-collection'
        | 'missing-diagnostics-in-file'
        | 'failed-validation';
    };

async function provideRenderedContent(
  clients: FlowClients,
  flowconfigCache: FlowconfigCache,
  uri: vscode.Uri,
): Promise<ErrorRenderingResult> {
  const docWithErrorUri = vscode.Uri.parse(uri.fragment, true);

  const flowconfig = await flowconfigCache.getWithUri(docWithErrorUri);
  if (!flowconfig) {
    return { type: 'missing-client' };
  }
  const client = clients.get(flowconfig)?._client;
  if (client == null) {
    return { type: 'missing-client' };
  }

  const diags = client.diagnostics?.get(docWithErrorUri);
  if (!diags) {
    return { type: 'missing-diagnostics-in-diagnostic-collection' };
  }

  const diag = diags[parseInt(uri.query, 10)];
  if (!diag) {
    return { type: 'missing-diagnostics-in-file' };
  }
  // We do the same trick from rust-analyzer.
  // See https://github.com/rust-lang/rust-analyzer/blob/21ec8f523812b88418b2bfc64240c62b3dd967bd/editors/code/src/client.ts#L195-L204
  // This structure is not in the public API of vscode.Diagnostic, but it's critical for the detailed
  // error view to work. The data comes from the additional data from Flow LSP.
  const rendered = validatedRenderedFromDiagnostic(diag);

  if (!rendered) {
    return { type: 'failed-validation' };
  }

  return { type: 'rendered', rendered };
}

export const OpenDetailedDiagnosticsCodeActionMiddleware: lsp.Middleware = {
  async provideCodeActions(document, range, context, token, next) {
    const lspCodeActions = await next(document, range, context, token);
    if (
      !vscode.workspace
        .getConfiguration('flow')
        .get<boolean>('detailedErrorRenderingCodeAction')
    ) {
      return lspCodeActions;
    }
    const openDetailedDiagnosticsCodeActions: vscode.CodeAction[] = [];
    for (let i = 0; i < context.diagnostics.length; i++) {
      const diagnostic = context.diagnostics[i];
      if (
        diagnostic.range.contains(range) &&
        diagnostic != null &&
        validatedRenderedFromDiagnostic(diagnostic) != null
      ) {
        const uri = encodeDiagnosticUri(document.uri, i);
        openDetailedDiagnosticsCodeActions.push({
          title: 'See detailed Flow error',
          command: {
            arguments: [{ uri }],
            command: OPEN_DOCUMENT_CLIENT_COMMAND,
            title: 'See detailed Flow error',
          },
          diagnostics: [diagnostic],
        });
      }
    }
    if (openDetailedDiagnosticsCodeActions.length === 0) {
      return lspCodeActions;
    }
    if (Array.isArray(lspCodeActions)) {
      lspCodeActions.push(...openDetailedDiagnosticsCodeActions);
      return lspCodeActions;
    } else {
      return openDetailedDiagnosticsCodeActions;
    }
  },
};

export class TextDocumentProvider
  implements vscode.TextDocumentContentProvider
{
  private _onDidChange = new vscode.EventEmitter<vscode.Uri>();

  public constructor(
    private clients: FlowClients,
    private flowconfigCache: FlowconfigCache,
  ) {}

  get onDidChange(): vscode.Event<vscode.Uri> {
    return this._onDidChange.event;
  }

  triggerUpdate(uri: vscode.Uri): void {
    if (uri.scheme === DIAGNOSTICS_URI_SCHEME) {
      this._onDidChange.fire(uri);
    }
  }

  dispose(): void {
    this._onDidChange.dispose();
  }

  async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
    const result = await provideRenderedContent(
      this.clients,
      this.flowconfigCache,
      uri,
    );
    switch (result.type) {
      case 'rendered':
        return result.rendered.map(({ text }) => text).join('');
      case 'missing-client': {
        const errorMessage =
          'Unable to find original Flow diagnostic due to missing client';
        return errorMessage;
      }
      case 'missing-diagnostics-in-diagnostic-collection': {
        const errorMessage =
          'Unable to find original Flow diagnostic due to missing diagnostic in DiagnosticCollection';
        return errorMessage;
      }
      case 'missing-diagnostics-in-file': {
        const errorMessage =
          'Unable to find the Flow diagnostic. Maybe the error is already fixed?';
        return errorMessage;
      }
      case 'failed-validation': {
        const errorMessage =
          'Unable to find original Flow diagnostic due to invalid rendered error';
        return errorMessage;
      }
    }
  }
}

export class ErrorLinkProvider implements vscode.DocumentLinkProvider {
  public constructor(
    private clients: FlowClients,
    private flowconfigCache: FlowconfigCache,
  ) {}

  async provideDocumentLinks(
    document: vscode.TextDocument,
    _token: vscode.CancellationToken,
  ): Promise<vscode.DocumentLink[] | null> {
    if (document.uri.scheme !== DIAGNOSTICS_URI_SCHEME) {
      return null;
    }

    const flowconfig = await this.flowconfigCache.getWithUri(
      vscode.Uri.parse(document.uri.fragment, true),
    );
    if (!flowconfig) {
      return null;
    }
    const rootUri = vscode.Uri.file(path.dirname(flowconfig));
    const renderedResult = await provideRenderedContent(
      this.clients,
      this.flowconfigCache,
      document.uri,
    );
    if (renderedResult.type !== 'rendered') {
      return null;
    }
    const stringContents = renderedResult.rendered
      .map(({ text }) => text)
      .join('');
    const lines = stringContents.split('\n');

    const result: vscode.DocumentLink[] = [];

    for (const [lineNumber, line] of lines.entries()) {
      for (const pathLineMatched of line.matchAll(
        /(^|(?<= ))\S+\.(jsx?|flow)(:[0-9]+:[0-9]+)?/g,
      )) {
        const [filename, firstLine, firstCol] = pathLineMatched[0].split(':');
        const offset = pathLineMatched.index;
        if (filename == null || offset == null) {
          continue;
        }
        const range = new vscode.Range(
          lineNumber,
          offset,
          lineNumber,
          offset + pathLineMatched[0].length,
        );

        const lineColumn =
          firstLine != null && firstCol != null
            ? { line: firstLine, column: firstCol }
            : undefined;

        result.push(
          new vscode.DocumentLink(
            range,
            this._uriOfFilepathWithOptionalLineColumn(
              rootUri,
              filename,
              lineColumn,
            ),
          ),
        );
      }
    }

    return result;
  }

  private _uriOfFilepathWithOptionalLineColumn(
    root: vscode.Uri,
    filename: string,
    lineColumn?: { line: number | string; column: number | string },
  ) {
    return vscode.Uri.from({
      scheme: 'file',
      path: path.isAbsolute(filename)
        ? filename
        : path.resolve(path.join(root.path, filename)),
      fragment: lineColumn
        ? `L${lineColumn.line},${lineColumn.column}`
        : undefined,
    });
  }
}

export class AnsiDecorationProvider implements vscode.Disposable {
  private _decorationTypeCache = new Map<
    string,
    vscode.TextEditorDecorationType
  >();

  public constructor(
    private clients: FlowClients,
    private flowconfigCache: FlowconfigCache,
  ) {}

  dispose(): void {
    for (const decorationType of this._decorationTypeCache.values()) {
      decorationType.dispose();
    }

    this._decorationTypeCache.clear();
  }

  async provideDecorations(editor: vscode.TextEditor): Promise<void> {
    if (editor.document.uri.scheme !== DIAGNOSTICS_URI_SCHEME) {
      return;
    }

    const decorations = await this._getDecorations(editor.document.uri);
    for (const [decorationType, ranges] of decorations) {
      editor.setDecorations(decorationType, ranges);
    }
  }

  private async _getDecorations(
    uri: vscode.Uri,
  ): Promise<[vscode.TextEditorDecorationType, vscode.Range[]][]> {
    const renderedResult = await provideRenderedContent(
      this.clients,
      this.flowconfigCache,
      uri,
    );
    if (renderedResult.type !== 'rendered') {
      return [];
    }

    const result = new Map<vscode.TextEditorDecorationType, vscode.Range[]>();
    // Populate all known decoration types in the result. This forces any
    // lingering decorations to be cleared if the text content changes to
    // something without ANSI codes for a given decoration type.
    for (const decorationType of this._decorationTypeCache.values()) {
      result.set(decorationType, []);
    }

    let lineNumber = 0;
    let columnNumber = 0;
    for (const { style, text } of renderedResult.rendered) {
      const lines = text.split('\n');
      if (lines.length === 0) {
        continue;
      }
      const endLineNumber = lineNumber + lines.length - 1;
      const endColumnNumber =
        lines.length > 1
          ? lines[lines.length - 1].length
          : columnNumber + lines[lines.length - 1].length;
      const range = new vscode.Range(
        lineNumber,
        columnNumber,
        endLineNumber,
        endColumnNumber,
      );
      lineNumber = endLineNumber;
      columnNumber = endColumnNumber;

      const decorationType = this._getDecorationType(style);

      const existingRanges = result.get(decorationType);
      if (existingRanges == null) {
        result.set(decorationType, [range]);
      } else {
        existingRanges.push(range);
      }
    }

    return [...result];
  }

  private _getDecorationType({
    type,
    color,
  }: AnsiStyle): vscode.TextEditorDecorationType {
    let decorationType = this._decorationTypeCache.get(`${type}-${color}`);

    if (decorationType) {
      return decorationType;
    }

    let themeColor: vscode.ThemeColor;
    switch (color) {
      case 'black':
        themeColor = new vscode.ThemeColor('terminal.ansiBlack');
        break;
      case 'red':
        themeColor = new vscode.ThemeColor('terminal.ansiRed');
        break;
      case 'green':
        themeColor = new vscode.ThemeColor('terminal.ansiGreen');
        break;
      case 'yellow':
        themeColor = new vscode.ThemeColor('terminal.ansiYellow');
        break;
      case 'blue':
        themeColor = new vscode.ThemeColor('terminal.ansiBlue');
        break;
      case 'magenta':
        themeColor = new vscode.ThemeColor('terminal.ansiMagenta');
        break;
      case 'cyan':
        themeColor = new vscode.ThemeColor('terminal.ansiCyan');
        break;
      case 'white':
        themeColor = new vscode.ThemeColor('terminal.ansiWhite');
        break;
      case 'default':
      default:
        themeColor = new vscode.ThemeColor('terminal.foreground');
    }

    decorationType = vscode.window.createTextEditorDecorationType({
      color: themeColor,
      fontWeight:
        type === 'bold' || type === 'bold-underline' ? 'bold' : undefined,
      opacity: type === 'dim' || type === 'dim-underline' ? '0.7' : undefined,
      textDecoration:
        type === 'underline' ||
        type === 'bold-underline' ||
        type === 'dim-underline'
          ? 'underline'
          : undefined,
    });

    this._decorationTypeCache.set(`${type}-${color}`, decorationType);
    return decorationType;
  }
}
