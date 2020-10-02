/*
 * @flow
 * @format
 */

import type Suite from 'flow-dev-tools/src/test/Suite.js';
import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default (suite(
  ({
    lspStartAndConnect,
    lspStart,
    lspRequest,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('textDocument/codeAction #0', [
      addFile('a.js.ignored', 'a.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/a.js',
        },
        range: {
          start: {
            line: 6,
            character: 11,
          },
          end: {
            line: 6,
            character: 17,
          },
        },
        context: {
          diagnostics: [],
        },
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/codeAction',
            `{${JSON.stringify([
              {
                title: 'insert type annotation',
                kind: 'quickfix',
                diagnostics: [],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/a.js': [
                      {
                        range: {
                          start: {
                            line: 6,
                            character: 17,
                          },
                          end: {
                            line: 6,
                            character: 17,
                          },
                        },
                        newText:
                          ': {a: number, b: (a: string, b: string) => number,...}',
                      },
                    ],
                  },
                },
              },
            ])}}`,
          ],
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/codeAction #1', [
      addFile('a.js.ignored', 'a.js'),
      addFile('b.js.ignored', 'b.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/b.js',
        },
        range: {
          start: {
            line: 6,
            character: 11,
          },
          end: {
            line: 6,
            character: 17,
          },
        },
        context: {
          diagnostics: [],
        },
      }).verifyAllLSPMessagesInStep(
        [['textDocument/codeAction', '{[]}']],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
): Suite);
