/*
 * @flow
 * @format
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
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
      addFile('error1.js.ignored', 'error1.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/error1.js',
        },
        range: {
          start: {
            line: 0,
            character: 1,
          },
          end: {
            line: 0,
            character: 2,
          },
        },
        context: {
          diagnostics: [],
        },
      }).verifyAllLSPMessagesInStep(
        [`textDocument/codeAction{[]}`],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/codeAction #1', [
      addFile('error1.js.ignored', 'error1.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/error1.js',
        },
        range: {
          start: {
            line: 1,
            character: 21,
          },
          end: {
            line: 1,
            character: 22,
          },
        },
        context: {
          diagnostics: [
            {
              range: {
                start: {
                  line: 1,
                  character: 21,
                },
                end: {
                  line: 1,
                  character: 22,
                },
              },
              severity: 1,
              code: 'InferError',
              source: 'Flow',
              message: 'Missing type annotation for `a`.',
            },
          ],
        },
      }).verifyAllLSPMessagesInStep(
        [
          `textDocument/codeAction{${JSON.stringify([
            {
              title: 'insert type annotation',
              kind: 'quickfix',
              diagnostics: [
                {
                  range: {
                    start: {line: 1, character: 21},
                    end: {line: 1, character: 22},
                  },
                  severity: 1,
                  code: 'InferError',
                  source: 'Flow',
                  message: 'Missing type annotation for `a`.',
                  relatedInformation: [],
                  relatedLocations: [],
                },
              ],
              edit: {
                changes: {
                  '<PLACEHOLDER_PROJECT_URL>/error1.js': [
                    {
                      range: {
                        start: {line: 1, character: 22},
                        end: {line: 1, character: 22},
                      },
                      newText: ': any',
                    },
                  ],
                },
              },
            },
          ])}}`,
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/codeAction #2', [
      addFile('error1.js.ignored', 'error1.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/error1.js',
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
          `textDocument/codeAction{${JSON.stringify([
            {
              title: 'insert type annotation',
              kind: 'quickfix',
              diagnostics: [],
              edit: {
                changes: {
                  '<PLACEHOLDER_PROJECT_URL>/error1.js': [
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
                        ': {a: number, b: (a: any, b: string) => number}',
                    },
                  ],
                },
              },
            },
          ])}}`,
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
