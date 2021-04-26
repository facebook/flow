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
        [{method: 'textDocument/codeAction', result: []}],
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
              message: 'Cannot build a typed interface for this module.',
            },
          ],
        },
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
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
                    message: 'Cannot build a typed interface for this module.',
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
                        newText: ': empty',
                      },
                    ],
                  },
                },
              },
            ],
          },
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
          {
            method: 'textDocument/codeAction',
            result: [
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
                          ': { a: number, b: (a: empty, b: string) => number, ... }',
                      },
                    ],
                  },
                },
              },
            ],
          },
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/codeAction #3', [
      addFile('exports-func.js.ignored', 'exports-func.js'),
      addFile('needs-import.js.ignored', 'needs-import.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/needs-import.js',
        },
        range: {
          start: {
            line: 4,
            character: 13,
          },
          end: {
            line: 4,
            character: 22,
          },
        },
        context: {
          diagnostics: [],
        },
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                title: 'insert type annotation',
                kind: 'quickfix',
                diagnostics: [],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/needs-import.js': [
                      {
                        range: {
                          start: {
                            line: 2,
                            character: 0,
                          },
                          end: {
                            line: 2,
                            character: 0,
                          },
                        },
                        newText: 'import type { Node } from "./exports-func";',
                      },
                      {
                        range: {
                          start: {
                            line: 4,
                            character: 10,
                          },
                          end: {
                            line: 4,
                            character: 10,
                          },
                        },
                        newText: ': Node',
                      },
                    ],
                  },
                },
              },
            ],
          },
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
): Suite);
