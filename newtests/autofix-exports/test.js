/*
 * @flow
 * @format
 */

import type {SuiteType} from '../Tester';
const {suite, test} = require('../Tester');

module.exports = (suite(
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
          only: ['quickfix'],
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
          only: ['quickfix'],
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
                command: {
                  arguments: [
                    'textDocument/codeAction',
                    'insert_type_for_sig_verification_failure',
                    'Insert type annotation to fix signature-verification-failure error',
                  ],
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  title: '',
                },
                diagnostics: [
                  {
                    code: 'InferError',
                    message: 'Cannot build a typed interface for this module.',
                    range: {
                      end: {
                        character: 22,
                        line: 1,
                      },
                      start: {
                        character: 21,
                        line: 1,
                      },
                    },
                    relatedInformation: [],
                    severity: 1,
                    source: 'Flow',
                  },
                ],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/error1.js': [
                      {
                        newText: ': any',
                        range: {
                          end: {
                            character: 22,
                            line: 1,
                          },
                          start: {
                            character: 22,
                            line: 1,
                          },
                        },
                      },
                    ],
                  },
                },
                kind: 'quickfix',
                title:
                  'Insert type annotation to fix signature-verification-failure error',
              },
            ],
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
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
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                command: {
                  arguments: [
                    'textDocument/codeAction',
                    'insert_type_for_sig_verification_failure',
                    'Insert type annotation to fix signature-verification-failure error',
                  ],
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  title: '',
                },
                diagnostics: [],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/error1.js': [
                      {
                        newText:
                          ': {| a: number, b: (a: any, b: string) => number |}',
                        range: {
                          end: {
                            character: 17,
                            line: 6,
                          },
                          start: {
                            character: 17,
                            line: 6,
                          },
                        },
                      },
                    ],
                  },
                },
                kind: 'quickfix',
                title:
                  'Insert type annotation to fix signature-verification-failure error',
              },
            ],
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
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
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                command: {
                  arguments: [
                    'textDocument/codeAction',
                    'insert_type_for_sig_verification_failure',
                    'Insert type annotation to fix signature-verification-failure error',
                  ],
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  title: '',
                },
                diagnostics: [],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/needs-import.js': [
                      {
                        newText: 'import type { Node } from "./exports-func";',
                        range: {
                          end: {
                            character: 0,
                            line: 2,
                          },
                          start: {
                            character: 0,
                            line: 2,
                          },
                        },
                      },
                      {
                        newText: ': Node',
                        range: {
                          end: {
                            character: 10,
                            line: 4,
                          },
                          start: {
                            character: 10,
                            line: 4,
                          },
                        },
                      },
                    ],
                  },
                },
                kind: 'quickfix',
                title:
                  'Insert type annotation to fix signature-verification-failure error',
              },
            ],
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
  ],
): SuiteType);
