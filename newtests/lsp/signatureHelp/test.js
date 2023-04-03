/*
 * @flow
 * @format
 */

import type {Suite} from '../../../packages/flow-dev-tools/src/test/Suite';
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(
  ({
    addFile,
    addFiles,
    addCode,
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    lspIgnoreStatusAndCancellation,
  }) => {
    return [
      test('textDocument/signatureHelp', [
        addFiles(
          'nestedClasses.js',
          'nestedFunctions.js',
          'paramDocumentation.js',
          'tupleRestParam.js',
          'member_callee.js',
        ),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/nestedClasses.js'},
          position: {line: 4, character: 2}, // `f(|class {})`
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(x: mixed): void',
                    parameters: [
                      {
                        label: 'x: mixed',
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/nestedClasses.js'},
          position: {line: 4, character: 7}, // `f(class| {})`
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(x: mixed): void',
                    parameters: [
                      {
                        label: 'x: mixed',
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/nestedClasses.js'},
          position: {line: 4, character: 9}, // `f(class {|})`
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: null,
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/nestedFunctions.js'},
          position: {line: 6, character: 2},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(a: string, f: F, b: number): void',
                    parameters: [
                      {
                        label: 'a: string',
                      },
                      {
                        label: 'f: F',
                      },
                      {
                        label: 'b: number',
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/nestedFunctions.js'},
          position: {line: 8, character: 2}, // inside nested function body
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: null,
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js'},
          position: {line: 6, character: 1},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(arg[0]: string, arg[1]: number): void',
                    parameters: [
                      {
                        label: 'arg[0]: string',
                      },
                      {
                        label: 'arg[1]: number',
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js'},
          position: {line: 12, character: 1},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(params[0]: string, params[1]: number): void',
                    parameters: [
                      {
                        label: 'params[0]: string',
                      },
                      {
                        label: 'params[1]: number',
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js'},
          position: {line: 18, character: 1},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(foo: string, bar: number): void',
                    parameters: [
                      {
                        label: 'foo: string',
                      },
                      {
                        label: 'bar: number',
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js'},
          position: {line: 24, character: 1},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(foo: string, bar: number): void',
                    parameters: [
                      {
                        label: 'foo: string',
                      },
                      {
                        label: 'bar: number',
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js'},
          position: {line: 30, character: 1},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(foo: string, arg[1]: number): void',
                    parameters: [
                      {
                        label: 'foo: string',
                      },
                      {
                        label: 'arg[1]: number',
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/tupleRestParam.js'},
          position: {line: 36, character: 1},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(foo: string, params[1]: number): void',
                    parameters: [
                      {
                        label: 'foo: string',
                      },
                      {
                        label: 'params[1]: number',
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/paramDocumentation.js',
          },
          position: {line: 12, character: 5},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(bar: string, baz: number): void',
                    documentation: {
                      kind: 'markdown',
                      value:
                        'foo\n\n**@unrecognized** this tag is unrecognized',
                    },
                    parameters: [
                      {
                        label: 'bar: string',
                        documentation: {
                          kind: 'markdown',
                          value: 'bar - the first summand',
                        },
                      },
                      {
                        label: 'baz: number',
                        documentation: {
                          kind: 'markdown',
                          value:
                            'baz - the second and third summands\nbaz.x (optional)  - the second summand\nbaz.y (optional, defaults to 0)  - the third summand',
                        },
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/member_callee.js',
          },
          position: {line: 17, character: 3},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(x: string): void',
                    documentation: {
                      kind: 'markdown',
                      value: 'foo',
                    },
                    parameters: [
                      {
                        label: 'x: string',
                        documentation: {
                          kind: 'markdown',
                          value: 'x - a string',
                        },
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/member_callee.js',
          },
          position: {line: 21, character: 3},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(y: number): void',
                    documentation: {
                      kind: 'markdown',
                      value: 'bar',
                    },
                    parameters: [
                      {
                        label: 'y: number',
                        documentation: {
                          kind: 'markdown',
                          value: 'y - a number',
                        },
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/member_callee.js',
          },
          position: {line: 33, character: 3},
        }).verifyAllLSPMessagesInStep(
          [
            {
              method: 'textDocument/signatureHelp',
              result: {
                signatures: [
                  {
                    label: '(z: bigint): void',
                    documentation: {
                      kind: 'markdown',
                      value: 'baz',
                    },
                    parameters: [
                      {
                        label: 'z: bigint',
                        documentation: {
                          kind: 'markdown',
                          value: 'z - a bigint',
                        },
                      },
                    ],
                  },
                ],
                activeSignature: 0,
                activeParameter: 0,
              },
            },
          ],
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ]),
    ];
  },
): Suite);
