/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

module.exports = (suite(
  ({
    addFile,
    addFiles,
    addCode,
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('textDocument/signatureHelp', [
      addFiles(
        'nestedClasses.js',
        'nestedFunctions.js',
        'paramDocumentation.js',
        'tupleRestParam.js',
        'member_callee.js',
        'constructor.js',
        'typeAliasCall.js',
        'calls_monomorphic.js',
        'calls_overloaded.js',
        'calls_generic.js',
        'jsx_attr.js',
        'jsx_attr_docs.js',
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
                    value: 'foo\n\n**@unrecognized** this tag is unrecognized',
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
      lspRequestAndWaitUntilResponse('textDocument/signatureHelp', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/constructor.js',
        },
        position: {line: 12, character: 3},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: string, y: number): void',
                  documentation: {
                    kind: 'markdown',
                    value: 'constructor docs',
                  },
                  parameters: [
                    {
                      label: 'x: string',
                      documentation: {
                        kind: 'markdown',
                        value: 'x - is an x',
                      },
                    },
                    {
                      label: 'y: number',
                      documentation: {
                        kind: 'markdown',
                        value: 'y - does a y',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/typeAliasCall.js',
        },
        position: {line: 14, character: 3},
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
                    value: 'variable declaration',
                  },
                  parameters: [
                    {
                      label: 'x: string',
                      documentation: {
                        kind: 'markdown',
                        value: 'x - is a string',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_monomorphic.js',
        },
        position: {line: 5, character: 4},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_monomorphic.js',
        },
        position: {line: 9, character: 6},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_monomorphic.js',
        },
        position: {line: 17, character: 13},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_monomorphic.js',
        },
        position: {line: 21, character: 9},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_monomorphic.js',
        },
        position: {line: 25, character: 8},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_monomorphic.js',
        },
        position: {line: 30, character: 8},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 8, character: 4},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 12, character: 6},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 16, character: 7},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 20, character: 13},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 24, character: 9},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 28, character: 8},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 33, character: 8},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 48, character: 4},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 52, character: 6},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 56, character: 7},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 60, character: 13},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 64, character: 9},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 68, character: 8},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 73, character: 8},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(): void',
                  parameters: [],
                },
                {
                  label: '(x: string): void',
                  parameters: [
                    {
                      label: 'x: string',
                    },
                  ],
                },
                {
                  label: '(x: A, y: A): void',
                  parameters: [
                    {
                      label: 'x: A',
                    },
                    {
                      label: 'y: A',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 80, character: 4},
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
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_overloaded.js',
        },
        position: {line: 81, character: 4},
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
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 5, character: 4},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: void, y: void, z: Array<void>): void',
                  parameters: [
                    {
                      label: 'x: void',
                    },
                    {
                      label: 'y: void',
                    },
                    {
                      label: 'z: Array<void>',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 6, character: 7},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | void, y: number | void, z: Array<number | void>): void',
                  parameters: [
                    {
                      label: 'x: number | void',
                    },
                    {
                      label: 'y: number | void',
                    },
                    {
                      label: 'z: Array<number | void>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 1,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 7, character: 11},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | string, y: number | string, z: Array<number | string>): void',
                  parameters: [
                    {
                      label: 'x: number | string',
                    },
                    {
                      label: 'y: number | string',
                    },
                    {
                      label: 'z: Array<number | string>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 2,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 11, character: 6},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: void, y: void, z: Array<void>): void',
                  parameters: [
                    {
                      label: 'x: void',
                    },
                    {
                      label: 'y: void',
                    },
                    {
                      label: 'z: Array<void>',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 12, character: 9},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | void, y: number | void, z: Array<number | void>): void',
                  parameters: [
                    {
                      label: 'x: number | void',
                    },
                    {
                      label: 'y: number | void',
                    },
                    {
                      label: 'z: Array<number | void>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 1,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 13, character: 13},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | string, y: number | string, z: Array<number | string>): void',
                  parameters: [
                    {
                      label: 'x: number | string',
                    },
                    {
                      label: 'y: number | string',
                    },
                    {
                      label: 'z: Array<number | string>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 2,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 17, character: 7},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: void, y: void, z: Array<void>): void',
                  parameters: [
                    {
                      label: 'x: void',
                    },
                    {
                      label: 'y: void',
                    },
                    {
                      label: 'z: Array<void>',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 18, character: 10},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | void, y: number | void, z: Array<number | void>): void',
                  parameters: [
                    {
                      label: 'x: number | void',
                    },
                    {
                      label: 'y: number | void',
                    },
                    {
                      label: 'z: Array<number | void>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 1,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 19, character: 14},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | string, y: number | string, z: Array<number | string>): void',
                  parameters: [
                    {
                      label: 'x: number | string',
                    },
                    {
                      label: 'y: number | string',
                    },
                    {
                      label: 'z: Array<number | string>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 2,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 23, character: 13},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: void, y: void, z: Array<void>): void',
                  parameters: [
                    {
                      label: 'x: void',
                    },
                    {
                      label: 'y: void',
                    },
                    {
                      label: 'z: Array<void>',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 24, character: 16},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | void, y: number | void, z: Array<number | void>): void',
                  parameters: [
                    {
                      label: 'x: number | void',
                    },
                    {
                      label: 'y: number | void',
                    },
                    {
                      label: 'z: Array<number | void>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 1,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 25, character: 20},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | string, y: number | string, z: Array<number | string>): void',
                  parameters: [
                    {
                      label: 'x: number | string',
                    },
                    {
                      label: 'y: number | string',
                    },
                    {
                      label: 'z: Array<number | string>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 2,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 29, character: 9},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: void, y: void, z: Array<void>): void',
                  parameters: [
                    {
                      label: 'x: void',
                    },
                    {
                      label: 'y: void',
                    },
                    {
                      label: 'z: Array<void>',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 30, character: 12},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | void, y: number | void, z: Array<number | void>): void',
                  parameters: [
                    {
                      label: 'x: number | void',
                    },
                    {
                      label: 'y: number | void',
                    },
                    {
                      label: 'z: Array<number | void>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 1,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 31, character: 16},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | string, y: number | string, z: Array<number | string>): void',
                  parameters: [
                    {
                      label: 'x: number | string',
                    },
                    {
                      label: 'y: number | string',
                    },
                    {
                      label: 'z: Array<number | string>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 2,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 35, character: 8},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: void, y: void, z: Array<void>): void',
                  parameters: [
                    {
                      label: 'x: void',
                    },
                    {
                      label: 'y: void',
                    },
                    {
                      label: 'z: Array<void>',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 36, character: 11},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | void, y: number | void, z: Array<number | void>): void',
                  parameters: [
                    {
                      label: 'x: number | void',
                    },
                    {
                      label: 'y: number | void',
                    },
                    {
                      label: 'z: Array<number | void>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 1,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 37, character: 15},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | string, y: number | string, z: Array<number | string>): void',
                  parameters: [
                    {
                      label: 'x: number | string',
                    },
                    {
                      label: 'y: number | string',
                    },
                    {
                      label: 'z: Array<number | string>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 2,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 42, character: 8},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: '(x: void, y: void, z: Array<void>): void',
                  parameters: [
                    {
                      label: 'x: void',
                    },
                    {
                      label: 'y: void',
                    },
                    {
                      label: 'z: Array<void>',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 43, character: 11},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | void, y: number | void, z: Array<number | void>): void',
                  parameters: [
                    {
                      label: 'x: number | void',
                    },
                    {
                      label: 'y: number | void',
                    },
                    {
                      label: 'z: Array<number | void>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 1,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/calls_generic.js',
        },
        position: {line: 44, character: 15},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label:
                    '(x: number | string, y: number | string, z: Array<number | string>): void',
                  parameters: [
                    {
                      label: 'x: number | string',
                    },
                    {
                      label: 'y: number | string',
                    },
                    {
                      label: 'z: Array<number | string>',
                    },
                  ],
                },
              ],
              activeSignature: 0,
              activeParameter: 2,
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 16, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 20, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 24, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 29, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 33, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 37, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: P',
                  parameters: [
                    {
                      label: 'foo: P',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 41, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: P',
                  parameters: [
                    {
                      label: 'foo: P',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 45, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 49, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 53, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
                    },
                  ],
                },
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 57, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: boolean',
                  parameters: [
                    {
                      label: 'foo: boolean',
                    },
                  ],
                },
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 61, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo?: string',
                  parameters: [
                    {
                      label: 'foo?: string',
                    },
                  ],
                },
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 65, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number & boolean',
                  parameters: [
                    {
                      label: 'foo: number & boolean',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 69, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo?: number & boolean & string',
                  parameters: [
                    {
                      label: 'foo?: number & boolean & string',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 73, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo?: boolean & string',
                  parameters: [
                    {
                      label: 'foo?: boolean & string',
                    },
                  ],
                },
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 77, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo?: number & (boolean | string)',
                  parameters: [
                    {
                      label: 'foo?: number & (boolean | string)',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 81, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 85, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: () => PropsA["foo"]',
                  parameters: [
                    {
                      label: 'foo: () => PropsA["foo"]',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr.js',
        },
        position: {line: 89, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [],
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr_docs.js',
        },
        position: {line: 27, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
                      documentation: {
                        kind: 'markdown',
                        value: 'This is documentation for Props1.foo property',
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
          uri: '<PLACEHOLDER_PROJECT_URL>/jsx_attr_docs.js',
        },
        position: {line: 31, character: 22},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/signatureHelp',
            result: {
              signatures: [
                {
                  label: 'foo: string',
                  parameters: [
                    {
                      label: 'foo: string',
                      documentation: {
                        kind: 'markdown',
                        value: 'This is documentation for Props2.foo property',
                      },
                    },
                  ],
                },
                {
                  label: 'foo: number',
                  parameters: [
                    {
                      label: 'foo: number',
                      documentation: {
                        kind: 'markdown',
                        value: 'This is documentation for Props1.foo property',
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
  ],
): SuiteType);
