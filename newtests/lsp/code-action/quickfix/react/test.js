/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../../../Tester';
const path = require('path');
const {suite, test} = require('../../../../Tester');

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
    test('provide quickfix for parse error', [
      addFile('parse-error.js.ignored', 'parse-error.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/parse-error.js'},
        range: {
          start: {
            line: 4,
            character: 6,
          },
          end: {
            line: 4,
            character: 6,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 4,
                  character: 6,
                },
                end: {
                  line: 4,
                  character: 7,
                },
              },
              message: "Unexpected token `>`. Did you mean `{'>'}`?",
              severity: 1,
              code: 'ParseError',
              relatedInformation: [],
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-parse-error.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test(
      'provide quickfix for component optional string param names with as renaming',
      [
        addFile(
          'component-as-renaming-with-optional-parse-error.js.ignored',
          'component-as-renaming-with-optional-parse-error.js',
        ),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/codeAction', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/component-as-renaming-with-optional-parse-error.js',
          },
          range: {
            start: {
              line: 1,
              character: 23,
            },
            end: {
              line: 1,
              character: 23,
            },
          },
          context: {
            only: ['quickfix'],
            diagnostics: [
              {
                range: {
                  start: {
                    line: 1,
                    character: 23,
                  },
                  end: {
                    line: 1,
                    character: 24,
                  },
                },
                message:
                  "You must use `'string-key' as localBinding?: <TYPE>` for props with invalid identifier names.",
                severity: 1,
                code: 'ParseError',
                relatedInformation: [],
                source: 'Flow',
              },
            ],
          },
        }).verifyLSPMessageSnapshot(
          path.join(
            __dirname,
            '__snapshots__',
            'quickfix-component-parse-error-optional-string-key.json',
          ),
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
      ],
    ),
    test('provide quickfix for component string param names with as renaming', [
      addFile(
        'component-as-renaming-parse-error.js.ignored',
        'component-as-renaming-parse-error.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/component-as-renaming-parse-error.js',
        },
        range: {
          start: {
            line: 1,
            character: 23,
          },
          end: {
            line: 1,
            character: 23,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 1,
                  character: 23,
                },
                end: {
                  line: 1,
                  character: 24,
                },
              },
              message:
                "You must use `'string-key' as localBinding: <TYPE>` for props with invalid identifier names.",
              severity: 1,
              code: 'ParseError',
              relatedInformation: [],
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-component-parse-error-string-key.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for component render type parse error', [
      addFile(
        'component-render-parse-error.js.ignored',
        'component-render-parse-error.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/component-render-parse-error.js',
        },
        range: {
          start: {
            line: 1,
            character: 15,
          },
          end: {
            line: 1,
            character: 15,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 1,
                  character: 15,
                },
                end: {
                  line: 1,
                  character: 16,
                },
              },
              message:
                'Components use `renders` instead of `:` to annotate the render type of a component.',
              severity: 1,
              code: 'ParseError',
              relatedInformation: [],
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-renders-type-parse-error-1.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/component-render-parse-error.js',
        },
        range: {
          start: {
            line: 5,
            character: 15,
          },
          end: {
            line: 5,
            character: 15,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 5,
                  character: 15,
                },
                end: {
                  line: 5,
                  character: 16,
                },
              },
              message:
                'Components use `renders` instead of `:` to annotate the render type of a component.',
              severity: 1,
              code: 'ParseError',
              relatedInformation: [],
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-renders-type-parse-error-2.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for invalid component prop', [
      addFile('invalid-component-prop.js.ignored', 'invalid-component-prop.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-component-prop.js',
        },
        range: {
          start: {line: 6, character: 15},
          end: {line: 6, character: 15},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-invalid-component-prop-1.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-component-prop.js',
        },
        range: {
          start: {line: 16, character: 15},
          end: {line: 16, character: 15},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-invalid-component-prop-2.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-component-prop.js',
        },
        range: {
          start: {line: 23, character: 15},
          end: {line: 23, character: 15},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-invalid-component-prop-3.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('provide quickfix for invalid render arguments', [
      addFile('invalid-renders.js.ignored', 'invalid-renders.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 8, character: 24},
          end: {line: 8, character: 24},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-invalid-render-2.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 9, character: 24},
          end: {line: 9, character: 24},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-invalid-render-3.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 10, character: 24},
          end: {line: 10, character: 24},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-invalid-render-4.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 11, character: 24},
          end: {line: 11, character: 24},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-invalid-render-5.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 12, character: 24},
          end: {line: 12, character: 24},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-invalid-render-6.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 13, character: 24},
          end: {line: 13, character: 24},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-invalid-render-7.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 14, character: 24},
          end: {line: 14, character: 24},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-invalid-render-8.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 15, character: 24},
          end: {line: 15, character: 24},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-invalid-render-9.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 16, character: 24},
          end: {line: 16, character: 24},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-invalid-render-10.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 17, character: 26},
          end: {line: 17, character: 26},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-invalid-render-11.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-renders.js',
        },
        range: {
          start: {line: 18, character: 26},
          end: {line: 18, character: 26},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-invalid-render-12.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test(
      'provide quickfix for adding missing JSX attributes',
      [
        addFile(
          'add-missing-attributes.js.ignored',
          'add-missing-attributes.js',
        ),
        lspStartAndConnect(),
      ].concat(
        [12, 17, 22, 27, 31, 35, 39, 43, 47, 51, 56, 61, 66, 81, 87].map(
          (i: number, idx: number) =>
            lspRequestAndWaitUntilResponse('textDocument/codeAction', {
              textDocument: {
                uri: '<PLACEHOLDER_PROJECT_URL>/add-missing-attributes.js',
              },
              range: {
                start: {line: i, character: 13},
                end: {line: i, character: 15},
              },
              context: {
                only: ['quickfix'],
                diagnostics: [],
              },
            }).verifyLSPMessageSnapshot(
              path.join(
                __dirname,
                '__snapshots__',
                'add-missing-attributes-' + (idx + 1) + '.json',
              ),
              [
                'textDocument/publishDiagnostics',
                ...lspIgnoreStatusAndCancellation,
              ],
            ),
        ),
      ),
    ),
    test(
      'provide quickfix for adding missing JSX attributes with snippets enabled',
      [
        addFile(
          'add-missing-attributes.js.ignored',
          'add-missing-attributes.js',
        ),
        lspStartAndConnect(6000, {
          ...lspInitializeParams,
          capabilities: {
            ...lspInitializeParams.capabilities,
            experimental: {
              ...lspInitializeParams.capabilities.experimental,
              snippetTextEdit: true,
            },
          },
        }),
        lspRequestAndWaitUntilResponse('textDocument/codeAction', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/add-missing-attributes.js',
          },
          range: {
            start: {line: 12, character: 13},
            end: {line: 12, character: 15},
          },
          context: {
            only: ['quickfix'],
            diagnostics: [],
          },
        }).verifyLSPMessageSnapshot(
          path.join(
            __dirname,
            '__snapshots__',
            'add-missing-attributes-snippets.json',
          ),
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
      ],
    ),
    test('provide quickfix to stub out react component', [
      addFile(
        'stub-out-react-component.js.ignored',
        'stub-out-react-component.js',
      ),
      lspStartAndConnect(6000, {...lspInitializeParams}),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/stub-out-react-component.js',
        },
        range: {
          start: {line: 2, character: 4},
          end: {line: 2, character: 4},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'stub-out-react-component-intrinsic-no-result.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/stub-out-react-component.js',
        },
        range: {
          start: {line: 4, character: 4},
          end: {line: 4, character: 4},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'stub-out-react-component-unbound.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/stub-out-react-component.js',
        },
        range: {
          start: {line: 7, character: 4},
          end: {line: 7, character: 4},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'stub-out-react-component-defined-no-result.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
): SuiteType);
