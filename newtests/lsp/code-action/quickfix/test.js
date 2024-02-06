/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../../Tester';
const path = require('path');
const {suite, test} = require('../../../Tester');

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
    test('provide quickfix for adding optional chaining', [
      addFile('add-optional-chaining.js.ignored', 'add-optional-chaining.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/add-optional-chaining.js',
        },
        range: {
          start: {
            line: 3,
            character: 4,
          },
          end: {
            line: 3,
            character: 7,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-add-optional-chaining-1.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/add-optional-chaining.js',
        },
        range: {
          start: {
            line: 5,
            character: 7,
          },
          end: {
            line: 5,
            character: 10,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-add-optional-chaining-2.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for PropMissing errors with dot syntax', [
      addFile('prop-missing.js.ignored', 'prop-missing.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/prop-missing.js'},
        range: {
          start: {
            line: 3,
            character: 2,
          },
          end: {
            line: 3,
            character: 9,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 3,
                  character: 2,
                },
                end: {
                  line: 3,
                  character: 9,
                },
              },
              message:
                'Cannot get `x.faceboy` because property `faceboy` (did you mean `facebook`?) is missing in  object type [1].',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-prop-missing-dot-syntax.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for PropMissing errors with bracket syntax', [
      addFile(
        'prop-missing-bracket-syntax.js.ignored',
        'prop-missing-bracket-syntax.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/prop-missing-bracket-syntax.js',
        },
        range: {
          start: {
            line: 3,
            character: 2,
          },
          end: {
            line: 3,
            character: 11,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 3,
                  character: 2,
                },
                end: {
                  line: 3,
                  character: 11,
                },
              },
              message:
                'Cannot get `x.faceboy` because property `faceboy` (did you mean `facebook`?) is missing in  object type [1].',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-prop-missing-bracket-syntax.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for invalid enum member access errors', [
      addFile(
        'invalid-enum-member-access.js.ignored',
        'invalid-enum-member-access.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/invalid-enum-member-access.js',
        },
        range: {
          start: {
            line: 6,
            character: 2,
          },
          end: {
            line: 6,
            character: 8,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 6,
                  character: 2,
                },
                end: {
                  line: 6,
                  character: 8,
                },
              },
              message:
                'Cannot access property `Foobat` because `Foobat` is not a member of `enum E`. Did you meanthe member `Foobar`?',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-invalid-enum.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test("don't provide quickfixes for object subtyping errors", [
      addFile('object-cast.js.ignored', 'object-cast.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/object-cast.js'},
        range: {
          start: {
            line: 3,
            character: 1,
          },
          end: {
            line: 3,
            character: 14,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 3,
                  character: 1,
                },
                end: {
                  line: 3,
                  character: 14,
                },
              },
              message:
                'Cannot cast object literal to `T` because property `floo` (did you mean `foo`?) is missing in  `T` [1] but exists in  object literal [2].',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
            {
              range: {
                start: {
                  line: 3,
                  character: 1,
                },
                end: {
                  line: 3,
                  character: 14,
                },
              },
              message:
                'Cannot cast object literal to `T` because property `foo` (did you mean `floo`?) is missing in  object literal [1] but exists in  `T` [2].',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [],
          },
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
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
          'quickfix-renders-type-parse-error.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for ClassObject errors', [
      addFile('class-object-subtype.js.ignored', 'class-object-subtype.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/class-object-subtype.js',
        },
        range: {
          start: {
            line: 8,
            character: 4,
          },
          end: {
            line: 8,
            character: 11,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 8,
                  character: 4,
                },
                end: {
                  line: 8,
                  character: 11,
                },
              },
              message:
                'Cannot call foo with new A() bound to x because cannot subtype class A [1] with object type [2]. Please use an interface instead.',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-class-object-basic.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('provide quickfix for nested ClassObject errors', [
      addFile('class-object-subtype.js.ignored', 'class-object-subtype.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/class-object-subtype.js',
        },
        range: {
          start: {
            line: 12,
            character: 9,
          },
          end: {
            line: 12,
            character: 16,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 12,
                  character: 9,
                },
                end: {
                  line: 12,
                  character: 16,
                },
              },
              message:
                'Cannot call `bar` with object literal bound to `_` because cannot subtype class  `A` [1] with  object type [2]. Please use an interface instead in property `i`.',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-class-object-nested.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('provide quickfix for aliased ClassObject errors', [
      addFile('class-object-subtype.js.ignored', 'class-object-subtype.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/class-object-subtype.js',
        },
        range: {
          start: {
            line: 18,
            character: 4,
          },
          end: {
            line: 18,
            character: 11,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 18,
                  character: 4,
                },
                end: {
                  line: 18,
                  character: 4,
                },
              },
              message:
                'Cannot call `baz` with object literal bound to `_` because cannot subtype class  `A` [1] with  object type [2]. Please use an interface instead in property `i`.',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-class-object-aliased.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('provide codeAction for cross-file ClassObject errors', [
      addFile('class-object-subtype.js.ignored', 'class-object-subtype.js'),
      addFile('lib.js.ignored', 'lib.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/class-object-subtype.js',
        },
        range: {
          start: {
            line: 22,
            character: 4,
          },
          end: {
            line: 22,
            character: 11,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 22,
                  character: 4,
                },
                end: {
                  line: 22,
                  character: 11,
                },
              },
              message:
                'Cannot call `qux` with object literal bound to `_` because cannot subtype class  `A` [1] with  object type [2]. Please use an interface instead in property `i`.',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-class-object-cross-file.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('provide codeAction for MethodUnbinding errors', [
      addFile('method-unbinding.js.ignored', 'method-unbinding.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/method-unbinding.js',
        },
        range: {
          start: {
            line: 6,
            character: 8,
          },
          end: {
            line: 6,
            character: 9,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [
            {
              range: {
                start: {
                  line: 6,
                  character: 8,
                },
                end: {
                  line: 6,
                  character: 9,
                },
              },
              message:
                'Cannot get `(new A).f` because  property `f` [1] cannot be unbound from the  context [2] where it was defined.',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-method-unbinding.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('ignore method unbinding when super is used', [
      addFile('method-unbinding.js.ignored', 'method-unbinding.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/method-unbinding.js',
        },
        range: {
          start: {
            line: 6,
            character: 8,
          },
          end: {
            line: 6,
            character: 9,
          },
        },
        context: {
          diagnostics: [
            {
              range: {
                start: {
                  line: 12,
                  character: 8,
                },
                end: {
                  line: 12,
                  character: 9,
                },
              },
              message:
                'Cannot get `(new B).f` because  property `f` [1] cannot be unbound from the  context [2] where it was defined.',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyAllLSPMessagesInStep(
        [['textDocument/codeAction', '[]']],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for unused promise errors', [
      addFile('fix-unused-promise.js.ignored', 'fix-unused-promise.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-unused-promise.js',
        },
        range: {
          start: {
            line: 5,
            character: 5,
          },
          end: {
            line: 5,
            character: 5,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-unused-promise.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide autoimport for missing import', [
      addFile('import-provider.js.ignored', 'import-provider.js'),
      addFile('fix-missing-import.js.ignored', 'fix-missing-import.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-missing-import.js',
        },
        range: {
          start: {line: 4, character: 9},
          end: {line: 4, character: 10},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-auto-import.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide autoimport for missing React import', [
      addFile('fix-missing-import.js.ignored', 'fix-missing-import.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-missing-import.js',
        },
        range: {
          start: {line: 6, character: 4},
          end: {line: 6, character: 5},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-auto-import-react.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `unknown` type', [
      addFile('fix-unknown-type.js.ignored', 'fix-unknown-type.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-unknown-type.js',
        },
        range: {
          start: {
            line: 2,
            character: 9,
          },
          end: {
            line: 2,
            character: 16,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-unknown-type.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `never` type', [
      addFile('fix-never-type.js.ignored', 'fix-never-type.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-never-type.js',
        },
        range: {
          start: {
            line: 2,
            character: 9,
          },
          end: {
            line: 2,
            character: 14,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-never-type.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `undefined` type', [
      addFile('fix-undefined-type.js.ignored', 'fix-undefined-type.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-undefined-type.js',
        },
        range: {
          start: {
            line: 2,
            character: 9,
          },
          end: {
            line: 2,
            character: 18,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-undefined-type.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `keyof`', [
      addFile('fix-keyof.js.ignored', 'fix-keyof.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-keyof.js',
        },
        range: {
          start: {
            line: 2,
            character: 9,
          },
          end: {
            line: 2,
            character: 16,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-keyof-type.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `extends` in type param', [
      addFile('fix-type-param-extends.js.ignored', 'fix-type-param-extends.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-type-param-extends.js',
        },
        range: {
          start: {
            line: 2,
            character: 7,
          },
          end: {
            line: 2,
            character: 23,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-extends-in-type-param.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `readonly` variance', [
      addFile('fix-readonly-variance.js.ignored', 'fix-readonly-variance.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-readonly-variance.js',
        },
        range: {
          start: {
            line: 2,
            character: 10,
          },
          end: {
            line: 2,
            character: 18,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-readonly-variance.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `in` variance', [
      addFile('fix-in-variance.js.ignored', 'fix-in-variance.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-in-variance.js',
        },
        range: {
          start: {
            line: 2,
            character: 20,
          },
          end: {
            line: 2,
            character: 21,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-in-variance.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `out` variance', [
      addFile('fix-out-variance.js.ignored', 'fix-out-variance.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-out-variance.js',
        },
        range: {
          start: {
            line: 2,
            character: 20,
          },
          end: {
            line: 2,
            character: 21,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-out-variance.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `in out` variance', [
      addFile('fix-in-out-variance.js.ignored', 'fix-in-out-variance.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-in-out-variance.js',
        },
        range: {
          start: {
            line: 2,
            character: 20,
          },
          end: {
            line: 2,
            character: 21,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-inout-variance.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `(x: T)` type cast', [
      addFile('fix-colon-cast.js.ignored', 'fix-colon-cast.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-colon-cast.js',
        },
        range: {
          start: {
            line: 2,
            character: 0,
          },
          end: {
            line: 2,
            character: 14,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-old-flow-type-cast.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]).flowConfig('_flowconfig_casting_syntax'),
    test('provide quickfix for `satisfies` type cast', [
      addFile(
        'fix-satisfies-expression.js.ignored',
        'fix-satisfies-expression.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-satisfies-expression.js',
        },
        range: {
          start: {
            line: 2,
            character: 0,
          },
          end: {
            line: 2,
            character: 21,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-satisfies-type-cast.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `readonly` array type', [
      addFile(
        'fix-readonly-array-type.js.ignored',
        'fix-readonly-array-type.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-readonly-array-type.js',
        },
        range: {
          start: {
            line: 2,
            character: 10,
          },
          end: {
            line: 2,
            character: 26,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-readonly-array-type.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `readonly` tuple type', [
      addFile(
        'fix-readonly-tuple-type.js.ignored',
        'fix-readonly-tuple-type.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-readonly-tuple-type.js',
        },
        range: {
          start: {
            line: 2,
            character: 10,
          },
          end: {
            line: 2,
            character: 34,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-readonly-tuple-type.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `Readonly` type', [
      addFile('fix-readonly-type.js.ignored', 'fix-readonly-type.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-readonly-type.js',
        },
        range: {
          start: {
            line: 1,
            character: 9,
          },
          end: {
            line: 1,
            character: 17,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-ts-readonly-type.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `ReadonlyArray` type', [
      addFile('fix-readonlyarray-type.js.ignored', 'fix-readonlyarray-type.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-readonlyarray-type.js',
        },
        range: {
          start: {
            line: 1,
            character: 9,
          },
          end: {
            line: 1,
            character: 22,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-readonly-array-no-dollar-type.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `ReadonlyMap` type', [
      addFile('fix-readonlymap-type.js.ignored', 'fix-readonlymap-type.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-readonlymap-type.js',
        },
        range: {
          start: {
            line: 1,
            character: 9,
          },
          end: {
            line: 1,
            character: 20,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-readonly-map-no-dollar-type.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `ReadonlySet` type', [
      addFile('fix-readonlyset-type.js.ignored', 'fix-readonlyset-type.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-readonlyset-type.js',
        },
        range: {
          start: {
            line: 1,
            character: 9,
          },
          end: {
            line: 1,
            character: 20,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-readonly-set-no-dollar-type.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `NonNullable` type', [
      addFile('fix-nonnullable-type.js.ignored', 'fix-nonnullable-type.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-nonnullable-type.js',
        },
        range: {
          start: {
            line: 1,
            character: 9,
          },
          end: {
            line: 1,
            character: 20,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-non-nullable-type.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `$Partial`', [
      addFile('fix-partial-type.js.ignored', 'fix-partial-type.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-partial-type.js',
        },
        range: {
          start: {
            line: 1,
            character: 9,
          },
          end: {
            line: 1,
            character: 17,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-dollar-partial-type.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for `$Shape`', [
      addFile('fix-shape-type.js.ignored', 'fix-shape-type.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-shape-type.js',
        },
        range: {
          start: {
            line: 1,
            character: 9,
          },
          end: {
            line: 1,
            character: 15,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-dollar-shape.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide quickfix for class member access', [
      addFile(
        'fix-class-member-access.js.ignored',
        'fix-class-member-access.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-class-member-access.js',
        },
        range: {
          start: {
            line: 8,
            character: 7,
          },
          end: {
            line: 8,
            character: 7,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-class-member-access-1.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-class-member-access.js',
        },
        range: {
          start: {
            line: 10,
            character: 7,
          },
          end: {
            line: 10,
            character: 7,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-class-member-access-2.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-class-member-access.js',
        },
        range: {
          start: {
            line: 12,
            character: 7,
          },
          end: {
            line: 12,
            character: 7,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'quickfix-class-member-access-3.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
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
          start: {line: 7, character: 24},
          end: {line: 7, character: 24},
        },
        context: {only: ['quickfix'], diagnostics: []},
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-invalid-render-1.json'),
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
  ],
): SuiteType);
