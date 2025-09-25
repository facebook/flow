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
    test('provide quickfix through annotation for invariant subtyping errors', [
      addFile(
        'add-annotation-for-invariant-subtyping-error.js.ignored',
        'add-annotation-for-invariant-subtyping-error.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/add-annotation-for-invariant-subtyping-error.js',
        },
        range: {
          start: {
            line: 5,
            character: 1,
          },
          end: {
            line: 5,
            character: 2,
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
          'quickfix-add-annotation-for-invariant-subtyping-error.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
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
            character: 3,
          },
          end: {
            line: 3,
            character: 3,
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
            character: 3,
          },
          end: {
            line: 3,
            character: 3,
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
            character: 3,
          },
          end: {
            line: 6,
            character: 3,
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
            character: 3,
          },
          end: {
            line: 3,
            character: 3,
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
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('provide quickfix for type-as-value errors', [
      addFile('type-as-value.js.ignored', 'type-as-value.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/type-as-value.js',
        },
        range: {
          start: {
            line: 3,
            character: 1,
          },
          end: {
            line: 3,
            character: 2,
          },
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'quickfix-type-as-value.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
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
            character: 5,
          },
          end: {
            line: 8,
            character: 5,
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
            character: 9,
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
            character: 5,
          },
          end: {
            line: 18,
            character: 5,
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
            character: 4,
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
  ],
): SuiteType);
