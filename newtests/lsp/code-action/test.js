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
    test('initialize with quickfix support', [
      lspStart({needsFlowServer: false}),
      lspRequestAndWaitUntilResponse(
        'initialize',
        lspInitializeParams,
      ).verifyAllLSPMessagesInStep(
        [
          [
            'initialize',
            '{"codeActionProvider":{"codeActionKinds":["quickfix"]}}',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('initialize without quickfix support', [
      lspStart({needsFlowServer: false}),
      lspRequestAndWaitUntilResponse('initialize', {
        ...lspInitializeParams,
        capabilities: {
          ...lspInitializeParams.capabilities,
          textDocument: {
            ...lspInitializeParams.capabilities.textDocument,
            codeAction: {},
          },
        },
      }).verifyAllLSPMessagesInStep(
        [['initialize', '{"codeActionProvider":false}']],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide codeAction for PropMissing errors with dot syntax', [
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
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                title: 'Replace `faceboy` with `facebook`',
                kind: 'quickfix',
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
                    severity: 1,
                    code: 'InferError',
                    source: 'Flow',
                    message:
                      'Cannot get `x.faceboy` because property `faceboy` (did you mean `facebook`?) is missing in  object type [1].',
                    relatedInformation: [],
                    relatedLocations: [],
                  },
                ],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/prop-missing.js': [
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
                        newText: 'facebook',
                      },
                    ],
                  },
                },
                command: {
                  title: '',
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  arguments: [
                    'textDocument/codeAction',
                    'replace_prop_typo_at_target',
                    'Replace `faceboy` with `facebook`',
                  ],
                },
              },
            ],
          },
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide codeAction for PropMissing errors with bracket syntax', [
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
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                title: 'Replace `faceboy` with `facebook`',
                kind: 'quickfix',
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
                    severity: 1,
                    code: 'InferError',
                    source: 'Flow',
                    message:
                      'Cannot get `x.faceboy` because property `faceboy` (did you mean `facebook`?) is missing in  object type [1].',
                    relatedInformation: [],
                    relatedLocations: [],
                  },
                ],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/prop-missing-bracket-syntax.js': [
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
                        newText: '"facebook"',
                      },
                    ],
                  },
                },
                command: {
                  title: '',
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  arguments: [
                    'textDocument/codeAction',
                    'replace_prop_typo_at_target',
                    'Replace `faceboy` with `facebook`',
                  ],
                },
              },
            ],
          },
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide codeAction for invalid enum member access errors', [
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
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                title: 'Replace `Foobat` with `Foobar`',
                kind: 'quickfix',
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
                    severity: 1,
                    code: 'InferError',
                    source: 'Flow',
                    message:
                      'Cannot access property `Foobat` because `Foobat` is not a member of `enum E`. Did you meanthe member `Foobar`?',
                    relatedInformation: [],
                    relatedLocations: [],
                  },
                ],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/invalid-enum-member-access.js': [
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
                        newText: 'Foobar',
                      },
                    ],
                  },
                },
                command: {
                  title: '',
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  arguments: [
                    'textDocument/codeAction',
                    'replace_enum_prop_typo_at_target',
                    'Replace `Foobat` with `Foobar`',
                  ],
                },
              },
            ],
          },
        ],
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
        [{method: 'textDocument/codeAction', result: []}],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide codeAction for parse error', [
      addFile('parse-error.js.ignored', 'parse-error.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/parse-error.js'},
        range: {
          start: {
            line: 2,
            character: 6,
          },
          end: {
            line: 2,
            character: 6,
          },
        },
        context: {
          diagnostics: [
            {
              range: {
                start: {
                  line: 2,
                  character: 6,
                },
                end: {
                  line: 2,
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
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                title: "Replace `>` with `{'>'}`",
                kind: 'quickfix',
                diagnostics: [
                  {
                    range: {
                      start: {
                        line: 2,
                        character: 6,
                      },
                      end: {
                        line: 2,
                        character: 7,
                      },
                    },
                    severity: 1,
                    code: 'ParseError',
                    source: 'Flow',
                    message: "Unexpected token `>`. Did you mean `{'>'}`?",
                    relatedInformation: [],
                    relatedLocations: [],
                  },
                ],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/parse-error.js': [
                      {
                        range: {
                          start: {
                            line: 2,
                            character: 6,
                          },
                          end: {
                            line: 2,
                            character: 7,
                          },
                        },
                        newText: "{'>'}",
                      },
                    ],
                  },
                },
                command: {
                  title: '',
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  arguments: [
                    'textDocument/codeAction',
                    'typo',
                    "Replace `>` with `{'>'}`",
                  ],
                },
              },
            ],
          },
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide codeAction for ClassObject errors', [
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
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                title: 'Rewrite object type as an interface',
                kind: 'quickfix',
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
                    severity: 1,
                    code: 'InferError',
                    source: 'Flow',
                    message:
                      'Cannot call foo with new A() bound to x because cannot subtype class A [1] with object type [2]. Please use an interface instead.',
                    relatedInformation: [],
                    relatedLocations: [],
                  },
                ],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/class-object-subtype.js': [
                      {
                        range: {
                          start: {
                            line: 6,
                            character: 17,
                          },
                          end: {
                            line: 6,
                            character: 35,
                          },
                        },
                        newText: 'interface { x: number }',
                      },
                    ],
                  },
                },
                command: {
                  title: '',
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  arguments: [
                    'textDocument/codeAction',
                    'replace_obj_with_interface',
                    'Rewrite object type as an interface',
                  ],
                },
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
    test('provide codeAction for nested ClassObject errors', [
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
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                title: 'Rewrite object type as an interface',
                kind: 'quickfix',
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
                    severity: 1,
                    code: 'InferError',
                    source: 'Flow',
                    message:
                      'Cannot call `bar` with object literal bound to `_` because cannot subtype class  `A` [1] with  object type [2]. Please use an interface instead in property `i`.',
                    relatedInformation: [],
                    relatedLocations: [],
                  },
                ],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/class-object-subtype.js': [
                      {
                        range: {
                          start: {
                            line: 10,
                            character: 23,
                          },
                          end: {
                            line: 10,
                            character: 39,
                          },
                        },
                        newText: 'interface { x: number }',
                      },
                    ],
                  },
                },
                command: {
                  title: '',
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  arguments: [
                    'textDocument/codeAction',
                    'replace_obj_with_interface',
                    'Rewrite object type as an interface',
                  ],
                },
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
    test('provide codeAction for aliased ClassObject errors', [
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
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                title: 'Rewrite `T` as an interface',
                kind: 'quickfix',
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
                    severity: 1,
                    code: 'InferError',
                    source: 'Flow',
                    message:
                      'Cannot call `baz` with object literal bound to `_` because cannot subtype class  `A` [1] with  object type [2]. Please use an interface instead in property `i`.',
                    relatedInformation: [],
                    relatedLocations: [],
                  },
                ],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/class-object-subtype.js': [
                      {
                        range: {
                          start: {
                            line: 14,
                            character: 9,
                          },
                          end: {
                            line: 14,
                            character: 27,
                          },
                        },
                        newText: 'interface { x: number }',
                      },
                    ],
                  },
                },
                command: {
                  title: '',
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  arguments: [
                    'textDocument/codeAction',
                    'replace_obj_with_interface',
                    'Rewrite `T` as an interface',
                  ],
                },
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
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [
              {
                title: 'Rewrite object type as an interface',
                kind: 'quickfix',
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
                    severity: 1,
                    code: 'InferError',
                    source: 'Flow',
                    message:
                      'Cannot call `qux` with object literal bound to `_` because cannot subtype class  `A` [1] with  object type [2]. Please use an interface instead in property `i`.',
                    relatedInformation: [],
                    relatedLocations: [],
                  },
                ],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/lib.js': [
                      {
                        range: {
                          start: {
                            line: 2,
                            character: 24,
                          },
                          end: {
                            line: 2,
                            character: 42,
                          },
                        },
                        newText: 'interface { x: number }',
                      },
                    ],
                  },
                },
                command: {
                  title: '',
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  arguments: [
                    'textDocument/codeAction',
                    'replace_obj_with_interface',
                    'Rewrite object type as an interface',
                  ],
                },
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
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/codeAction',
            JSON.stringify([
              {
                title: 'Rewrite function as an arrow function',
                kind: 'quickfix',
                diagnostics: [
                  {
                    range: {
                      start: {line: 6, character: 8},
                      end: {line: 6, character: 9},
                    },
                    severity: 1,
                    code: 'InferError',
                    source: 'Flow',
                    message:
                      'Cannot get `(new A).f` because  property `f` [1] cannot be unbound from the  context [2] where it was defined.',
                    relatedInformation: [],
                    relatedLocations: [],
                  },
                ],
                edit: {
                  changes: {
                    '<PLACEHOLDER_PROJECT_URL>/method-unbinding.js': [
                      {
                        range: {
                          start: {
                            line: 2,
                            character: 0,
                          },
                          end: {
                            line: 4,
                            character: 1,
                          },
                        },
                        newText:
                          'class A {\n  f = (x: number): number => {\n    return x;\n  };\n}',
                      },
                    ],
                  },
                },
                command: {
                  title: '',
                  command: 'log:org.flow:<PLACEHOLDER_PROJECT_URL>',
                  arguments: [
                    'textDocument/codeAction',
                    'replace_method_with_arrow',
                    'Rewrite function as an arrow function',
                  ],
                },
              },
            ]),
          ],
        ],
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
  ],
): Suite);
