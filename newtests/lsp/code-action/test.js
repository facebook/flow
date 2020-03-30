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
    test('provide codeAction for PropMissing errors', [
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
          [
            'textDocument/codeAction',
            JSON.stringify([
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
                  command: 'log:<PLACEHOLDER_PROJECT_URL>',
                  arguments: ['Replace `faceboy` with `facebook`'],
                },
              },
            ]),
          ],
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
          [
            'textDocument/codeAction',
            JSON.stringify([
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
                  command: 'log:<PLACEHOLDER_PROJECT_URL>',
                  arguments: ['Replace `Foobat` with `Foobar`'],
                },
              },
            ]),
          ],
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
        [['textDocument/codeAction', '[]']],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
