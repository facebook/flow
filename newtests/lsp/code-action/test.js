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
        ['initialize{"codeActionProvider":{"codeActionKinds":["quickfix"]}}'],
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
        ['initialize{"codeActionProvider":false}'],
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
          `textDocument/codeAction{${JSON.stringify([
            {
              title: 'Apply suggestion',
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
                command: 'log',
                arguments: ['Apply suggestion'],
              },
            },
          ])}}`,
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
