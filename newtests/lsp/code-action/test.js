/*
 * @flow
 * @format
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    ideStartAndConnect,
    ideStart,
    ideRequest,
    lspInitializeParams,
    ideRequestAndWaitUntilResponse,
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('textDocument/codeAction #0', [
      addFile('error1.js.ignored', 'error1.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>error1.js',
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
      }).verifyAllIDEMessagesInStep(
        [`textDocument/codeAction{[]}`],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/codeAction #1', [
      addFile('error1.js.ignored', 'error1.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>error1.js',
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
              message: 'Missing type annotation for `a`.',
            },
          ],
        },
      }).verifyAllIDEMessagesInStep(
        [
          `textDocument/codeAction{${JSON.stringify([
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
                  message: 'Missing type annotation for `a`.',
                  relatedInformation: [],
                  relatedLocations: [],
                },
              ],
              edit: {
                changes: {
                  '<PLACEHOLDER_PROJECT_URL_SLASH>error1.js': [
                    {
                      range: {
                        start: {line: 1, character: 22},
                        end: {line: 1, character: 22},
                      },
                      newText: ': any',
                    },
                  ],
                },
              },
            },
          ])}}`,
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('textDocument/codeAction #2', [
      addFile('error1.js.ignored', 'error1.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>error1.js',
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
      }).verifyAllIDEMessagesInStep(
        [
          `textDocument/codeAction{${JSON.stringify([
            {
              title: 'insert type annotation',
              kind: 'quickfix',
              diagnostics: [],
              edit: {
                changes: {
                  '<PLACEHOLDER_PROJECT_URL_SLASH>error1.js': [
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
                        ': {a: number, b: (a: any, b: string) => number}',
                    },
                  ],
                },
              },
            },
          ])}}`,
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
