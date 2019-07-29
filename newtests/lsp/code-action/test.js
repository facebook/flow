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
    test('textDocument/codeAction #1', [
      addFile('error1.js'),
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
              message: 'Missing type annotation for `a`.',
              severity: 1,
              code: 'InferError',
              source: 'Flow',
            },
          ],
        },
      }).verifyAllIDEMessagesInStep(
        ['textDocument/codeAction{[]}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
