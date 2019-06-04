/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    ideStartAndConnect,
    ideRequestAndWaitUntilResponse,
    ideNotification,
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('didOpen+didChange+didClose', [
      ideStartAndConnect(),
      ideNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow

function jones(): number { return 15; }
jones();
`,
        },
      }).verifyAllIDEMessagesInStep([''], [...lspIgnoreStatusAndCancellation]),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
        position: {line: 3, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{open.js,line":2}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js',
          version: 2,
        },
        contentChanges: [
          {
            text: `// @flow
function wilbur(): string { return 'hello'; }
function jones(): number { return 15; }
wilbur();
`,
          },
        ],
      }).verifyAllIDEMessagesInStep([''], [...lspIgnoreStatusAndCancellation]),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
        position: {line: 3, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{open.js,"line":1}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideNotification('textDocument/didClose', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
      }).verifyAllIDEMessagesInStep([''], [...lspIgnoreStatusAndCancellation]),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
        position: {line: 3, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{unexpected error}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
