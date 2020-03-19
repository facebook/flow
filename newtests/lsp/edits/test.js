/*
 * @flow
 * @format
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    lspNotification,
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('didOpen+didChange+didClose', [
      lspStartAndConnect(),
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/open.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow

function jones(): number { return 15; }
jones();
`,
        },
      }).verifyAllLSPMessagesInStep([''], [...lspIgnoreStatusAndCancellation]),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/open.js'},
        position: {line: 3, character: 1},
      }).verifyAllLSPMessagesInStep(
        [['textDocument/definition', '{open.js,line":2}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspNotification('textDocument/didChange', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/open.js',
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
      }).verifyAllLSPMessagesInStep([''], [...lspIgnoreStatusAndCancellation]),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/open.js'},
        position: {line: 3, character: 1},
      }).verifyAllLSPMessagesInStep(
        [['textDocument/definition', '{open.js,"line":1}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspNotification('textDocument/didClose', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/open.js'},
      }).verifyAllLSPMessagesInStep([''], [...lspIgnoreStatusAndCancellation]),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/open.js'},
        position: {line: 3, character: 1},
      }).verifyAllLSPMessagesInStep(
        [['textDocument/definition', '{unexpected error}']],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
