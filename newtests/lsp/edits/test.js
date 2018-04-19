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
  }) => [
    test('didOpen+didChange+didClose', [
      ideStartAndConnect(),
      ideNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_DIR>/open.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow

function jones(): number { return 15; }
jones();
`,
        },
      }).verifyAllIDEMessagesInStep([''], []),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/open.js'},
        position: {line: 3, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{open.js,line":2}'],
        [],
      ),
      ideNotification('textDocument/didChange', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/open.js', version: 2},
        contentChanges: [
          {
            text: `// @flow
function wilbur(): string { return 'hello'; }
function jones(): number { return 15; }
wilbur();
`,
          },
        ],
      }),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/open.js'},
        position: {line: 3, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{open.js,"line":1}'],
        [],
      ),
      ideNotification('textDocument/didClose', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/open.js'},
      }),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/open.js'},
        position: {line: 3, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{No such file or directory}'],
        [],
      ),
    ]),
  ],
);
