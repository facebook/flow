/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({ideStartAndConnect, ideRequestAndWaitUntilResponse, addFile}) => [
    test('textDocument/definition', [
      addFile('definition.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/definition.js'},
        position: {line: 6, character: 1},
      }).verifyAllIDEMessagesInStep(
        [
          'textDocument/definition{definition.js,"start":{"line":2,"character":0}}',
        ],
        [],
      ),
    ]),

    test('textDocument/hover', [
      addFile('hover.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 6, character: 1}, // over a function use
      }).verifyAllIDEMessagesInStep(['textDocument/hover{() => number}'], []),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 3, character: 1}, // over whitespace
      }).verifyAllIDEMessagesInStep(['textDocument/hover{null}'], []),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 2, character: 1}, // over a keyword
      }).verifyAllIDEMessagesInStep(['textDocument/hover{null}'], []),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 0, character: 1}, // over a comment
      }).verifyAllIDEMessagesInStep(['textDocument/hover{null}'], []),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 6, character: 100}, // past the end of a line
      }).verifyAllIDEMessagesInStep(['textDocument/hover{null}'], []),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 100, character: 0}, // past the end of the file
      }).verifyAllIDEMessagesInStep(['textDocument/hover{null}'], []),
    ]),
  ],
);
