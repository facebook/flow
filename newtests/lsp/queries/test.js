/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    ideStartAndConnect,
    ideRequest,
    ideNotification,
    ideRequestAndWaitUntilResponse,
    addFile,
  }) => [
    test('invalid_method', [
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('foobar', {}).verifyAllIDEMessagesInStep(
        ['foobar{not implemented}'],
        [],
      ),
      ideNotification('barfoo', {})
        .waitUntilIDEMessage(2000, 'barfoo')
        .verifyAllIDEMessagesInStep(['telemetry/event{not implemented}'], []),
    ]),

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

    test('textDocument/completion', [
      addFile('completion.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/completion.js'},
        position: {line: 10, character: 15}, // statement position
      }).verifyAllIDEMessagesInStep(
        [
          'textDocument/completion{"label":"x","label":"fred","detail":"(a: number, b: string) => number","inlineDetail":"(a: number, b: string)"}',
        ],
        [],
      ),
    ]),
  ],
);
