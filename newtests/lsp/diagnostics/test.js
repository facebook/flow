/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({ideStartAndConnect, ideRequestAndWaitUntilResponse, addFile}) => [
    test('textDocument/publishDiagnostics #1', [
      ideStartAndConnect(),
      addFile('witherrors1.js')
        .waitUntilIDEMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/publishDiagnostics{"Cannot return `23` because  number [1] is incompatible with  string [2].","message":"[1] number","message":"[2] string"}',
          ],
          ['window/progress'],
        ),
    ]),

    test('textDocument/publishDiagnostics #2', [
      ideStartAndConnect(),
      addFile('witherrors2.js')
        .waitUntilIDEMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/publishDiagnostics{"Cannot cast `b` to `A` because  number [1] is incompatible with  string [2] in property `x.y.z`."}',
          ],
          ['window/progress'],
        ),
    ]),
  ],
);
