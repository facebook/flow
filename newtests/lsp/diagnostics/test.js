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
        // Flow may send multiple publishDiagnostics when reporting partial
        // progress and then complete results, e.g. an empty publishDiagnostics
        // followed by the final results, or a partial publishDiagnostics followed
        // by the final results, or the full results twice (in case the partial
        // results happened to be identical), or just the full results once.
        // Therefore: when we wait in a step to get publishDiagnostics, it's
        // racey as to whether we'll get one or more publishDiagnostics in the step.
        // To be robust against races: we'll wait up to 9s to get the
        // expected publishDiagnostic, then verify that this expected publishDiagnostic
        // was sent at least once, and ignore any additional publishDiagnostics.
        .waitUntilIDEMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot return}',
        )
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/publishDiagnostics{"Cannot return `23` because  number [1] is incompatible with  string [2].","message":"[1] number","message":"[2] string"}',
          ],
          ['window/progress', 'textDocument/publishDiagnostics'],
        ),
    ]),

    test('textDocument/publishDiagnostics #2', [
      ideStartAndConnect(),
      addFile('witherrors2.js')
        .waitUntilIDEMessage(
          9000,
          'textDocument/publishDiagnostics{Cannot cost}',
        )
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/publishDiagnostics{"Cannot cast `b` to `A` because  number [1] is incompatible with  string [2] in property `x.y.z`."}',
          ],
          ['window/progress', 'textDocument/publishDiagnostics'],
        ),
    ]),
  ],
);
