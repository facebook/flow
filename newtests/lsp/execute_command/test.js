/*
 * @flow
 * @format
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('executeCommand log', [
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('workspace/executeCommand', {
        command: 'log',
        arguments: ['test'],
      }).verifyAllLSPMessagesInStep(
        ['workspace/executeCommand'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
