/*
 * @flow
 * @format
 */

import type {Suite} from 'flow-dev-tools/src/test/Suite';
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(
  ({
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('executeCommand log', [
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('workspace/executeCommand', {
        command: 'log:file://foo',
        arguments: ['test'],
      }).verifyAllLSPMessagesInStep(
        ['workspace/executeCommand'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
): Suite);
