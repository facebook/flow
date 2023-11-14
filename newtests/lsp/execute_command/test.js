/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

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
): SuiteType);
