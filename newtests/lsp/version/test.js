/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

module.exports = (suite(
  ({
    lspStart,
    lspRequest,
    lspNotification,
    lspInitializeParams,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('initialize error for wrong version', [
      lspStart({needsFlowServer: false}),
      lspRequest('initialize', lspInitializeParams)
        .waitUntilLSPMessage(30000, 'initialize')
        .verifyAllLSPMessagesInStep(
          [['initialize', '{Wrong version of Flow. The config specifies}']],
          [...lspIgnoreStatusAndCancellation],
        ),
      lspRequest('shutdown')
        .waitUntilLSPMessage(10000, 'shutdown')
        .verifyAllLSPMessagesInStep(
          ['shutdown'],
          [...lspIgnoreStatusAndCancellation],
        ),
      lspNotification('exit')
        .waitUntilLSPStatus(10000, 'stopped')
        .waitUntilServerStatus(10000, 'stopped')
        .verifyLSPStatus('stopped')
        .verifyServerStatus('stopped'),
    ]),
  ],
): SuiteType);
