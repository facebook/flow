/*
 * @flow
 * @format
 */

import type {Suite} from 'flow-dev-tools/src/test/Suite.js';
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(
  ({addFile, flowCmd, lspStart, lspInitializeParams, waitUntilLSPMessage}) => [
    test('start from IDE, then issue command line request', [
      addFile('debug_sleep.js'),
      // if the test harness were to start the flow server, it would without autostop.
      // set needsFlowServer to false to prevent the test harness from starting the
      // flow server; instead, `flow lsp` will start the server, with autostop
      lspStart({needsFlowServer: false})
        .lspRequestAndWaitUntilResponse('initialize', lspInitializeParams)
        .waitUntilLSPMessage(
          1000,
          'window/logMessage',
          JSON.stringify({
            message: 'Starting Flow server',
          }),
        ),
      flowCmd(['status'], undefined, false).waitUntilLSPMessage(0, ''),
      waitUntilLSPMessage(
        5000,
        'telemetry/connectionStatus',
        JSON.stringify({
          isConnected: true,
        }),
      ).verifyAllLSPMessagesInStep(
        [
          {method: 'telemetry/connectionStatus', params: {isConnected: true}},
          ['window/showStatus', 'Flow is ready.'],
        ],
        [],
      ),
    ]),
  ],
): Suite);
