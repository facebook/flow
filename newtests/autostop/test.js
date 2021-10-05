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
      // this next step passes because of the buggy behavior.
      // this test should be updated once the behavior is fixed
      waitUntilLSPMessage(
        5000,
        'window/showStatus',
        JSON.stringify({
          message: 'Flow: server is stopped',
        }),
      ),
    ]),
  ],
): Suite);
