/*
 * @flow
 * @format
 */

import type {Suite} from 'flow-dev-tools/src/test/Suite';
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(
  ({
    lspStartAndConnect,
    lspStart,
    lspRequest,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    addFile,
    modifyFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('LSP process reconnects after being kicked off', [
      addFile('lib.js', 'libs/lib.js'),
      addFile('foo.js'),
      lspStartAndConnect(),
      // modify lib file to make server restart
      modifyFile(
        'libs/lib.js',
        'bar',
        'baz',
      ).waitAndVerifyNoLSPMessagesSinceStartOfStep(0),
      /* wait to reconnect to server.
       * NOTE: D7616745 is broken and this is a repro for it.
       * Look at the test.log output; there is a "Server fatal exception []" telemetry event.
       * The [] is supposed to be [13], but the server kills the client too quickly for us to
       * get the memo.
       * This doesn't cause an actual problem, it's just strange. */
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/foo.js'},
        position: {line: 3, character: 1},
      }).waitUntilLSPMessage(
        5000,
        'telemetry/connectionStatus',
        JSON.stringify({isConnected: true}),
      ),
      // verify that there's a new server that answers requests
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/foo.js'},
        position: {line: 3, character: 1},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/hover',
            result: {
              contents: [{language: 'flow', value: 'number'}],
              range: {
                start: {line: 3, character: 0},
                end: {line: 3, character: 3},
              },
            },
          },
        ],
        ['telemetry/connectionStatus', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
): Suite);
