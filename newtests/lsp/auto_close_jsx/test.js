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
    lspNotification,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('basic', [
      addFile('basic.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('flow/autoCloseJsx', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/basic.js'},
        position: {line: 2, character: 5},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'flow/autoCloseJsx',
            result: '$0</foo>',
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('nested', [
      addFile('nested.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('flow/autoCloseJsx', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/nested.js'},
        position: {line: 3, character: 7},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'flow/autoCloseJsx',
            result: '$0</bar>',
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('props', [
      addFile('props.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('flow/autoCloseJsx', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/props.js'},
        position: {line: 2, character: 16},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'flow/autoCloseJsx',
            result: '$0</Foo>',
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
  ],
): Suite);
