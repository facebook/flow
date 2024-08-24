/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {join} = require('path');
const {suite, test} = require('../../Tester');

module.exports = (suite(
  ({
    lspStartAndConnect,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    lspNotification,
    addFile,
    addFiles,
    modifyFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('textDocument/publishDiagnostics client enabled', [
      lspStartAndConnect(null, {
        ...lspInitializeParams,
        initializationOptions: {detailedErrorRendering: true},
      }),
      addFile('file_with_simple_error.js')
        .waitUntilLSPMessage(
          9000,
          'textDocument/publishDiagnostics',
          '{Cannot assign}',
        )
        .verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'has-detailed-errors.json'),
          ['window/showStatus', '$/cancelRequest'],
        ),
    ]),

    test('textDocument/publishDiagnostics client disabled', [
      lspStartAndConnect(null, {
        ...lspInitializeParams,
        initializationOptions: {detailedErrorRendering: false},
      }),
      addFile('file_with_simple_error.js')
        .waitUntilLSPMessage(
          9000,
          'textDocument/publishDiagnostics',
          '{Cannot assign}',
        )
        .verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'no-detailed-errors.json'),
          ['window/showStatus', '$/cancelRequest'],
        ),
    ]),

    test('textDocument/publishDiagnostics no client config', [
      lspStartAndConnect(),
      addFile('file_with_simple_error.js')
        .waitUntilLSPMessage(
          9000,
          'textDocument/publishDiagnostics',
          '{Cannot assign}',
        )
        .verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'no-detailed-errors.json'),
          ['window/showStatus', '$/cancelRequest'],
        ),
    ]),
  ],
): SuiteType);
