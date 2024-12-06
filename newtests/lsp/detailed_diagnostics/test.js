/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {readFileSync} = require('fs');
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
          join(__dirname, '__snapshots__', 'no-detailed-errors.json'),
          ['window/showStatus', '$/cancelRequest'],
        ),
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/file_with_simple_error.js',
          languageId: 'javascript',
          version: 1,
          text: readFileSync(
            join(__dirname, 'file_with_simple_error.js'),
          ).toString(),
        },
      })
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
