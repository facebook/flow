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
    test('textDocument/diagnostics client enabled', [
      addFile('file_with_simple_error.js'),
      lspStartAndConnect(null, {
        ...lspInitializeParams,
        initializationOptions: {detailedErrorRendering: true},
      }),
      lspRequestAndWaitUntilResponse('textDocument/diagnostics', {
        textDocument: {
          uri: `<PLACEHOLDER_PROJECT_URL>/file_with_simple_error.js`,
        },
      }).verifyLSPMessageSnapshot(
        join(__dirname, '__snapshots__', 'no-detailed-errors.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
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
      }).verifyAllLSPMessagesInStep(
        [],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
      lspRequestAndWaitUntilResponse('textDocument/diagnostics', {
        textDocument: {
          uri: `<PLACEHOLDER_PROJECT_URL>/file_with_simple_error.js`,
        },
      }).verifyLSPMessageSnapshot(
        join(__dirname, '__snapshots__', 'has-detailed-errors.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('textDocument/diagnostics client disabled', [
      addFile('file_with_simple_error.js'),
      lspStartAndConnect(null, {
        ...lspInitializeParams,
        initializationOptions: {detailedErrorRendering: false},
      }),
      lspRequestAndWaitUntilResponse('textDocument/diagnostics', {
        textDocument: {
          uri: `<PLACEHOLDER_PROJECT_URL>/file_with_simple_error.js`,
        },
      }).verifyLSPMessageSnapshot(
        join(__dirname, '__snapshots__', 'no-detailed-errors.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('textDocument/diagnostics no client config', [
      addFile('file_with_simple_error.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/diagnostics', {
        textDocument: {
          uri: `<PLACEHOLDER_PROJECT_URL>/file_with_simple_error.js`,
        },
      }).verifyLSPMessageSnapshot(
        join(__dirname, '__snapshots__', 'no-detailed-errors.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
  ],
): SuiteType);
