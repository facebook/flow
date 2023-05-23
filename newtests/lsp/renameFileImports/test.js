/**
 * @flow
 * @format
 */

import type {Suite} from 'flow-dev-tools/src/test/Suite';
const {readFileSync, readdirSync} = require('fs');
const {join} = require('path');
const {suite, test} = require('flow-dev-tools/src/test/Tester');

/**
 * Most of the logic for rename is shared by DocumentHighlight. The tests there are more comprehensive
 * and ensure that all references are being found.
 *
 * These tests sanity check that a local rename works and test that destructured/shorthand
 * renames are handled correctly.
 */
module.exports = (suite(
  ({
    lspNotification,
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    addFiles,
  }) => {
    function snapshot(oldName: string, newName: string, expectedFile: string) {
      return lspRequestAndWaitUntilResponse('flow/renameFileImports', {
        oldUri: `<PLACEHOLDER_PROJECT_URL>/__fixtures__/${oldName}`,
        newUri: `<PLACEHOLDER_PROJECT_URL>/__fixtures__/${newName}`,
      }).verifyLSPMessageSnapshot(
        join(__dirname, '__snapshots__', expectedFile),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      );
    }
    const fixtures = readdirSync(join(__dirname, '__fixtures__')).map(file =>
      join('__fixtures__', file),
    );
    return [
      test('Cross file imports', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot(
          'dependencyFile.js',
          'NEW_NAME.js',
          'cross_file_imports_1.json',
        ),
      ]),
    ];
  },
): Suite);
