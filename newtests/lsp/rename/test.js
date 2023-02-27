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
    function snapshot(fixture, line, col, expectedFile) {
      return lspRequestAndWaitUntilResponse('textDocument/rename', {
        textDocument: {
          uri: `<PLACEHOLDER_PROJECT_URL>/__fixtures__/${fixture}`,
        },
        position: {line: line, character: col},
        newName: 'NEW_NAME',
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
      test('Variable defs and uses', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 3, 5, 'var_defs_1.json'),
        snapshot('locals.js', 4, 2, 'var_defs_2.json'),
      ]),
      test('Type aliases', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 9, 6, 'type_aliases_1.json'),
      ]),
      test('Destructuring', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 14, 7, 'destructuring_1.json'),
        snapshot('locals.js', 15, 10, 'destructuring_2.json'),
        snapshot('locals.js', 15, 26, 'destructuring_3.json'),
        snapshot('locals.js', 16, 7, 'destructuring_4.json'),
      ]),
      test('Imports', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 21, 10, 'imports_1.json'),
        snapshot('locals.js', 22, 16, 'imports_2.json'),
        snapshot('locals.js', 23, 2, 'imports_3.json'),
        snapshot('locals.js', 26, 8, 'imports_4.json'),
        snapshot('locals.js', 27, 8, 'imports_4.json'),
      ]),
    ];
  },
): Suite);
