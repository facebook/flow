/**
 * @flow
 * @format
 */

import type {Suite} from 'flow-dev-tools/src/test/Suite';
const {readFileSync, readdirSync} = require('fs');
const {join} = require('path');
const {suite, test} = require('flow-dev-tools/src/test/Tester');

/**
 * The code in findReferences shares the same logic as documentHighlight. That test suite is
 * much more comprehensive and covers the more complex behaviors of findReferences. Rather than
 * duplicate the test suites, this test just sanity checks that findReferences is working end-to-end
 */
module.exports = (suite(
  ({
    lspNotification,
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    addFiles,
  }) => {
    function snapshot(
      fixture: string,
      line: number,
      col: number,
      expectedFile: string,
    ) {
      return lspRequestAndWaitUntilResponse('textDocument/references', {
        textDocument: {
          uri: `<PLACEHOLDER_PROJECT_URL>/__fixtures__/${fixture}`,
        },
        position: {line: line, character: col},
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
        snapshot('locals.js', 23, 1, 'var_defs_3.json'),
        snapshot('locals.js', 25, 3, 'var_defs_4.json'),
      ]),
      test('Nested functions', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 9, 10, 'nested_funcs_1.json'),
        snapshot('locals.js', 12, 3, 'nested_funcs_2.json'),
      ]),
      test('Classes', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 17, 7, 'classes_1.json'),
      ]),
      test('JSX Props', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshot('locals.js', 29, 16, 'jsx_props.json'),
      ]),
    ];
  },
): Suite);
