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
    addCode,
    addFiles,
  }) => {
    return [
      test('auto import value', [
        addFiles('__fixtures__/FooBarBaz1.js', '__fixtures__/FooBarBaz2.js'),
        addCode(`FooBarBaz`),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
          position: {line: 2, character: 9},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'value.json'),
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ]),
      test('auto import type', [
        addFiles('__fixtures__/FooBarBaz1.js', '__fixtures__/FooBarBaz2.js'),
        addCode(`type F = FooBarBaz`),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
          position: {line: 2, character: 18},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'type.json'),
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ]),
    ];
  },
): Suite);
