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
    const fixtures = [
      'package.json',
      'inside.js',
      'main.js',
      'foo.js',
      'bar.js.flow',
      join('subdir', 'index.js'),
      join('subdir', 'baz', '42.js'),
    ].map(file => join('__fixtures__', 'haste_pkg', file));
    return [
      test('auto import aaa', [
        addFiles(...fixtures),
        addCode(`aaa`),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
          position: {line: 2, character: 3},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'a.json'),
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ]),
      test('auto import bbb', [
        addFiles(...fixtures),
        addCode(`bbb`),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
          position: {line: 2, character: 3},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'b.json'),
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ]),
      test('auto import ccc', [
        addFiles(...fixtures),
        addCode(`ccc`),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
          position: {line: 2, character: 3},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'c.json'),
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ]),
      test('auto import ddd', [
        addFiles(...fixtures),
        addCode(`ddd`),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
          position: {line: 2, character: 3},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'd.json'),
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ]),
      test('auto import eee', [
        addFiles(...fixtures),
        addCode(`eee`),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
          position: {line: 2, character: 3},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'e.json'),
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ]),
      test('auto import inside haste package', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/__fixtures__/haste_pkg/inside.js',
          },
          position: {line: 0, character: 3},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          join(__dirname, '__snapshots__', 'inside.json'),
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
