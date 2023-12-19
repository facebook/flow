/**
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {readFileSync, readdirSync} = require('fs');
const {join} = require('path');
const {suite, test} = require('../../Tester');

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
    function findAllRefsSnapshot(
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
        join(__dirname, '__snapshots__', 'references', expectedFile),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      );
    }
    function globalRenameSnapshot(
      fixture: string,
      line: number,
      col: number,
      expectedFile: string,
      newName: string = 'NEW_NAME',
    ) {
      return lspRequestAndWaitUntilResponse('textDocument/rename', {
        textDocument: {
          uri: `<PLACEHOLDER_PROJECT_URL>/__fixtures__/${fixture}`,
        },
        position: {line: line, character: col},
        newName,
      }).verifyLSPMessageSnapshot(
        join(__dirname, '__snapshots__', 'rename', expectedFile),
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
      test('Find all refs from properties', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        findAllRefsSnapshot('use-prop-site-a.js', 5, 6, 'prop_defs_1.json'),
        findAllRefsSnapshot('use-prop-site-b.js', 5, 6, 'prop_defs_2.json'),
      ]),
      test('Find all refs and rename with unsaved changes', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        // Open a document with errors. We should get a live syntax error immediately.
        lspNotification('textDocument/didOpen', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/__fixtures__/use-prop-site-a.js',
            languageId: 'javascript',
            version: 1,
            text: readFileSync(
              join(__dirname, '__fixtures__', 'use-prop-site-a.js'),
            ).toString(),
          },
        }).waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics'),
        // Make a change that introduces the error. We should get a report immediately.
        lspNotification('textDocument/didChange', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/__fixtures__/use-prop-site-a.js',
            version: 2,
          },
          contentChanges: [
            {
              text: `\n${readFileSync(
                join(__dirname, '__fixtures__', 'use-prop-site-a.js'),
              ).toString()}`,
            },
          ],
        }).waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics'),
        findAllRefsSnapshot(
          'use-prop-site-a.js',
          6,
          6,
          'prop_defs_with_unsaved_changes.json',
        ),
        globalRenameSnapshot(
          'use-prop-site-a.js',
          6,
          6,
          'prop_defs_with_unsaved_changes.json',
        ),
      ]),
      test('Find all refs from identifiers', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        findAllRefsSnapshot(
          'identifiers-def.js',
          2,
          14,
          'identifiers-def-1.json',
        ),
        findAllRefsSnapshot(
          'identifiers-def.js',
          3,
          17,
          'identifiers-def-2.json',
        ),
        findAllRefsSnapshot(
          'use-exported-identifiers-site-b.js',
          8,
          12,
          'identifiers-def-3.json',
        ),
        findAllRefsSnapshot('private-name.js', 7, 14, 'private-name-1.json'),
        findAllRefsSnapshot('private-name.js', 8, 14, 'private-name-2.json'),
        findAllRefsSnapshot('private-name.js', 20, 17, 'private-name-3.json'),
        findAllRefsSnapshot('private-name.js', 23, 12, 'private-name-4.json'),
      ]),
      test('Global rename property 1', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        globalRenameSnapshot('use-prop-site-a.js', 5, 6, 'prop_defs_1.json'),
      ]),
      test('Global rename property 2', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        globalRenameSnapshot('use-prop-site-b.js', 5, 6, 'prop_defs_2.json'),
      ]),
      test('Global rename identifier 1', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        globalRenameSnapshot(
          'identifiers-def.js',
          2,
          14,
          'identifiers-def-1.json',
        ),
      ]),
      test('Global rename identifier 2', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        globalRenameSnapshot(
          'identifiers-def.js',
          3,
          17,
          'identifiers-def-2.json',
        ),
      ]),
      test('Global rename private name', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        globalRenameSnapshot(
          'private-name.js',
          7,
          14,
          'private-name-rename-to-non-private-1.json',
        ),
        globalRenameSnapshot(
          'private-name.js',
          8,
          14,
          'private-name-rename-to-non-private-2.json',
        ),
        globalRenameSnapshot(
          'private-name.js',
          20,
          17,
          'private-name-rename-to-non-private-3.json',
        ),
        globalRenameSnapshot(
          'private-name.js',
          23,
          12,
          'private-name-rename-to-non-private-4.json',
        ),
        globalRenameSnapshot(
          'private-name.js',
          7,
          14,
          'private-name-rename-to-private-1.json',
          '#NEW_NAME',
        ),
        globalRenameSnapshot(
          'private-name.js',
          8,
          14,
          'private-name-rename-to-private-2.json',
          '#NEW_NAME',
        ),
        globalRenameSnapshot(
          'private-name.js',
          20,
          17,
          'private-name-rename-to-private-3.json',
          '#NEW_NAME',
        ),
        globalRenameSnapshot(
          'private-name.js',
          23,
          12,
          'private-name-rename-to-private-4.json',
          '#NEW_NAME',
        ),
      ]),
    ];
  },
): SuiteType);
