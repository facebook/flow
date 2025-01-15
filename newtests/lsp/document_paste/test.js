/**
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {readFileSync, readdirSync} = require('fs');
const {join} = require('path');
const {suite, test} = require('../../Tester');

module.exports = (suite(
  ({
    lspNotification,
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    addFiles,
    addFile,
  }) => {
    function snapshotForPrepare(expectedFile: string, range: mixed) {
      return lspRequestAndWaitUntilResponse('flow/prepareDocumentPaste', {
        uri: `<PLACEHOLDER_PROJECT_URL>/__fixtures__/source.js`,
        ranges: [range],
      }).verifyLSPMessageSnapshot(
        join(__dirname, '__snapshots__', expectedFile),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      );
    }
    function snapshotForProvide(
      path: string,
      text: string,
      expectedFile: string,
      dataTransfer: mixed,
    ) {
      return lspRequestAndWaitUntilResponse('flow/provideDocumentPasteEdits', {
        textDocument: {
          uri: `<PLACEHOLDER_PROJECT_URL>/__fixtures__/${path}`,
          text,
        },
        // Ranges are ignored for now.
        ranges: [],
        dataTransfer,
      }).verifyLSPMessageSnapshot(
        join(__dirname, '__snapshots__', expectedFile),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      );
    }
    const fixturesPath = join(__dirname, '__fixtures__');
    const fixtures = readdirSync(fixturesPath).map(file =>
      join('__fixtures__', file),
    );
    function readDataTransferFromSnapshot(file: string) {
      return JSON.parse(
        readFileSync(join(__dirname, '__snapshots__', file)).toString(),
      ).result;
    }
    return [
      test('Prepare tests', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        // Sanity check: If user selects nothing, then metadata will be empty
        snapshotForPrepare('prepare_on_empty_range.json', {
          start: {line: 0, character: 0},
          end: {line: 0, character: 0},
        }),
        // Sanity check: If user selects everything, then no imported name will
        // be unbounded on paste, so metadata will be empty.
        snapshotForPrepare('prepare_on_entire_file.json', {
          start: {line: 0, character: 0},
          end: {line: 1000, character: 0},
        }),
        // When user selects all the non-imports, then we should get metadata of all
        // imported names.
        snapshotForPrepare('prepare_on_all_non_imports.json', {
          start: {line: 12, character: 0},
          end: {line: 1000, character: 0},
        }),
        snapshotForPrepare('prepare_on_all_toplevel_non_imports.json', {
          start: {line: 12, character: 0},
          end: {line: 21, character: 0},
        }),
        snapshotForPrepare('prepare_on_nested.json', {
          start: {line: 21, character: 0},
          end: {line: 1000, character: 0},
        }),
      ]),
      test('Provide tests', [
        addFiles(...fixtures),
        lspStartAndConnect(),
        snapshotForProvide(
          'test.js',
          '',
          'provide_for_empty_data_transfer.json',
          {imports: []},
        ),
        snapshotForProvide(
          'test.js',
          '// @flow\nconst _ = 3',
          'provide_for_full_data_transfer.json',
          readDataTransferFromSnapshot('prepare_on_all_non_imports.json'),
        ),
        snapshotForProvide(
          'bar/test.js',
          '// @flow\nconst _ = 3',
          'provide_for_full_data_transfer_with_deeper_path.json',
          readDataTransferFromSnapshot('prepare_on_all_non_imports.json'),
        ),
        snapshotForProvide(
          'test.js',
          '// @flow\nconst foo = 3;',
          'provide_for_full_data_transfer_with_existing_binding.json',
          readDataTransferFromSnapshot('prepare_on_all_non_imports.json'),
        ),
      ]),
    ];
  },
): SuiteType);
