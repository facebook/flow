/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../../Tester';
const path = require('path');
const {suite, test} = require('../../../Tester');

module.exports = (suite(
  ({
    lspStartAndConnect,
    lspStart,
    lspRequest,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('provide codeAction for basic extract function', [
      addFile(
        'refactor-extract-function-basic.js.ignored',
        'refactor-extract-function-basic.js',
      ),
      lspStartAndConnect(),
      // Partial selection is not allowed and gives no results.
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/refactor-extract-function-basic.js',
        },
        range: {
          start: {
            line: 4,
            character: 2,
          },
          end: {
            line: 5,
            character: 15,
          },
        },
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/codeAction',
            result: [],
          },
        ],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      // Full selection is allowed and gives one result.
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/refactor-extract-function-basic.js',
        },
        range: {
          start: {
            line: 4,
            character: 2,
          },
          end: {
            line: 5,
            character: 21,
          },
        },
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'refactor-extract-basic.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide codeAction for statements with comments', [
      addFile(
        'refactor-extract-with-comments.js.ignored',
        'refactor-extract-with-comments.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/refactor-extract-with-comments.js',
        },
        range: {
          start: {
            line: 3,
            character: 2,
          },
          end: {
            line: 6,
            character: 26,
          },
        },
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'refactor-extract-stmts-with-comments.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide codeAction for extract function with type imports', [
      addFile(
        'refactor-extract-function-type-provider.js.ignored',
        'refactor-extract-function-type-provider.js',
      ),
      addFile(
        'refactor-extract-function-import-type.js.ignored',
        'refactor-extract-function-import-type.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/refactor-extract-function-import-type.js',
        },
        range: {start: {line: 7, character: 2}, end: {line: 7, character: 19}},
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'refactor-extract-function-import-type-1.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/refactor-extract-function-import-type.js',
        },
        range: {start: {line: 6, character: 2}, end: {line: 6, character: 24}},
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'refactor-extract-function-import-type-2.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test(
      'provide codeAction for basic extract method, constant, class fields.',
      [
        addFile(
          'refactor-extract-method.js.ignored',
          'refactor-extract-method.js',
        ),
        lspStartAndConnect(),
        // Partial selection is not allowed and gives no results.
        lspRequestAndWaitUntilResponse('textDocument/codeAction', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/refactor-extract-method.js',
          },
          range: {
            start: {line: 4, character: 4},
            end: {line: 4, character: 16},
          },
          context: {
            only: ['refactor'],
            diagnostics: [],
          },
        }).verifyLSPMessageSnapshot(
          path.join(
            __dirname,
            '__snapshots__',
            'refactor-extract-in-class-1.json',
          ),
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
        lspRequestAndWaitUntilResponse('textDocument/codeAction', {
          textDocument: {
            uri: '<PLACEHOLDER_PROJECT_URL>/refactor-extract-method.js',
          },
          range: {
            start: {line: 4, character: 4},
            end: {line: 4, character: 15},
          },
          context: {
            only: ['refactor'],
            diagnostics: [],
          },
        }).verifyLSPMessageSnapshot(
          path.join(
            __dirname,
            '__snapshots__',
            'refactor-extract-in-class-2.json',
          ),
          [
            'textDocument/publishDiagnostics',
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
      ],
    ),
    test('provide codeAction for basic extract type alias', [
      addFile(
        'refactor-extract-type-alias.js.ignored',
        'refactor-extract-type-alias.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/refactor-extract-type-alias.js',
        },
        range: {
          start: {line: 3, character: 11},
          end: {line: 3, character: 17},
        },
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'refactor-extract-type-alias.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('obey context.only', [
      addFile('only-filter.js.ignored', 'only-filter.js'),
      lspStartAndConnect(),
      // no context.only gets back a quickfix and refactor
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/only-filter.js',
        },
        range: {
          start: {
            line: 3,
            character: 0,
          },
          end: {
            line: 3,
            character: 7,
          },
        },
        context: {
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'refactor-no-context-only.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      // context.only: ["refactor"] only gets the refactor
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/only-filter.js',
        },
        range: {
          start: {
            line: 3,
            character: 0,
          },
          end: {
            line: 3,
            character: 7,
          },
        },
        context: {
          diagnostics: [],
          only: ['refactor'],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'refactor-context-only-refactor.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      // context.only: ["quickfix"] only gets the quickfix
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/only-filter.js',
        },
        range: {
          start: {
            line: 3,
            character: 0,
          },
          end: {
            line: 3,
            character: 7,
          },
        },
        context: {
          diagnostics: [],
          only: ['quickfix'],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'refactor-context-only-quickly.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('provide codeAction for refactoring arrow functions', [
      addFile(
        'refactor-arrow-functions.js.ignored',
        'refactor-arrow-functions.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/refactor-arrow-functions.js',
        },
        range: {
          start: {
            line: 4,
            character: 10,
          },
          end: {
            line: 4,
            character: 10,
          },
        },
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'refactor-add-braces-to-arrow.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/refactor-arrow-functions.js',
        },
        range: {
          start: {
            line: 7,
            character: 5,
          },
          end: {
            line: 7,
            character: 5,
          },
        },
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'refactor-remove-braces-to-arrow.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
): SuiteType);
