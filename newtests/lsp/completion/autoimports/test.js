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
    addFiles,
    addCode,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('textDocument/completion with autoimports', [
      addFiles('foo.js', 'bar.js', 'foobar.js', 'lib/builtins.js'),
      addCode(`f`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 1},
        context: {triggerKind: 1},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'completion_with_auto_imports.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('textDocument/completion with ranked autoimports', [
      addFiles('foo.js', 'bar.js', 'foobar.js', 'lib/builtins.js'),
      addCode(`f`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 1},
        context: {triggerKind: 1},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'completion_with_ranked_auto_imports.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]).flowConfig('_flowconfig_ranked'),

    test(
      'textDocument/completion with min number of characters to get auto imports',
      [
        addFiles('foo.js', 'bar.js', 'foobar.js', 'lib/builtins.js'),
        addCode(`f`),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
          position: {line: 2, character: 1},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          path.join(
            __dirname,
            '__snapshots__',
            'completion_with_require_min_chars_one_char.json',
          ),
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ],
    ).flowConfig('_flowconfig_require_min_chars'),

    test(
      'textDocument/completion with min number of characters to get auto imports',
      [
        addFiles('foo.js', 'bar.js', 'foobar.js', 'lib/builtins.js'),
        addCode(`foo`),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
          position: {line: 2, character: 3},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          path.join(
            __dirname,
            '__snapshots__',
            'completion_with_require_min_chars_three_char.json',
          ),
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ],
    ).flowConfig('_flowconfig_require_min_chars'),

    test(
      'textDocument/completion with ranked autoimports boosting exact match imports',
      [
        addFiles('actor.js'),
        addCode(`const actoooooooor = 3;\nconst actors = 3;\nactor`),
        lspStartAndConnect(),
        lspRequestAndWaitUntilResponse('textDocument/completion', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
          position: {line: 4, character: 5},
          context: {triggerKind: 1},
        }).verifyLSPMessageSnapshot(
          path.join(
            __dirname,
            '__snapshots__',
            'completion_with_ranked_auto_imports_boost_exact_match_imports.json',
          ),
          [
            'textDocument/publishDiagnostics',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      ],
    ).flowConfig('_flowconfig_ranked'),

    test('textDocument/completion with JSX autoimports', [
      addCode(`function Foo(props: {...}): null {}`),
      addCode(`(<F`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 4, character: 3},
        context: {triggerKind: 1},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'completion_with_jsx_auto_imports.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('textDocument/completion on types', [
      addFiles('foo.js', 'types.js', 'lib/builtins.js'),
      addCode(`type Test = T`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 13},
        context: {triggerKind: 1},
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'completion_on_types.json'),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('textDocument/completion on types with ranked autoimports', [
      addFiles('foo.js', 'types.js', 'lib/builtins.js'),
      addCode(`type Test = T`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 13},
        context: {triggerKind: 1},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'completion_with_types_with_ranked_auto_imports.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]).flowConfig('_flowconfig_ranked'),

    test('textDocument/completion should exclude reserved words', [
      addFiles('reserved.js'),
      addCode(`null`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 3},
        context: {triggerKind: 1},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'completion_exclude_reserved_words.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    // should not suggest importing `foobar` from foobar.js
    test('textDocument/completion should exclude variables already in scope', [
      addFiles('foobar.js'),
      addCode(`const foobar = ''; foobar`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 25},
        context: {triggerKind: 1},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'completion_exclude_already_in_scope.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('should sort properly', [
      addFiles('AllTheThings.js'),
      addCode(`All`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 3},
        context: {triggerKind: 1},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'completion_should_sort_properly.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('should handle scoped module names', [
      addFiles('lib/scoped.js'),
      addCode(`xy`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 2, character: 2},
        context: {triggerKind: 1},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'completion_handle_scoped_module_names.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),

    test('should not auto import values in object keys in type annotations', [
      addFiles('functions.js'),
      addCode(`type A = {\nfuncA\n}`),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/test.js'},
        position: {line: 3, character: 5},
        context: {triggerKind: 1},
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'completion_no_auto_import_in_object_key_in_annot.json',
        ),
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
  ],
): SuiteType);
