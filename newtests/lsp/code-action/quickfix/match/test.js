/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../../../Tester';
const path = require('path');
const {suite, test} = require('../../../../Tester');
const {generateSimpleTests} = require('../../test-utils');

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
    test('match invalid object shorthand quickfix', [
      addFile(
        'fix-match-invalid-object-shorthand.js.ignored',
        'fix-match-invalid-object-shorthand.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-invalid-object-shorthand.js',
        },
        range: {
          start: {line: 6, character: 5},
          end: {line: 6, character: 5},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-invalid-object-shorthand.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match statement invalid body quickfix', [
      addFile(
        'fix-match-statement-invalid-body.js.ignored',
        'fix-match-statement-invalid-body.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-statement-invalid-body.js',
        },
        range: {
          start: {line: 4, character: 10},
          end: {line: 4, character: 10},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-statement-invalid-body.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match invalid binding kind', [
      addFile(
        'fix-match-invalid-binding-kind.js.ignored',
        'fix-match-invalid-binding-kind.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-invalid-binding-kind.js',
        },
        range: {
          start: {line: 3, character: 2},
          end: {line: 3, character: 7},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-invalid-binding-kind.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match invalid wildcard syntax', [
      addFile(
        'fix-match-invalid-wildcard-syntax.js.ignored',
        'fix-match-invalid-wildcard-syntax.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-invalid-wildcard-syntax.js',
        },
        range: {
          start: {line: 3, character: 3},
          end: {line: 3, character: 9},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-invalid-wildcard-syntax.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match statement invalid case syntax - multiple', [
      addFile(
        'fix-match-statement-invalid-case-syntax-multiple.js.ignored',
        'fix-match-statement-invalid-case-syntax-multiple.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-statement-invalid-case-syntax-multiple.js',
        },
        range: {
          start: {line: 4, character: 1},
          end: {line: 4, character: 5},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-statement-invalid-case-syntax-multiple.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match expression invalid case syntax - multiple', [
      addFile(
        'fix-match-expression-invalid-case-syntax-multiple.js.ignored',
        'fix-match-expression-invalid-case-syntax-multiple.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-expression-invalid-case-syntax-multiple.js',
        },
        range: {
          start: {line: 4, character: 11},
          end: {line: 4, character: 15},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-expression-invalid-case-syntax-multiple.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match statement invalid case syntax - single', [
      addFile(
        'fix-match-statement-invalid-case-syntax-single.js.ignored',
        'fix-match-statement-invalid-case-syntax-single.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-statement-invalid-case-syntax-single.js',
        },
        range: {
          start: {line: 3, character: 3},
          end: {line: 3, character: 3},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-statement-invalid-case-syntax-single.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match non exhaustive object pattern - only rest', [
      addFile(
        'fix-match-non-exhaustive-object-pattern-only-rest.js.ignored',
        'fix-match-non-exhaustive-object-pattern-only-rest.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-non-exhaustive-object-pattern-only-rest.js',
        },
        range: {
          start: {line: 5, character: 3},
          end: {line: 5, character: 4},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-non-exhaustive-object-pattern-only-rest.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match non exhaustive object pattern - rest and only props', [
      addFile(
        'fix-match-non-exhaustive-object-pattern-rest-and-only-props.js.ignored',
        'fix-match-non-exhaustive-object-pattern-rest-and-only-props.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-non-exhaustive-object-pattern-rest-and-only-props.js',
        },
        range: {
          start: {line: 5, character: 3},
          end: {line: 5, character: 4},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-non-exhaustive-object-pattern-rest-and-only-props.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match non exhaustive object pattern - rest and both', [
      addFile(
        'fix-match-non-exhaustive-object-pattern-rest-and-both.js.ignored',
        'fix-match-non-exhaustive-object-pattern-rest-and-both.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-non-exhaustive-object-pattern-rest-and-both.js',
        },
        range: {
          start: {line: 5, character: 3},
          end: {line: 5, character: 4},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-non-exhaustive-object-pattern-rest-and-both.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match non exhaustive object pattern - non-ident key', [
      addFile(
        'fix-match-non-exhaustive-object-pattern-non-ident-key.js.ignored',
        'fix-match-non-exhaustive-object-pattern-non-ident-key.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-non-exhaustive-object-pattern-non-ident-key.js',
        },
        range: {
          start: {line: 5, character: 3},
          end: {line: 5, character: 4},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-non-exhaustive-object-pattern-non-ident-key.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match not exhaustive - expression - basic', [
      addFile(
        'fix-match-not-exhaustive-expression-basic.js.ignored',
        'fix-match-not-exhaustive-expression-basic.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-not-exhaustive-expression-basic.js',
        },
        range: {
          start: {line: 8, character: 11},
          end: {line: 8, character: 15},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-not-exhaustive-expression-basic.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match not exhaustive - statement - basic', [
      addFile(
        'fix-match-not-exhaustive-statement-basic.js.ignored',
        'fix-match-not-exhaustive-statement-basic.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-not-exhaustive-statement-basic.js',
        },
        range: {
          start: {line: 6, character: 1},
          end: {line: 6, character: 5},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-not-exhaustive-statement-basic.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match not exhaustive - statement - single', [
      addFile(
        'fix-match-not-exhaustive-statement-single.js.ignored',
        'fix-match-not-exhaustive-statement-single.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-not-exhaustive-statement-single.js',
        },
        range: {
          start: {line: 4, character: 1},
          end: {line: 4, character: 5},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-not-exhaustive-statement-single.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match not exhaustive - statement - complex', [
      addFile(
        'fix-match-not-exhaustive-statement-complex.js.ignored',
        'fix-match-not-exhaustive-statement-complex.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-not-exhaustive-statement-complex.js',
        },
        range: {
          start: {line: 4, character: 1},
          end: {line: 4, character: 5},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-not-exhaustive-statement-complex.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match not exhaustive - statement - disjoint', [
      addFile(
        'fix-match-not-exhaustive-statement-disjoint.js.ignored',
        'fix-match-not-exhaustive-statement-disjoint.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-not-exhaustive-statement-disjoint.js',
        },
        range: {
          start: {line: 4, character: 1},
          end: {line: 4, character: 5},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-not-exhaustive-statement-disjoint.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match not exhaustive - statement - many', [
      addFile(
        'fix-match-not-exhaustive-statement-many.js.ignored',
        'fix-match-not-exhaustive-statement-many.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-not-exhaustive-statement-many.js',
        },
        range: {
          start: {line: 4, character: 1},
          end: {line: 4, character: 5},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-not-exhaustive-statement-many.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match unused pattern - remove case', [
      addFile(
        'fix-match-unused-pattern-remove-case.js.ignored',
        'fix-match-unused-pattern-remove-case.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-unused-pattern-remove-case.js',
        },
        range: {
          start: {line: 7, character: 3},
          end: {line: 7, character: 7},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-unused-pattern-remove-case.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match unused pattern - remove from "or" pattern', [
      addFile(
        'fix-match-unused-pattern-or.js.ignored',
        'fix-match-unused-pattern-or.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-unused-pattern-or.js',
        },
        range: {
          start: {line: 6, character: 7},
          end: {line: 6, character: 11},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-unused-pattern-or.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match unused pattern - remove object rest', [
      addFile(
        'fix-match-unused-pattern-object-rest.js.ignored',
        'fix-match-unused-pattern-object-rest.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-unused-pattern-object-rest.js',
        },
        range: {
          start: {line: 5, character: 12},
          end: {line: 5, character: 14},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-unused-pattern-object-rest.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match unused pattern - remove array rest', [
      addFile(
        'fix-match-unused-pattern-array-rest.js.ignored',
        'fix-match-unused-pattern-array-rest.js',
      ),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/fix-match-unused-pattern-array-rest.js',
        },
        range: {
          start: {line: 5, character: 7},
          end: {line: 5, character: 9},
        },
        context: {
          only: ['quickfix'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(
          __dirname,
          '__snapshots__',
          'fix-match-unused-pattern-array-rest.json',
        ),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('match coded like switch', [
      ...generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'fix-match-coded-like-switch.js',
        'fix-match-coded-like-switch',
      ),
    ]),
  ],
): SuiteType);
