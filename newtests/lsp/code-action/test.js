/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const path = require('path');
const {suite, test} = require('../../Tester');

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
    test('initialize with code actions support', [
      lspStart({needsFlowServer: false}),
      lspRequestAndWaitUntilResponse(
        'initialize',
        lspInitializeParams,
      ).verifyAllLSPMessagesInStep(
        [
          [
            'initialize',
            '{"codeActionProvider":{"codeActionKinds":["source.addMissingImports.flow","source.organizeImports.flow","refactor.extract","quickfix"]}}',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('initialize without quickfix support', [
      lspStart({needsFlowServer: false}),
      lspRequestAndWaitUntilResponse('initialize', {
        ...lspInitializeParams,
        capabilities: {
          ...lspInitializeParams.capabilities,
          textDocument: {
            ...lspInitializeParams.capabilities.textDocument,
            codeAction: {
              codeActionLiteralSupport: {
                codeActionKind: {
                  valueSet: ['refactor.extract'],
                },
              },
            },
          },
        },
      }).verifyAllLSPMessagesInStep(
        [
          [
            'initialize',
            '{"codeActionProvider":{"codeActionKinds":["refactor.extract"]}}',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('initialize without refactor.extract support', [
      lspStart({needsFlowServer: false}),
      lspRequestAndWaitUntilResponse('initialize', {
        ...lspInitializeParams,
        capabilities: {
          ...lspInitializeParams.capabilities,
          textDocument: {
            ...lspInitializeParams.capabilities.textDocument,
            codeAction: {
              codeActionLiteralSupport: {
                codeActionKind: {
                  valueSet: ['quickfix'],
                },
              },
            },
          },
        },
      }).verifyAllLSPMessagesInStep(
        [
          [
            'initialize',
            '{"codeActionProvider":{"codeActionKinds":["quickfix"]}}',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('initialize without any code actions support', [
      lspStart({needsFlowServer: false}),
      lspRequestAndWaitUntilResponse('initialize', {
        ...lspInitializeParams,
        capabilities: {
          ...lspInitializeParams.capabilities,
          textDocument: {
            ...lspInitializeParams.capabilities.textDocument,
            codeAction: {},
          },
        },
      }).verifyAllLSPMessagesInStep(
        [['initialize', '{"codeActionProvider":false}']],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('skip non-@flow files', [
      addFile('not_flow.js.ignored', 'not_flow.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/not_flow.js',
        },
        range: {
          start: {
            line: 1,
            character: 2,
          },
          end: {
            line: 1,
            character: 6,
          },
        },
        context: {
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
    ]),
    test('provide codeAction for inserting jsdocs', [
      addFile('insert-jsdoc.js.ignored', 'insert-jsdoc.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/insert-jsdoc.js',
        },
        range: {
          start: {
            line: 3,
            character: 10,
          },
          end: {
            line: 3,
            character: 10,
          },
        },
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'insert-docs-1.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/insert-jsdoc.js',
        },
        range: {
          start: {
            line: 8,
            character: 18,
          },
          end: {
            line: 8,
            character: 18,
          },
        },
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'insert-docs-2.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/codeAction', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/insert-jsdoc.js',
        },
        range: {
          start: {
            line: 11,
            character: 18,
          },
          end: {
            line: 11,
            character: 18,
          },
        },
        context: {
          only: ['refactor'],
          diagnostics: [],
        },
      }).verifyLSPMessageSnapshot(
        path.join(__dirname, '__snapshots__', 'insert-docs-3.json'),
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
): SuiteType);
