/**
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');
const path = require('path');

module.exports = (suite(
  ({
    lspStartAndConnect,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    addFiles,
  }) => [
    test('textDocument/workspaceSymbol with hierarchical support', [
      addFiles(
        path.join('__fixtures__', 'a.js'),
        path.join('__fixtures__', 'b.js'),
      ),
      lspStartAndConnect(6000, lspInitializeParams),
      lspRequestAndWaitUntilResponse('workspace/symbol', {
        query: 'Foo',
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'workspace/symbol',
            result: [
              {
                name: 'FooClass',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/__fixtures__/b.js',
                },
              },
              {
                name: 'FooType',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/__fixtures__/b.js',
                },
              },
              {
                name: 'FooVar',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/__fixtures__/b.js',
                },
              },
              {
                name: 'IFoo',
                kind: 13,
                location: {
                  uri: '<PLACEHOLDER_PROJECT_URL>/__fixtures__/a.js',
                },
              },
            ],
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
  ],
): SuiteType);
