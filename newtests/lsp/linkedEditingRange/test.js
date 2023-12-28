/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

module.exports = (suite(
  ({
    lspStartAndConnect,
    lspStart,
    lspRequest,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    addFile,
    lspNotification,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('basic opening', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/linkedEditingRange', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 5, character: 5}, // opening Foo
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/linkedEditingRange',
            result: {
              ranges: [
                {
                  start: {
                    line: 5,
                    character: 3,
                  },
                  end: {
                    line: 5,
                    character: 6,
                  },
                },
                {
                  start: {
                    line: 7,
                    character: 4,
                  },
                  end: {
                    line: 7,
                    character: 7,
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('basic closing', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/linkedEditingRange', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 7, character: 7}, // closing Foo
      }).verifyAllLSPMessagesInStep(
        [{method: 'textDocument/linkedEditingRange', result: null}],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('opening outer', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/linkedEditingRange', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 11, character: 5}, // opening div
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/linkedEditingRange',
            result: {
              ranges: [
                {
                  start: {
                    line: 11,
                    character: 3,
                  },
                  end: {
                    line: 11,
                    character: 6,
                  },
                },
                {
                  start: {
                    line: 15,
                    character: 4,
                  },
                  end: {
                    line: 15,
                    character: 7,
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('opening nested', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/linkedEditingRange', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 12, character: 7}, // opening nested Foo
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/linkedEditingRange',
            result: {
              ranges: [
                {
                  start: {
                    line: 12,
                    character: 5,
                  },
                  end: {
                    line: 12,
                    character: 8,
                  },
                },
                {
                  start: {
                    line: 14,
                    character: 6,
                  },
                  end: {
                    line: 14,
                    character: 9,
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('opening fragment', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/linkedEditingRange', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 19, character: 3}, // opening fragment
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/linkedEditingRange',
            result: {
              ranges: [
                {
                  start: {
                    line: 19,
                    character: 3,
                  },
                  end: {
                    line: 19,
                    character: 3,
                  },
                },
                {
                  start: {
                    line: 21,
                    character: 4,
                  },
                  end: {
                    line: 21,
                    character: 4,
                  },
                },
              ],
            },
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('closing fragment', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/linkedEditingRange', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 21, character: 4}, // closing fragment
      }).verifyAllLSPMessagesInStep(
        [{method: 'textDocument/linkedEditingRange', result: null}],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('self closing', [
      addFile('jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/linkedEditingRange', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/jsx.js'},
        position: {line: 25, character: 5}, // self closing Foo
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/linkedEditingRange',
            result: null,
          },
        ],
        [
          'textDocument/publishDiagnostics',
          'window/showStatus',
          '$/cancelRequest',
        ],
      ),
    ]),
    test('malformed', [
      addFile('malformed_jsx.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/linkedEditingRange', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/malformed_jsx.js'},
        position: {line: 4, character: 7}, // inner div tag
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/linkedEditingRange',
            result: null,
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
