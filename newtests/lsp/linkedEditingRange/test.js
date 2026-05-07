/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

module.exports = suite(
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
                  end: {
                    character: 6,
                    line: 5,
                  },
                  start: {
                    character: 3,
                    line: 5,
                  },
                },
                {
                  end: {
                    character: 7,
                    line: 7,
                  },
                  start: {
                    character: 4,
                    line: 7,
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
                  end: {
                    character: 6,
                    line: 11,
                  },
                  start: {
                    character: 3,
                    line: 11,
                  },
                },
                {
                  end: {
                    character: 7,
                    line: 15,
                  },
                  start: {
                    character: 4,
                    line: 15,
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
                  end: {
                    character: 8,
                    line: 12,
                  },
                  start: {
                    character: 5,
                    line: 12,
                  },
                },
                {
                  end: {
                    character: 9,
                    line: 14,
                  },
                  start: {
                    character: 6,
                    line: 14,
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
                  end: {
                    character: 3,
                    line: 19,
                  },
                  start: {
                    character: 3,
                    line: 19,
                  },
                },
                {
                  end: {
                    character: 4,
                    line: 21,
                  },
                  start: {
                    character: 4,
                    line: 21,
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
) as SuiteType;
