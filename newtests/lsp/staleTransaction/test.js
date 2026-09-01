/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

module.exports = suite(
  ({
    lspStartAndConnect,
    lspNotification,
    lspRequestAndWaitUntilResponse,
    addFiles,
    lspIgnoreStatusAndCancellation,
  }) => [
    // The live errors run and the hover are separate units of work, each with its own
    // transaction, and the dispatcher takes the first one's committed-heap guard back
    // when it returns. The artifacts the live errors run cached hold lazy type thunks;
    // in lazy mode the hover is what first forces the ones reaching `b.js` and `c.js`.
    // Those thunks used to read the transaction that created them, which by then was
    // released, and the hover died with "transaction was read after its guard was
    // released".
    test('hover forces thunks cached by an earlier request', [
      addFiles('a.js', 'b.js', 'c.js'),
      lspStartAndConnect(),
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/main.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow

import Widget from './a';

export default function App(): typeof Widget {
  return Widget;
}
`,
        },
      })
        .waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllLSPMessagesInStep(
          [],
          ['window/showStatus', 'textDocument/publishDiagnostics'],
        ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/main.js'},
        position: {line: 2, character: 8},
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'textDocument/hover',
            result: {
              contents: [
                {
                  language: 'flow',
                  value: 'component Widget(...Props)',
                },
                '`Props` defined at [`b.js:3:12`](<PLACEHOLDER_PROJECT_URL>/b.js#L3,13)',
                '`Widget` defined at [`a.js:4:15`](<PLACEHOLDER_PROJECT_URL>/a.js#L4,16)',
              ],
              range: {
                end: {
                  character: 13,
                  line: 2,
                },
                start: {
                  character: 7,
                  line: 2,
                },
              },
            },
          },
        ],
        [
          'window/showStatus',
          '$/cancelRequest',
          'textDocument/publishDiagnostics',
        ],
      ),
    ]),
  ],
) as SuiteType;
