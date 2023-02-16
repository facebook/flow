/*
 * @flow
 * @format
 */

import type {Suite} from 'flow-dev-tools/src/test/Suite';
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(
  ({
    lspStartAndConnect,
    lspNotification,
    lspRequestAndWaitUntilResponse,
    addFile,
    addFiles,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('invalid_method', [
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('foobar', {}).verifyAllLSPMessagesInStep(
        [
          {
            method: 'foobar',
            error: {message: 'Unhandled method foobar', code: -32601},
          },
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspNotification('barfoo', {})
        .waitUntilLSPMessage(2000, 'telemetry/event')
        .verifyAllLSPMessagesInStep(
          [
            {
              method: 'telemetry/event',
              params: {type: 1, message: 'Unhandled method barfoo'},
            },
          ],
          [...lspIgnoreStatusAndCancellation],
        ),
    ]),

    test('textDocument/definition', [
      addFile('definition.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/definition.js'},
        position: {line: 6, character: 1},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/definition',
            '{definition.js,"start":{"line":2,"character":9}}',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/definition', [
      addFile('definition.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/definition.js'},
        position: {line: 7, character: 11}, // over a comment
      }).verifyAllLSPMessagesInStep(
        [['textDocument/definition', '{[]}']],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/definition', [
      addFile('definition.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/definition.js'},
        position: {line: 7, character: 1}, // over whitespace
      }).verifyAllLSPMessagesInStep(
        [['textDocument/definition', '{[]}']],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/hover', [
      addFile('hover.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover.js'},
        position: {line: 6, character: 1}, // over a function use
      }).verifyAllLSPMessagesInStep(
        [['textDocument/hover', '{() => number}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover.js'},
        position: {line: 3, character: 1}, // over whitespace
      }).verifyAllLSPMessagesInStep(
        [['textDocument/hover', '{null}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover.js'},
        position: {line: 2, character: 1}, // over a keyword
      }).verifyAllLSPMessagesInStep(
        [['textDocument/hover', '{null}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover.js'},
        position: {line: 0, character: 1}, // over a comment
      }).verifyAllLSPMessagesInStep(
        [['textDocument/hover', '{null}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover.js'},
        position: {line: 6, character: 100}, // past the end of a line
      }).verifyAllLSPMessagesInStep(
        [['textDocument/hover', '{null}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover.js'},
        position: {line: 100, character: 0}, // past the end of the file
      }).verifyAllLSPMessagesInStep(
        [['textDocument/hover', '{null}']],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/hover evaluate', [
      addFile('hover_evaluate.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover_evaluate.js'},
        position: {line: 10, character: 6}, // T1
      }).verifyAllLSPMessagesInStep(
        [['textDocument/hover', '{type T1 = Foo}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover_evaluate.js'},
        position: {line: 12, character: 6}, // T2
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/hover',
            'type T2 = Foo["bar"]\n= {baz: ?{qux: $ReadOnlyArray<string>, ...}, ...}',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover_evaluate.js'},
        position: {line: 14, character: 6}, // T3
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/hover',
            'type T3 = Foo["bar"]["baz"]\n= ?{qux: $ReadOnlyArray<string>, ...}',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover_evaluate.js'},
        position: {line: 16, character: 6}, // T4
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/hover',
            'type T4 = Foo["bar"]["baz"]?.["qux"]\n= $ReadOnlyArray<string> | void',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover_evaluate.js'},
        position: {line: 18, character: 6}, // T5
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/hover',
            'type T5 = Foo["bar"]["baz"]?.["qux"][number]\n= string | void',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover_evaluate.js'},
        position: {line: 20, character: 6}, // T6
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/hover',
            'type T6 = $NonMaybeType<Foo["bar"]["baz"]?.["qux"][number]>\n= string',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/documentHighlight', [
      addFiles('references.js', 'references2.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/references.js'},
        position: {line: 9, character: 17}, // on an identifier
      }).verifyAllLSPMessagesInStep(
        [['textDocument/documentHighlight', '{"line":3,"line":9}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/references.js'},
        position: {line: 9, character: 1}, // on a keyword
      }).verifyAllLSPMessagesInStep(
        [['textDocument/documentHighlight', '{[]}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/references.js'},
        position: {line: 6, character: 0}, // on whitespace
      }).verifyAllLSPMessagesInStep(
        [['textDocument/documentHighlight', '{[]}']],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/references.js'},
        position: {line: 6, character: 100}, // off the right edge of the text
      }).verifyAllLSPMessagesInStep(
        [['textDocument/documentHighlight', '{[]}']],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/documentSymbol', [
      addFiles('outline.js', 'references.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/documentSymbol', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/outline.js'},
      }).verifyAllLSPMessagesInStep(
        [
          [
            'textDocument/documentSymbol',
            '{WORD_REGEX,State,Preferences,pref1,EPrefs,pref2,MyClass1,_projectRoot,command,constructor,dispose,MyInterface2,getFoo,myFunction3}',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/typeCoverage', [
      addFiles('coverage.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/coverage.js'},
      }).verifyAllLSPMessagesInStep(
        ['textDocument/typeCoverage'],
        ['window/showStatus', '$/cancelRequest'],
      ),
    ]),

    test('textDocument/typeCoverage 2', [
      addFiles('coverage2.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/coverage2.js'},
      }).verifyAllLSPMessagesInStep(
        [['textDocument/typeCoverage', '{Use @flow}']],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('telemetry/rage', [
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse(
        'telemetry/rage',
        {},
      ).verifyAllLSPMessagesInStep(
        [
          [
            'telemetry/rage',
            '{Focused: 1,LSP adapter state: Connected,.monitor_log,.log}',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
): Suite);
