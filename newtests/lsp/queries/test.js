/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    ideStartAndConnect,
    ideNotification,
    ideRequestAndWaitUntilResponse,
    addFile,
    addFiles,
  }) => [
    test('invalid_method', [
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('foobar', {}).verifyAllIDEMessagesInStep(
        ['foobar{not implemented}'],
        [],
      ),
      ideNotification('barfoo', {})
        .waitUntilIDEMessage(2000, 'barfoo')
        .verifyAllIDEMessagesInStep(['telemetry/event{not implemented}'], []),
    ]),

    test('textDocument/definition', [
      addFile('definition.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/definition.js'},
        position: {line: 6, character: 1},
      }).verifyAllIDEMessagesInStep(
        [
          'textDocument/definition{definition.js,"start":{"line":2,"character":0}}',
        ],
        [],
      ),
    ]),

    test('textDocument/hover', [
      addFile('hover.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 6, character: 1}, // over a function use
      }).verifyAllIDEMessagesInStep(['textDocument/hover{() => number}'], []),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 3, character: 1}, // over whitespace
      }).verifyAllIDEMessagesInStep(['textDocument/hover{null}'], []),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 2, character: 1}, // over a keyword
      }).verifyAllIDEMessagesInStep(['textDocument/hover{null}'], []),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 0, character: 1}, // over a comment
      }).verifyAllIDEMessagesInStep(['textDocument/hover{null}'], []),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 6, character: 100}, // past the end of a line
      }).verifyAllIDEMessagesInStep(['textDocument/hover{null}'], []),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/hover.js'},
        position: {line: 100, character: 0}, // past the end of the file
      }).verifyAllIDEMessagesInStep(['textDocument/hover{null}'], []),
    ]),

    test('textDocument/completion', [
      addFile('completion.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/completion.js'},
        position: {line: 10, character: 15}, // statement position
      }).verifyAllIDEMessagesInStep(
        [
          'textDocument/completion{"label":"x","label":"fred","detail":"(a: number, b: string) => number","inlineDetail":"(a: number, b: string)"}',
        ],
        [],
      ),
    ]),

    test('textDocument/documentHighlight', [
      addFiles('references.js', 'references2.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/references.js'},
        position: {line: 9, character: 17}, // on an identifier
      }).verifyAllIDEMessagesInStep(
        ['textDocument/documentHighlight{"line":3,"line":9}'],
        [],
      ),
      ideRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/references.js'},
        position: {line: 9, character: 1}, // on a keyword
      }).verifyAllIDEMessagesInStep(['textDocument/documentHighlight{[]}'], []),
      ideRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/references.js'},
        position: {line: 6, character: 0}, // on whitespace
      }).verifyAllIDEMessagesInStep(['textDocument/documentHighlight{[]}'], []),
      ideRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/references.js'},
        position: {line: 6, character: 100}, // off the right edge of the text
      }).verifyAllIDEMessagesInStep(['textDocument/documentHighlight{[]}'], []),
    ]),

    test('textDocument/references', [
      addFiles('references.js', 'references2.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/references', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/references.js'},
        position: {line: 9, character: 17}, // on an identifier
      }).verifyAllIDEMessagesInStep(
        ['textDocument/references{line":3,"line":5,"line":9}'],
        [],
      ),
    ]),

    test('textDocument/documentSymbol', [
      addFiles('outline.js', 'references.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/documentSymbol', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/outline.js'},
      }).verifyAllIDEMessagesInStep(
        [
          'textDocument/documentSymbol{WORD_REGEX,State,Preferences,pref1,EPrefs,pref2,MyClass1,_projectRoot,command,constructor,dispose,MyInterface2,getFoo,myFunction3}',
        ],
        [],
      ),
    ]),

    test('textDocument/typeCoverage', [
      addFiles('coverage.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/coverage.js'},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/typeCoverage{"line":12,"line":8,"line":6}'],
        [],
      ),
    ]),

    test('textDocument/typeCoverage 2', [
      addFiles('coverage2.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/coverage2.js'},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/typeCoverage{Use @flow}'],
        [],
      ),
    ]),

    test('telemetry/rage', [
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse(
        'telemetry/rage',
        {},
      ).verifyAllIDEMessagesInStep(
        [
          'telemetry/rage{Focused: 1,LSP adapter state: Connected,.monitor_log,.log}',
        ],
        [],
      ),
    ]),
  ],
);
