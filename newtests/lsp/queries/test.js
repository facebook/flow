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
    lspIgnoreStatusAndCancellation,
  }) => [
    test('invalid_method', [
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('foobar', {}).verifyAllIDEMessagesInStep(
        ['foobar{not implemented}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideNotification('barfoo', {})
        .waitUntilIDEMessage(2000, 'barfoo')
        .verifyAllIDEMessagesInStep(
          ['telemetry/event{not implemented}'],
          [...lspIgnoreStatusAndCancellation],
        ),
    ]),

    test('textDocument/definition', [
      addFile('definition.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>definition.js'},
        position: {line: 6, character: 1},
      }).verifyAllIDEMessagesInStep(
        [
          'textDocument/definition{definition.js,"start":{"line":2,"character":0}}',
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/definition', [
      addFile('definition.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>definition.js'},
        position: {line: 7, character: 11}, // over a comment
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{[]}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/definition', [
      addFile('definition.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>definition.js'},
        position: {line: 7, character: 1}, // over whitespace
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{[]}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/hover', [
      addFile('hover.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>hover.js'},
        position: {line: 6, character: 1}, // over a function use
      }).verifyAllIDEMessagesInStep(
        ['textDocument/hover{() => number}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>hover.js'},
        position: {line: 3, character: 1}, // over whitespace
      }).verifyAllIDEMessagesInStep(
        ['textDocument/hover{null}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>hover.js'},
        position: {line: 2, character: 1}, // over a keyword
      }).verifyAllIDEMessagesInStep(
        ['textDocument/hover{null}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>hover.js'},
        position: {line: 0, character: 1}, // over a comment
      }).verifyAllIDEMessagesInStep(
        ['textDocument/hover{null}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>hover.js'},
        position: {line: 6, character: 100}, // past the end of a line
      }).verifyAllIDEMessagesInStep(
        ['textDocument/hover{null}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>hover.js'},
        position: {line: 100, character: 0}, // past the end of the file
      }).verifyAllIDEMessagesInStep(
        ['textDocument/hover{null}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/documentHighlight', [
      addFiles('references.js', 'references2.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {line: 9, character: 17}, // on an identifier
      }).verifyAllIDEMessagesInStep(
        ['textDocument/documentHighlight{"line":3,"line":9}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {line: 9, character: 1}, // on a keyword
      }).verifyAllIDEMessagesInStep(
        ['textDocument/documentHighlight{[]}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {line: 6, character: 0}, // on whitespace
      }).verifyAllIDEMessagesInStep(
        ['textDocument/documentHighlight{[]}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/documentHighlight', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {line: 6, character: 100}, // off the right edge of the text
      }).verifyAllIDEMessagesInStep(
        ['textDocument/documentHighlight{[]}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/references', [
      addFiles('references.js', 'references2.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/references', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {line: 9, character: 17}, // on an identifier
      }).verifyAllIDEMessagesInStep(
        ['textDocument/references{line":3,"line":5,"line":9}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/references', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {
          line: 9,
          character: 17,
        }, // on an identifier
        context: {includeIndirectReferences: true},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/references{line":3,"line":5,"line":6,"line":9}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/rename', [
      addFiles('references.js', 'references2.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/rename', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {line: 9, character: 17}, // on an identifier
        newName: 'foobar',
      }).verifyAllIDEMessagesInStep(
        ['textDocument/rename{"line":3,"line":5,"line":9}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/documentSymbol', [
      addFiles('outline.js', 'references.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/documentSymbol', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>outline.js'},
      }).verifyAllIDEMessagesInStep(
        [
          'textDocument/documentSymbol{WORD_REGEX,State,Preferences,pref1,EPrefs,pref2,MyClass1,_projectRoot,command,constructor,dispose,MyInterface2,getFoo,myFunction3}',
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/typeCoverage', [
      addFiles('coverage.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>coverage.js'},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/typeCoverage{"line":12,"line":8,"line":6}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),

    test('textDocument/typeCoverage 2', [
      addFiles('coverage2.js'),
      ideStartAndConnect(),
      ideRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/coverage2.js'},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/typeCoverage{Use @flow}'],
        [...lspIgnoreStatusAndCancellation],
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
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
