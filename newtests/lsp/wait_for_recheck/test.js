/*
 * @flow
 * @format
 */

import {suite, test} from '../../../packages/flow-dev-tools/src/test/Tester';

/**
 * This test suite takes each query tested by lsp/queries and runs it once with
 * wait_for_recheck=true and once with wait_for_recheck=false. In each case,
 * we've triggered a 20s recheck and have a 2s timeout. Queries that run in
 * parallel with a recheck will return a response. Those that don't will
 * timeout
 */
export default suite(
  ({
    ideStartAndConnect,
    ideNotification,
    ideRequestAndWaitUntilResponse,
    addFile,
    addFiles,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('invalid_method handled immediately when wait_for_recheck=true', [
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('foobar', {}).verifyAllIDEMessagesInStep(
        ['foobar{unexpected error}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideNotification('barfoo', {})
        .waitUntilIDEMessage(2000, 'barfoo')
        .verifyAllIDEMessagesInStep(
          ['telemetry/event{not implemented}'],
          [...lspIgnoreStatusAndCancellation],
        ),
    ]).waitForRecheck(true),

    test('invalid_method handled immediately when wait_for_recheck=false', [
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('foobar', {}).verifyAllIDEMessagesInStep(
        ['foobar{unexpected error}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideNotification('barfoo', {})
        .waitUntilIDEMessage(2000, 'barfoo')
        .verifyAllIDEMessagesInStep(
          ['telemetry/event{not implemented}'],
          [...lspIgnoreStatusAndCancellation],
        ),
    ]).waitForRecheck(false),

    test('textDocument/definition will time out with wait_for_recheck=true', [
      addFile('definition.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>definition.js'},
        position: {line: 6, character: 1},
      })
        .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/definition will return with wait_for_recheck=false', [
      addFile('definition.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>definition.js'},
        position: {line: 6, character: 1},
      })
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/definition{definition.js,"start":{"line":2,"character":0}}',
          ],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(false),

    test('textDocument/hover will time out with wait_for_recheck=true', [
      addFile('hover.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>hover.js'},
        position: {line: 6, character: 1}, // over a function use
      })
        .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/hover will return with wait_for_recheck=false', [
      addFile('hover.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>hover.js'},
        position: {line: 6, character: 1}, // over a function use
      })
        .verifyAllIDEMessagesInStep(
          ['textDocument/hover{() => number}'],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(false),

    test('textDocument/completion will time out with wait_for_recheck=true', [
      addFile('completion.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>completion.js'},
        position: {line: 10, character: 15}, // statement position
      })
        .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/completion will return with wait_for_recheck=false', [
      addFile('completion.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>completion.js'},
        position: {line: 10, character: 15}, // statement position
      })
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/completion{"label":"x","label":"fred","detail":"(a: number, b: string) => number","inlineDetail":"(a: number, b: string)"}',
          ],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(false),

    test(
      'textDocument/documentHighlight will time out with wait_for_recheck=false',
      [
        addFiles('references.js', 'references2.js'),
        ideStartAndConnect(),
        addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
          [''],
          [...lspIgnoreStatusAndCancellation],
        ),
        ideRequestAndWaitUntilResponse('textDocument/documentHighlight', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
          position: {line: 9, character: 17}, // on an identifier
        })
          .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
          .timeout(2000),
      ],
    ).waitForRecheck(false),

    test(
      'textDocument/documentHighlight will time out with wait_for_recheck=true',
      [
        addFiles('references.js', 'references2.js'),
        ideStartAndConnect(),
        addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
          [''],
          [...lspIgnoreStatusAndCancellation],
        ),
        ideRequestAndWaitUntilResponse('textDocument/documentHighlight', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
          position: {line: 9, character: 17}, // on an identifier
        })
          .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
          .timeout(2000),
      ],
    ).waitForRecheck(true),

    test('textDocument/references will time out with wait_for_recheck=true', [
      addFiles('references.js', 'references2.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/references', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {line: 9, character: 17}, // on an identifier
      })
        .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/references will time out with wait_for_recheck=false', [
      addFiles('references.js', 'references2.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/references', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {line: 9, character: 17}, // on an identifier
      })
        .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(false),

    test('textDocument/rename will time out with wait_for_recheck=true', [
      addFiles('references.js', 'references2.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/rename', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {line: 9, character: 17}, // on an identifier
        newName: 'foobar',
      })
        .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/rename will time out with wait_for_recheck=false', [
      addFiles('references.js', 'references2.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/rename', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>references.js'},
        position: {line: 9, character: 17}, // on an identifier
        newName: 'foobar',
      })
        .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(false),

    test('textDocument/documentSymbol will return with wait_for_recheck=true', [
      addFiles('outline.js', 'references.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/documentSymbol', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>outline.js'},
      })
        .verifyAllIDEMessagesInStep(
          [
            'textDocument/documentSymbol{WORD_REGEX,State,Preferences,pref1,EPrefs,pref2,MyClass1,_projectRoot,command,constructor,dispose,MyInterface2,getFoo,myFunction3}',
          ],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(true),

    test(
      'textDocument/documentSymbol will return with wait_for_recheck=false',
      [
        addFiles('outline.js', 'references.js'),
        ideStartAndConnect(),
        addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
          [''],
          [...lspIgnoreStatusAndCancellation],
        ),
        ideRequestAndWaitUntilResponse('textDocument/documentSymbol', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>outline.js'},
        })
          .verifyAllIDEMessagesInStep(
            [
              'textDocument/documentSymbol{WORD_REGEX,State,Preferences,pref1,EPrefs,pref2,MyClass1,_projectRoot,command,constructor,dispose,MyInterface2,getFoo,myFunction3}',
            ],
            [...lspIgnoreStatusAndCancellation],
          )
          .timeout(2000),
      ],
    ).waitForRecheck(false),

    test('textDocument/typeCoverage will time out with wait_for_recheck=true', [
      addFiles('coverage.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>coverage.js'},
      })
        .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/typeCoverage will return with wait_for_recheck=false', [
      addFiles('coverage.js'),
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>coverage.js'},
      })
        .verifyAllIDEMessagesInStep(
          ['textDocument/typeCoverage{"line":12,"line":8,"line":6}'],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(false),

    test('telemetry/rage will time out with wait_for_recheck=true', [
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/coverage2.js'},
      })
        .verifyAllIDEMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('telemetry/rage will return with wait_for_recheck=false', [
      ideStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllIDEMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_DIR>/coverage2.js'},
      })
        .verifyAllIDEMessagesInStep(
          ['textDocument/typeCoverage{Use @flow}'],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(false),
  ],
);
