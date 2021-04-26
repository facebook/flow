/*
 * @flow
 * @format
 */

import type Suite from 'flow-dev-tools/src/test/Suite.js';
import {suite, test} from 'flow-dev-tools/src/test/Tester';

/**
 * This test suite takes each query tested by lsp/queries and runs it once with
 * wait_for_recheck=true and once with wait_for_recheck=false. In each case,
 * we've triggered a 20s recheck and have a 2s timeout. Queries that run in
 * parallel with a recheck will return a response. Those that don't will
 * timeout
 */
export default (suite(
  ({
    lspStartAndConnect,
    lspNotification,
    lspRequestAndWaitUntilResponse,
    addFile,
    addFiles,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('invalid_method handled immediately when wait_for_recheck=true', [
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('foobar', {}).verifyAllLSPMessagesInStep(
        ['foobar'],
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
    ]).waitForRecheck(true),

    test('invalid_method handled immediately when wait_for_recheck=false', [
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('foobar', {}).verifyAllLSPMessagesInStep(
        ['foobar'],
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
    ]).waitForRecheck(false),

    test('textDocument/definition will time out with wait_for_recheck=true', [
      addFile('definition.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/definition.js'},
        position: {line: 6, character: 1},
      })
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/definition will return with wait_for_recheck=false', [
      addFile('definition.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/definition.js'},
        position: {line: 6, character: 1},
      })
        .verifyAllLSPMessagesInStep(
          [
            [
              'textDocument/definition',
              '{definition.js,"start":{"line":2,"character":9}}',
            ],
          ],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(false),

    test('textDocument/hover will time out with wait_for_recheck=true', [
      addFile('hover.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover.js'},
        position: {line: 6, character: 1}, // over a function use
      })
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/hover will return with wait_for_recheck=false', [
      addFile('hover.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/hover.js'},
        position: {line: 6, character: 1}, // over a function use
      })
        .verifyAllLSPMessagesInStep(
          [['textDocument/hover', '{() => number}']],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(false),

    test('textDocument/completion will time out with wait_for_recheck=true', [
      addFile('completion.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/completion.js'},
        position: {line: 10, character: 15}, // statement position
      })
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/completion will return with wait_for_recheck=false', [
      addFile('completion.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/completion', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/completion.js'},
        position: {line: 10, character: 15}, // statement position
      })
        .verifyAllLSPMessagesInStep(
          [
            [
              'textDocument/completion',
              '{"label":"x","label":"fred","detail":"(a: number, b: string) => number"}',
            ],
          ],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(false),

    test(
      'textDocument/documentHighlight will return with wait_for_recheck=false',
      [
        addFiles('references.js', 'references2.js'),
        lspStartAndConnect(),
        addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
          [''],
          [...lspIgnoreStatusAndCancellation],
        ),
        lspRequestAndWaitUntilResponse('textDocument/documentHighlight', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/references.js'},
          position: {line: 9, character: 17}, // on an identifier
        })
          .verifyAllLSPMessagesInStep(
            ['textDocument/documentHighlight'],
            [...lspIgnoreStatusAndCancellation],
          )
          .timeout(2000),
      ],
    ).waitForRecheck(false),

    test(
      'textDocument/documentHighlight will time out with wait_for_recheck=true',
      [
        addFiles('references.js', 'references2.js'),
        lspStartAndConnect(),
        addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
          [''],
          [...lspIgnoreStatusAndCancellation],
        ),
        lspRequestAndWaitUntilResponse('textDocument/documentHighlight', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/references.js'},
          position: {line: 9, character: 17}, // on an identifier
        })
          .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation])
          .timeout(2000),
      ],
    ).waitForRecheck(true),

    test('textDocument/references will time out with wait_for_recheck=true', [
      addFiles('references.js', 'references2.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/references', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/references.js'},
        position: {line: 9, character: 17}, // on an identifier
      })
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/references will time out with wait_for_recheck=false', [
      addFiles('references.js', 'references2.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/references', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/references.js'},
        position: {line: 9, character: 17}, // on an identifier
      })
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(false),

    test('textDocument/rename will time out with wait_for_recheck=true', [
      addFiles('references.js', 'references2.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/rename', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/references.js'},
        position: {line: 9, character: 17}, // on an identifier
        newName: 'foobar',
      })
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/rename will time out with wait_for_recheck=false', [
      addFiles('references.js', 'references2.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/rename', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/references.js'},
        position: {line: 9, character: 17}, // on an identifier
        newName: 'foobar',
      })
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(false),

    test('textDocument/documentSymbol will return with wait_for_recheck=true', [
      addFiles('outline.js', 'references.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/documentSymbol', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/outline.js'},
      })
        .verifyAllLSPMessagesInStep(
          [
            [
              'textDocument/documentSymbol',
              '{WORD_REGEX,State,Preferences,pref1,EPrefs,pref2,MyClass1,_projectRoot,command,constructor,dispose,MyInterface2,getFoo,myFunction3}',
            ],
          ],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(true),

    test(
      'textDocument/documentSymbol will return with wait_for_recheck=false',
      [
        addFiles('outline.js', 'references.js'),
        lspStartAndConnect(),
        addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
          [''],
          [...lspIgnoreStatusAndCancellation],
        ),
        lspRequestAndWaitUntilResponse('textDocument/documentSymbol', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/outline.js'},
        })
          .verifyAllLSPMessagesInStep(
            [
              [
                'textDocument/documentSymbol',
                '{WORD_REGEX,State,Preferences,pref1,EPrefs,pref2,MyClass1,_projectRoot,command,constructor,dispose,MyInterface2,getFoo,myFunction3}',
              ],
            ],
            [...lspIgnoreStatusAndCancellation],
          )
          .timeout(2000),
      ],
    ).waitForRecheck(false),

    test('textDocument/typeCoverage will time out with wait_for_recheck=true', [
      addFiles('coverage.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/coverage.js'},
      })
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(2000),
    ]).waitForRecheck(true),

    test('textDocument/typeCoverage will return with wait_for_recheck=false', [
      addFiles('coverage.js'),
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/typeCoverage', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/coverage.js'},
      })
        .verifyAllLSPMessagesInStep(
          [['textDocument/typeCoverage', '{"line":12,"line":8,"line":6}']],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(2000),
    ]).waitForRecheck(false),

    test('telemetry/rage will time out with wait_for_recheck=true', [
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('telemetry/rage', {})
        .verifyAllLSPMessagesInStep([], [...lspIgnoreStatusAndCancellation])
        .timeout(10000),
    ]).waitForRecheck(true),

    test('telemetry/rage will return with wait_for_recheck=false', [
      lspStartAndConnect(),
      addFile('sleep.js.ignored', 'sleep.js').verifyAllLSPMessagesInStep(
        [''],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('telemetry/rage', {})
        .verifyAllLSPMessagesInStep(
          ['telemetry/rage'],
          [...lspIgnoreStatusAndCancellation],
        )
        .timeout(10000),
    ]).waitForRecheck(false),
  ],
): Suite);
