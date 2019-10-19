/*
 * @flow
 * @format
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    startFlowServer,
    lspStart,
    lspStartAndConnect,
    lspRequest,
    lspNotification,
    lspResponse,
    lspRequestAndWaitUntilResponse,
    waitUntilLSPStatus,
    waitUntilServerStatus,
    flowCmd,
    modifyFile,
    lspInitializeParams,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('Warm flow starts up, and server remains running after shutdown', [
      lspStart({needsFlowServer: true}),
      lspRequest('initialize', lspInitializeParams)
        .waitUntilLSPMessage(60000, 'telemetry/connectionStatus')
        .verifyAllLSPMessagesInStep(
          ['initialize', 'telemetry/connectionStatus{true}'],
          [...lspIgnoreStatusAndCancellation],
        ),
      lspRequest('shutdown')
        .waitUntilLSPMessage(20000, 'shutdown')
        .verifyAllLSPMessagesInStep(
          ['shutdown'],
          ['telemetry/connectionStatus', ...lspIgnoreStatusAndCancellation],
        ),
      lspNotification('exit')
        .waitUntilLSPStatus(20000, 'stopped')
        .verifyLSPStatus('stopped')
        .verifyServerStatus('running'),
    ]),

    test('Cold flow starts up with progress, and shuts down', [
      lspStart({needsFlowServer: false}),
      lspRequest('initialize', lspInitializeParams)
        .waitUntilLSPMessage(60000, 'telemetry/connectionStatus')
        .verifyAllLSPMessagesInStep(
          [
            'initialize',
            'window/logMessage{Starting Flow server}',
            'telemetry/connectionStatus{true}',
          ],
          [...lspIgnoreStatusAndCancellation],
        ),
      lspRequest('shutdown')
        .waitUntilLSPMessage(20000, 'shutdown')
        .verifyAllLSPMessagesInStep(
          ['shutdown'],
          ['telemetry/connectionStatus', ...lspIgnoreStatusAndCancellation],
        ),
      lspNotification('exit')
        .waitUntilLSPStatus(20000, 'stopped')
        .waitUntilServerStatus(20000, 'stopped')
        .verifyLSPStatus('stopped')
        .verifyServerStatus('stopped'),
    ]),

    test('Termination in-flight, and external restart', [
      lspStartAndConnect(),
      flowCmd(['stop'])
        .waitUntilServerStatus(20000, 'stopped')
        .waitUntilLSPMessage(20000, 'window/showStatus{stopped}')
        .verifyAllLSPMessagesInStep(
          [
            'telemetry/connectionStatus{false}',
            'telemetry/event{End_of_file}',
            'window/showStatus{stopped}',
          ],
          [
            // After the EOF, lsp's reconnection attempt might occur
            // before the monitor has also shut down (in which case it
            // will display "connecting...") or after (in which cast it won't)
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
      startFlowServer()
        .waitUntilLSPMessage(60000, 'telemetry/connectionStatus')
        // it really can take a while for flow to be ready to connect
        .verifyAllLSPMessagesInStep(
          ['telemetry/connectionStatus{true}'],
          [...lspIgnoreStatusAndCancellation],
        ),
    ]),

    test('Termination in-flight, and internal restart', [
      lspStartAndConnect(),
      flowCmd(['stop'])
        .waitUntilServerStatus(20000, 'stopped')
        .waitUntilLSPMessage(20000, 'window/showStatus{stopped}')
        .verifyAllLSPMessagesInStep(
          [
            'telemetry/connectionStatus{false}',
            'telemetry/event{End_of_file}',
            'window/showStatus{stopped}',
          ],
          [...lspIgnoreStatusAndCancellation],
        ),
      lspResponse('mostRecent', {title: 'Restart'})
        // .waitUntilServerStatus(60000, 'running') -- commented out because
        // the method currently only waits for servers that the test infrastructure
        // launched; not ones that lspCommand launched.
        .waitUntilLSPMessage(60000, 'telemetry/connectionStatus')
        .verifyAllLSPMessagesInStep(
          ['window/logMessage{Starting}', 'telemetry/connectionStatus{true}'],
          [...lspIgnoreStatusAndCancellation],
        ),
    ]),

    /*
    TODO(ljw): fix race. The following test is fine in theory...
    But on AppVeyor, what happens 1 in 50 runs is that 'flow force-recheck --no-auto-start .flowconfig'
    sends a message to the monitor and thence the server telling it to force-recheck,
    and it's a race whether monitor would be able to send the response back to
    forceRecheckCommand before dying or not. If it happens to die before it can
    send a response then forceRecheckCommand will use its retry logic to send
    the FORCE_RECHECK request a second time. And if lspCommand happens to restart
    the server before that retry request is sent, then the retry request will
    end up killing the newly started server!
    I'm disabling the test for now so it doesn't interfere with AppVeyor.
    In any case, it won't have a bad user experience - the retry behavior of
    forceRecheckCommand doesn't correspond to any real watchman behavior; and if
    the user does happen to do forceRecheckCommand in a way that stops flow,
    then the worst that will happen is that Nucllsp pops up a box saying
    "flow is stopped [restart]".

    test('Restarts a lost server in response to flowconfig benign change', [
      lspStartAndConnect(),
      modifyFile('.flowconfig', '#placeholder', '#replaced')
        .waitUntilLSPMessage(20000, 'telemetry/connectionStatus{false}')
        .dontMindServerDeath()
        .waitUntilLSPMessage(60000, 'telemetry/connectionStatus{true}')
        .verifyAllLSPMessagesInStep(
          [
            'telemetry/connectionStatus{false}',
            'telemetry/event{Server fatal exception}',
            'window/logMessage{Starting}',
            'telemetry/connectionStatus{true}',
          ],
          [...lspIgnoreStatusAndCancellation],
        ),
    ]),
*/

    test('Terminates in response to flowconfig version change', [
      lspStartAndConnect(),
      modifyFile('.flowconfig', '>0.60.0', '>0.61.0')
        .waitUntilServerStatus(20000, 'stopped')
        .waitUntilLSPStatus(20000, 'stopped')
        .verifyAllLSPMessagesInStep(
          [
            'telemetry/connectionStatus{false}',
            'telemetry/event{Server fatal exception}',
          ],
          [
            'telemetry/event{Version in flowconfig}', // might or might not come depending on how fast
            ...lspIgnoreStatusAndCancellation,
          ],
        ),
    ]),

    test('Editor open files outlive server', [
      lspStartAndConnect(),
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/open.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow
function jones(): number { return 15; }
jones();
`,
        },
      })
        .lspRequestAndWaitUntilResponse('textDocument/definition', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/open.js'},
          position: {line: 2, character: 1},
        })
        .verifyAllLSPMessagesInStep(
          ['textDocument/definition{open.js,"line":1}'],
          [...lspIgnoreStatusAndCancellation],
        ),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/open.js'},
        position: {line: 2, character: 1},
      }).verifyAllLSPMessagesInStep(
        ['textDocument/definition{open.js,"line":1}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/open.js'},
        position: {line: 2, character: 1},
      }).verifyAllLSPMessagesInStep(
        ['textDocument/definition{open.js,"line":1}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      flowCmd(['stop'])
        .waitUntilServerStatus(20000, 'stopped')
        .waitUntilLSPMessage(20000, 'telemetry/connectionStatus{false}')
        .verifyAllLSPMessagesInStep(
          ['telemetry/connectionStatus{false}'],
          ['telemetry/event{End_of_file}', ...lspIgnoreStatusAndCancellation],
        ),
      startFlowServer()
        .waitUntilLSPMessage(60000, 'telemetry/connectionStatus')
        .verifyAllLSPMessagesInStep(
          ['telemetry/connectionStatus{true}'],
          [...lspIgnoreStatusAndCancellation],
        ),
      lspRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/open.js'},
        position: {line: 2, character: 1},
      }).verifyAllLSPMessagesInStep(
        ['textDocument/definition{open.js,line":1}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
