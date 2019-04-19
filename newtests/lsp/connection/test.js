/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    startFlowServer,
    ideStart,
    ideStartAndConnect,
    ideRequest,
    ideNotification,
    ideResponse,
    ideRequestAndWaitUntilResponse,
    waitUntilIDEStatus,
    waitUntilServerStatus,
    flowCmd,
    modifyFile,
    lspInitializeParams,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('Warm flow starts up, and server remains running after shutdown', [
      ideStart({mode: 'lsp', needsFlowServer: true}),
      ideRequest('initialize', lspInitializeParams)
        .waitUntilIDEMessage(60000, 'telemetry/connectionStatus')
        .verifyAllIDEMessagesInStep(
          ['initialize', 'telemetry/connectionStatus{true}'],
          [...lspIgnoreStatusAndCancellation],
        ),
      ideRequest('shutdown')
        .waitUntilIDEMessage(20000, 'shutdown')
        .verifyAllIDEMessagesInStep(
          ['shutdown'],
          ['telemetry/connectionStatus', ...lspIgnoreStatusAndCancellation],
        ),
      ideNotification('exit')
        .waitUntilIDEStatus(20000, 'stopped')
        .verifyIDEStatus('stopped')
        .verifyServerStatus('running'),
    ]),

    test('Cold flow starts up with progress, and shuts down', [
      ideStart({mode: 'lsp', needsFlowServer: false}),
      ideRequest('initialize', lspInitializeParams)
        .waitUntilIDEMessage(60000, 'telemetry/connectionStatus')
        .verifyAllIDEMessagesInStep(
          [
            'initialize',
            'window/logMessage{Starting Flow server}',
            'telemetry/connectionStatus{true}',
          ],
          [...lspIgnoreStatusAndCancellation],
        ),
      ideRequest('shutdown')
        .waitUntilIDEMessage(20000, 'shutdown')
        .verifyAllIDEMessagesInStep(
          ['shutdown'],
          ['telemetry/connectionStatus', ...lspIgnoreStatusAndCancellation],
        ),
      ideNotification('exit')
        .waitUntilIDEStatus(20000, 'stopped')
        .waitUntilServerStatus(20000, 'stopped')
        .verifyIDEStatus('stopped')
        .verifyServerStatus('stopped'),
    ]),

    test('Termination in-flight, and external restart', [
      ideStartAndConnect(),
      flowCmd(['stop'])
        .waitUntilServerStatus(20000, 'stopped')
        .waitUntilIDEMessage(20000, 'window/showStatus{stopped}')
        .verifyAllIDEMessagesInStep(
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
        .waitUntilIDEMessage(60000, 'telemetry/connectionStatus')
        // it really can take a while for flow to be ready to connect
        .verifyAllIDEMessagesInStep(
          ['telemetry/connectionStatus{true}'],
          [...lspIgnoreStatusAndCancellation],
        ),
    ]),

    test('Termination in-flight, and internal restart', [
      ideStartAndConnect(),
      flowCmd(['stop'])
        .waitUntilServerStatus(20000, 'stopped')
        .waitUntilIDEMessage(20000, 'window/showStatus{stopped}')
        .verifyAllIDEMessagesInStep(
          [
            'telemetry/connectionStatus{false}',
            'telemetry/event{End_of_file}',
            'window/showStatus{stopped}',
          ],
          [...lspIgnoreStatusAndCancellation],
        ),
      ideResponse('mostRecent', {title: 'Restart'})
        // .waitUntilServerStatus(60000, 'running') -- commented out because
        // the method currently only waits for servers that the test infrastructure
        // launched; not ones that lspCommand launched.
        .waitUntilIDEMessage(60000, 'telemetry/connectionStatus')
        .verifyAllIDEMessagesInStep(
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
    then the worst that will happen is that Nuclide pops up a box saying
    "flow is stopped [restart]".

    test('Restarts a lost server in response to flowconfig benign change', [
      ideStartAndConnect(),
      modifyFile('.flowconfig', '#placeholder', '#replaced')
        .waitUntilIDEMessage(20000, 'telemetry/connectionStatus{false}')
        .dontMindServerDeath()
        .waitUntilIDEMessage(60000, 'telemetry/connectionStatus{true}')
        .verifyAllIDEMessagesInStep(
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
      ideStartAndConnect(),
      modifyFile('.flowconfig', '>0.60.0', '>0.61.0')
        .waitUntilServerStatus(20000, 'stopped')
        .waitUntilIDEStatus(20000, 'stopped')
        .verifyAllIDEMessagesInStep(
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
      ideStartAndConnect(),
      ideNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow
function jones(): number { return 15; }
jones();
`,
        },
      })
        .ideRequestAndWaitUntilResponse('textDocument/definition', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
          position: {line: 2, character: 1},
        })
        .verifyAllIDEMessagesInStep(
          ['textDocument/definition{open.js,"line":1}'],
          [...lspIgnoreStatusAndCancellation],
        ),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
        position: {line: 2, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{open.js,"line":1}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
        position: {line: 2, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{open.js,"line":1}'],
        [...lspIgnoreStatusAndCancellation],
      ),
      flowCmd(['stop'])
        .waitUntilServerStatus(20000, 'stopped')
        .waitUntilIDEMessage(20000, 'telemetry/connectionStatus{false}')
        .verifyAllIDEMessagesInStep(
          ['telemetry/connectionStatus{false}'],
          ['telemetry/event{End_of_file}', ...lspIgnoreStatusAndCancellation],
        ),
      startFlowServer()
        .waitUntilIDEMessage(60000, 'telemetry/connectionStatus')
        .verifyAllIDEMessagesInStep(
          ['telemetry/connectionStatus{true}'],
          [...lspIgnoreStatusAndCancellation],
        ),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
        position: {line: 2, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{open.js,line":1}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
