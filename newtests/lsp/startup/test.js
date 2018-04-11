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
    lspExpect,
    waitUntilIDEStatus,
    waitUntilServerStatus,
    flowCmd,
    modifyFile,
    lspInitializeParams,
  }) => [
    test('Warm flow starts up, and server remains running after shutdown', [
      ideStart({mode: 'lsp', needsFlowServer: true}),
      ideRequest('initialize', lspInitializeParams)
        .waitUntilIDEMessage(20000, 'telemetry/connectionStatus')
        .verifyAllIDEMessagesInStep(
          ['initialize', 'telemetry/connectionStatus{true}'],
          [],
        ),
      ideRequest('shutdown')
        .waitUntilIDEMessage(3000, 'shutdown')
        .verifyAllIDEMessagesInStep(
          ['shutdown'],
          ['telemetry/connectionStatus'],
        ),
      ideNotification('exit')
        .waitUntilIDEStatus(3000, 'stopped')
        .verifyIDEStatus('stopped')
        .verifyServerStatus('running'),
    ]),

    test('Cold flow starts up with progress, and shuts down', [
      ideStart({mode: 'lsp', needsFlowServer: false}),
      ideRequest('initialize', lspInitializeParams)
        .waitUntilIDEMessage(30000, 'telemetry/connectionStatus')
        .verifyAllIDEMessagesInStep(
          [
            'initialize',
            'window/logMessage{Starting Flow server}',
            'window/showMessageRequest{"id":1,Connecting}',
            '$/cancelRequest{"id":1}',
            'window/progress{null}',
            'telemetry/connectionStatus{true}',
          ],
          ['window/progress{Connecting}'],
        ),
      ideRequest('shutdown')
        .waitUntilIDEMessage(3000, 'shutdown')
        .verifyAllIDEMessagesInStep(
          ['shutdown'],
          ['telemetry/connectionStatus'],
        ),
      ideNotification('exit')
        .waitUntilIDEStatus(3000, 'stopped')
        .waitUntilServerStatus(3000, 'stopped')
        .verifyIDEStatus('stopped')
        .verifyServerStatus('stopped'),
    ]),

    test('Termination in-flight, and external restart', [
      ideStartAndConnect(),
      flowCmd(['stop'])
        .waitUntilServerStatus(3000, 'stopped')
        .waitUntilIDEMessage(3000, 'window/actionRequired')
        .verifyAllIDEMessagesInStep(
          [
            'telemetry/connectionStatus{false}',
            'telemetry/event{End_of_file}',
            'window/showMessageRequest{stopped}',
            'window/actionRequired{stopped}',
          ],
          [
            // After the EOF, lsp's reconnection attempt might occur
            // before the monitor has also shut down (in which case it
            // will display "connecting...") or after (in which cast it won't)
            'window/actionRequired{null}',
            'window/showMessageRequest{Connecting}',
            'window/progress',
            '$/cancelRequest',
          ],
        ),
      startFlowServer()
        .waitUntilIDEMessage(20000, 'telemetry/connectionStatus')
        // it really can take a while for flow to be ready to connect
        .verifyAllIDEMessagesInStep(
          ['telemetry/connectionStatus{true}'],
          [
            // The "Connecting" dialog might or might not appear, depending
            // on how quickly it reconnects
            'window/actionRequired{null}',
            'window/showMessageRequest{Connecting}',
            'window/progress',
            '$/cancelRequest', // remove "Connecting" dialog
          ],
        ),
    ]),

    test('Termination in-flight, and internal restart', [
      ideStartAndConnect(),
      flowCmd(['stop'])
        .waitUntilServerStatus(3000, 'stopped')
        .waitUntilIDEMessage(3000, 'window/actionRequired')
        .verifyAllIDEMessagesInStep(
          [
            'telemetry/connectionStatus{false}',
            'telemetry/event{End_of_file}',
            'window/showMessageRequest{stopped}',
            'window/actionRequired{stopped}',
          ],
          [
            'window/showMessageRequest{Connecting}',
            'window/progress',
            '$/cancelRequest',
          ],
        ),
      ideResponse('mostRecent', {title: 'Restart'})
        .waitUntilServerStatus(20000, 'running')
        .waitUntilIDEMessage(10000, 'telemetry/connectionStatus')
        .verifyAllIDEMessagesInStep(
          [
            'window/actionRequired{null}',
            'window/logMessage{Starting}',
            'telemetry/connectionStatus{true}',
          ],
          [
            'window/showMessageRequest{Connecting}',
            'window/progress',
            '$/cancelRequest',
          ],
        ),
    ]),

    test('Restarts a lost server in response to flowconfig benign change', [
      ideStartAndConnect(),
      modifyFile('.flowconfig', '#placeholder', '#replaced')
        .waitUntilIDEMessage(3000, 'telemetry/connectionStatus{false}')
        .dontMindServerDeath()
        .waitUntilIDEMessage(20000, 'telemetry/connectionStatus{true}')
        .verifyAllIDEMessagesInStep(
          [
            'telemetry/connectionStatus{false}',
            'telemetry/event{Server fatal exception}',
            'window/logMessage{Starting}',
            'telemetry/connectionStatus{true}',
          ],
          ['window/showMessageRequest', 'window/progress', '$/cancelRequest'],
        ),
    ]),

    test('Terminates in response to flowconfig version change', [
      ideStartAndConnect(),
      modifyFile('.flowconfig', '>0.60.0', '>0.61.0')
        .waitUntilServerStatus(3000, 'stopped')
        .waitUntilIDEStatus(6000, 'stopped')
        .verifyAllIDEMessagesInStep(
          [
            'telemetry/connectionStatus{false}',
            'telemetry/event{Server fatal exception}',
            'telemetry/event{Version in flowconfig}',
          ],
          [],
        ),
    ]),

    /*
    test('Editor open files outlive server', [
      lspStart({needsFlowServer: true, doInitialize: true}),
      lspSend('didOpen file'),
      flowCmd(['stop']).serverExpectStatus({running: false, timeoutMs: 3000}),
      flowCmd(['start']),
      lspExpect('lost server message', '', 3000),
      lspExpect('started server message', '', 3000),
      serverExpectStatus({running: true, timeoutMs: 0}),
      lspSend('definition, line, col').lspExpect(
        'definition',
        'response',
        3000,
      ),
    ]),
    test('Editor modified files outlive server', [
      lspStart({needsFlowServer: true, doInitialize: true}),
      lspSend('didOpen file'),
      lspSend('didChange file, range, contents'),
      flowCmd(['stop']).serverExpectStatus({running: false, timeoutMs: 3000}),
      flowCmd(['start']),
      lspExpect('lost server message', '', 3000),
      lspExpect('started server message', '', 3000),
      lspSend('definition, line, col').lspExpect(
        'definition',
        'response',
        3000,
      ),
    ]),
    */
  ],
);
