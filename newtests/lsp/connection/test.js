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
  }) => [
    test('Warm flow starts up, and server remains running after shutdown', [
      ideStart({mode: 'lsp', needsFlowServer: true}),
      ideRequest('initialize', lspInitializeParams)
        .waitUntilIDEMessage(60000, 'telemetry/connectionStatus')
        .verifyAllIDEMessagesInStep(
          ['initialize', 'telemetry/connectionStatus{true}'],
          ['window/showStatus'],
        ),
      ideRequest('shutdown')
        .waitUntilIDEMessage(20000, 'shutdown')
        .verifyAllIDEMessagesInStep(
          ['shutdown'],
          ['telemetry/connectionStatus', 'window/showStatus'],
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
          ['window/showStatus'],
        ),
      ideRequest('shutdown')
        .waitUntilIDEMessage(20000, 'shutdown')
        .verifyAllIDEMessagesInStep(
          ['shutdown'],
          ['window/showStatus', 'telemetry/connectionStatus'],
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
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      startFlowServer()
        .waitUntilIDEMessage(60000, 'telemetry/connectionStatus')
        // it really can take a while for flow to be ready to connect
        .verifyAllIDEMessagesInStep(
          ['telemetry/connectionStatus{true}'],
          ['window/showStatus'],
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
          ['window/showStatus', '$/cancelRequest'],
        ),
      ideResponse('mostRecent', {title: 'Restart'})
        // .waitUntilServerStatus(60000, 'running') -- commented out because
        // the method currently only waits for servers that the test infrastructure
        // launched; not ones that lspCommand launched.
        .waitUntilIDEMessage(60000, 'telemetry/connectionStatus')
        .verifyAllIDEMessagesInStep(
          ['window/logMessage{Starting}', 'telemetry/connectionStatus{true}'],
          ['window/showStatus', '$/cancelRequest'],
        ),
    ]),

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
          ['window/showStatus', '$/cancelRequest'],
        ),
    ]),

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
          [],
        ),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
        position: {line: 2, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{open.js,"line":1}'],
        [],
      ),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
        position: {line: 2, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{open.js,"line":1}'],
        [],
      ),
      flowCmd(['stop'])
        .waitUntilServerStatus(20000, 'stopped')
        .waitUntilIDEMessage(20000, 'telemetry/connectionStatus{false}')
        .verifyAllIDEMessagesInStep(
          ['telemetry/connectionStatus{false}'],
          [
            'telemetry/event{End_of_file}',
            'window/showStatus',
            '$/cancelRequest',
          ],
        ),
      startFlowServer()
        .waitUntilIDEMessage(60000, 'telemetry/connectionStatus')
        .verifyAllIDEMessagesInStep(
          ['telemetry/connectionStatus{true}'],
          ['window/showStatus', '$/cancelRequest'],
        ),
      ideRequestAndWaitUntilResponse('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL_SLASH>open.js'},
        position: {line: 2, character: 1},
      }).verifyAllIDEMessagesInStep(
        ['textDocument/definition{open.js,line":1}'],
        [],
      ),
    ]),
  ],
);
