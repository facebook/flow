/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

const initializeParams = {
  rootPath: '<PLACEHOLDER_PROJECT_DIR>',
  rootUri: '<PLACEHOLDER_PROJECT_URL>',
  capabilities: {
    workspace: {},
    textDocument: {
      synchronization: {},
      completion: {},
      hover: {},
      definition: {},
    },
    window: {progress: {}, actionRequired: {}},
    telemetry: {connectionStatus: {}},
  },
  trace: 'verbose',
};

export default suite(
  ({
    ideStart,
    ideRequest,
    ideNotification,
    lspExpect,
    waitUntilIDEStatus,
    waitUntilServerStatus,
    flowCmd,
    modifyFile,
  }) => [
    test('Warm flow starts up, and server remains running after shutdown', [
      ideStart({mode: 'lsp', needsFlowServer: true, doInitialize: false}),
      ideRequest('initialize', initializeParams)
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
      ideStart({mode: 'lsp', needsFlowServer: false, doInitialize: false}),
      ideRequest('initialize', initializeParams)
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
    /*
    test(
      'Upon termination in-flight, reports, and reports on external restart',
      [
        lspStart({needsFlowServer: true, doInitialize: true}),
        flowCmd(['stop']).lspExpect(
          'message that flow has shut down',
          '',
          3000,
        ),
        flowCmd(['start']).lspExpect('message gets dismissed', '', 3000),
      ],
    ),
    test(
      'Upon termination in-flight, reports, and restarts it upon Restart click',
      [
        lspStart({needsFlowServer: true, doInitialize: true}),
        flowCmd(['stop']).lspExpect('shut down message', '', 3000),
        lspSend('restart click').lspExpect('dismiss message', '', 3000),
        serverExpectStatus({running: true, timeoutMs: 3000}),
      ],
    ),
    test('Restarts a lost server in response to flowconfig change', [
      lspStart({needsFlowServer: true, doInitialize: true}),
      modifyFile({
        filename: '.flowconfig',
        content: '...nonversion_change...',
        watchman: true,
      }),
      lspExpect('lost server message', '', 3000),
      lspExpect('started server message', '', 3000),
      serverExpectStatus({running: true, timeoutMs: 0}),
    ]),
    test('Terminates in response to flowconfig version change', [
      lspStart({needsFlowServer: true, doInitialize: true}),
      modifyFile({
        filename: '.flowconfig',
        content: '...nonversion_change...',
        watchman: true,
      }),
      lspExpect('termination message', '', 3000),
      lspExpectStatus({running: false, timeoutMs: 3000}),
      serverExpectStatus({running: false, timeoutMs: 0}),
    ]),
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
