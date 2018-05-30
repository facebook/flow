/*
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({ideStart, ideRequest, ideNotification, lspInitializeParams}) => [
    test('initialize error for wrong version', [
      ideStart({mode: 'lsp', needsFlowServer: false}),
      ideRequest('initialize', lspInitializeParams)
        .waitUntilIDEMessage(10000, 'initialize')
        .verifyAllIDEMessagesInStep(
          ['initialize{Wrong version of Flow. The config specifies}'],
          [],
        ),
      ideRequest('shutdown')
        .waitUntilIDEMessage(3000, 'shutdown')
        .verifyAllIDEMessagesInStep(['shutdown'], []),
      ideNotification('exit')
        .waitUntilIDEStatus(3000, 'stopped')
        .waitUntilServerStatus(3000, 'stopped')
        .verifyIDEStatus('stopped')
        .verifyServerStatus('stopped'),
    ]),
  ],
);
