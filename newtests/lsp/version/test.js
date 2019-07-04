/*
 * @flow
 * @format
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    ideStart,
    ideRequest,
    ideNotification,
    lspInitializeParams,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('initialize error for wrong version', [
      ideStart({mode: 'lsp', needsFlowServer: false}),
      ideRequest('initialize', lspInitializeParams)
        .waitUntilIDEMessage(30000, 'initialize')
        .verifyAllIDEMessagesInStep(
          ['initialize{Wrong version of Flow. The config specifies}'],
          [...lspIgnoreStatusAndCancellation],
        ),
      ideRequest('shutdown')
        .waitUntilIDEMessage(10000, 'shutdown')
        .verifyAllIDEMessagesInStep(
          ['shutdown'],
          [...lspIgnoreStatusAndCancellation],
        ),
      ideNotification('exit')
        .waitUntilIDEStatus(10000, 'stopped')
        .waitUntilServerStatus(10000, 'stopped')
        .verifyIDEStatus('stopped')
        .verifyServerStatus('stopped'),
    ]),
  ],
);
