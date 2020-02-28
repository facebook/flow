/*
 * @flow
 * @format
 */

import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default suite(
  ({
    lspStartAndConnect,
    lspStart,
    lspRequest,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('initialize with quickfix support', [
      lspStart({needsFlowServer: false}),
      lspRequestAndWaitUntilResponse(
        'initialize',
        lspInitializeParams,
      ).verifyAllLSPMessagesInStep(
        ['initialize{"codeActionProvider":{"codeActionKinds":["quickfix"]}}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('initialize without quickfix support', [
      lspStart({needsFlowServer: false}),
      lspRequestAndWaitUntilResponse('initialize', {
        ...lspInitializeParams,
        capabilities: {
          ...lspInitializeParams.capabilities,
          textDocument: {
            ...lspInitializeParams.capabilities.textDocument,
            codeAction: {},
          },
        },
      }).verifyAllLSPMessagesInStep(
        ['initialize{"codeActionProvider":false}'],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
  ],
);
