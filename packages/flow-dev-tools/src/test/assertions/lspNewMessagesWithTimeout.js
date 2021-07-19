/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import simpleDiffAssertion from './simpleDiffAssertion';

import type {AssertionLocation, ErrorAssertion} from './assertionTypes';
import type {LSPMessage} from '../lsp';

export default function(
  timeoutMs: number,
  expected: $ReadOnlyArray<LSPMessage>,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env) => {
    const actual = env.getLSPMessagesSinceStartOfStep();

    let suggestion = {
      method: 'waitAndVerifyNoLSPMessagesSinceStartOfStep',
      args: [Math.round(timeoutMs / 10)],
    };
    if (actual.length > 0) {
      suggestion = {
        method: 'waitAndVerifyAllLSPMessagesContentSinceStartOfStep',
        args: [timeoutMs, actual],
      };
    }
    return simpleDiffAssertion(
      JSON.stringify(expected, null, 2),
      JSON.stringify(actual, null, 2),
      assertLoc,
      reason,
      'new lsp messages',
      suggestion,
    );
  };
}
