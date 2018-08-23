/**
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import simpleDiffAssertion from './simpleDiffAssertion';

import type {AssertionLocation, ErrorAssertion} from './assertionTypes';
import type {IDEMessage} from '../ide';

export default function(
  timeoutMs: number,
  expected: $ReadOnlyArray<IDEMessage>,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env) => {
    const actual = env.getIDEMessagesSinceStartOfStep();

    let suggestion = {
      method: 'waitAndVerifyNoIDEMessagesSinceStartOfStep',
      args: [Math.round(timeoutMs / 10)],
    };
    if (actual.length > 0) {
      suggestion = {
        method: 'waitAndVerifyAllIDEMessagesContentSinceStartOfStep',
        args: [timeoutMs, actual],
      };
    }
    return simpleDiffAssertion(
      JSON.stringify(expected, null, 2),
      JSON.stringify(actual, null, 2),
      assertLoc,
      reason,
      'new ide messages',
      suggestion,
    );
  };
}
