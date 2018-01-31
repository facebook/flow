/* @flow */
import simpleDiffAssertion from './simpleDiffAssertion';

import type {AssertionLocation, ErrorAssertion} from './assertionTypes';
import type {IDEMessage} from '../ide';

export default function(
  timeoutMs: number,
  expected: $ReadOnlyArray<IDEMessage>,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env) => {
    const actual = env.getIDEMessages();

    let suggestion = {
      method: 'ideNoNewMessagesAfterSleep',
      args: [Math.round(timeoutMs / 10)],
    };
    if (actual.length > 0) {
      suggestion = {
        method: 'ideNewMessagesWithTimeout',
        args: [timeoutMs, actual],
      };
    }
    return simpleDiffAssertion(
      JSON.stringify(expected, null, 2),
      JSON.stringify(actual, null, 2),
      assertLoc,
      reason,
      "new ide messages",
      suggestion,
    );
  }
}
