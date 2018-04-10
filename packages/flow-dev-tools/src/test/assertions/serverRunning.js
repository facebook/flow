/**
 * @flow
 * @format
 * @lint-ignore-every LINEWRAP1
 */

import simpleDiffAssertion from './simpleDiffAssertion';

import type {AssertionLocation, ErrorAssertion} from './assertionTypes';

export default function(
  expected: boolean,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env) => {
    const actual = env.getServerRunning();
    const suggestion = {method: 'serverRunning', args: [actual]};
    return simpleDiffAssertion(
      String(expected),
      String(actual),
      assertLoc,
      reason,
      "'server is running'",
      suggestion,
    );
  };
}
