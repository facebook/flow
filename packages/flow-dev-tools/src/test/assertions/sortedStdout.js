/**
 * @flow
 * @format
 */

import simpleDiffAssertion from './simpleDiffAssertion';

import type {AssertionLocation, ErrorAssertion} from './assertionTypes';

export default function(
  expected: string,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env) => {
    const actual = env
      .getStdout()
      .split('\n')
      .filter(line => line !== '') // Whitespace in a sorted stdout isn't useful
      .sort()
      .join('\n');
    const suggestion = {method: 'sortedStdout', args: [actual]};
    return simpleDiffAssertion(
      expected,
      actual,
      assertLoc,
      reason,
      'sortedStdout',
      suggestion,
    );
  };
}
