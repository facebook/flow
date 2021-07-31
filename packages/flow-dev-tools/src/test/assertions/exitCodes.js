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

export default function(
  expectedArr: Array<number>,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env) => {
    const actual = JSON.stringify(env.getExitCodes(), null, 2);
    const expected = JSON.stringify(expectedArr, null, 2);
    const suggestion = {method: 'exitCodes', args: [actual]};
    return simpleDiffAssertion(
      expected,
      actual,
      assertLoc,
      reason,
      'exit codes',
      suggestion,
    );
  };
}
