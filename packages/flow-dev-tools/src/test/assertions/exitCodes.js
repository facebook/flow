/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {default: simpleDiffAssertion} = require('./simpleDiffAssertion');

import type {AssertionLocation, ErrorAssertion} from './assertionTypes';

function exitCodes(
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

module.exports = {
  default: exitCodes,
};
