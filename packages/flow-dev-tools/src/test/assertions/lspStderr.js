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

function formatIfJSON(actual: string) {
  try {
    return JSON.stringify(JSON.parse(actual), null, 2);
  } catch (e) {
    return actual;
  }
}

function lspStderr(
  expected: string,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env) => {
    const actual = formatIfJSON(env.getLSPStderrSinceStartOfStep());
    expected = formatIfJSON(expected);
    const suggestion = {method: 'lspStderr', args: [formatIfJSON(actual)]};
    return simpleDiffAssertion(
      expected,
      actual,
      assertLoc,
      reason,
      'lspStderr',
      suggestion,
    );
  };
}

module.exports = {
  default: lspStderr,
};
