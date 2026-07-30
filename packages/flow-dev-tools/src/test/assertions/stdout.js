/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {default: simpleDiffAssertion} = require('./simpleDiffAssertion');
const {normalizeOutput} = require('./normalize');

import type {
  AssertionLocation,
  ErrorAssertion,
  NormalizeTag,
  Suggestion,
} from './assertionTypes';

function formatIfJSON(actual: string) {
  try {
    return JSON.stringify(JSON.parse(actual), null, 2);
  } catch (e) {
    return actual;
  }
}

function stdout(
  expected: string,
  assertLoc: ?AssertionLocation,
  normalize?: $ReadOnlyArray<NormalizeTag>,
): ErrorAssertion {
  return (reason: ?string, env) => {
    let actual = formatIfJSON(env.getStdout());
    // The recorded golden `expected` is already normalized, so only the actual output needs it here.
    if (normalize != null) {
      actual = normalizeOutput(normalize, actual, env.getProjectDir());
    }
    expected = formatIfJSON(expected);
    const suggestion: Suggestion =
      normalize == null
        ? {method: 'stdout', args: [actual]}
        : {method: 'stdout', args: [actual, {normalize}]};
    return simpleDiffAssertion(
      expected,
      actual,
      assertLoc,
      reason,
      'stdout',
      suggestion,
    );
  };
}

module.exports = {
  default: stdout,
};
