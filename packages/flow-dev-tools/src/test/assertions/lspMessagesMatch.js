/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import type {ErrorAssertionResult} from './assertionTypes';
import type {LSPMessage} from '../lsp';
import type {AssertionLocation, AssertionMethod} from './assertionTypes';

const {Builder} = require('../builder');
const {default: simpleDiffAssertion} = require('./simpleDiffAssertion');

function lspMessagesMatch(
  actualMessages: Array<LSPMessage>,
  expects: $ReadOnlyArray<string | [string, string] | LSPMessage>,
  ignores: $ReadOnlyArray<string | [string, string] | LSPMessage>,
  reason: ?string,
  assertionMethod: AssertionMethod,
  assertLoc: ?AssertionLocation,
): ErrorAssertionResult {
  let actuals: Array<string | [string, string] | LSPMessage> = [];
  let iExpect = 0;
  // we test that messages are equal using a diff of strings, so we have
  // to convert the expected value into a string.
  let diffable = (expected: LSPMessage | string | [string, string]) => {
    if (typeof expected === 'string') {
      return expected;
    } else if (Array.isArray(expected)) {
      return expected.join(',');
    } else {
      return JSON.stringify(expected, null, 2);
    }
  };
  let doesMatch = (
    actual: LSPMessage,
    expected: string | [string, string] | LSPMessage,
  ) => {
    if (typeof expected === 'string') {
      return Builder.doesMessageFuzzyMatch(actual, expected);
    } else if (Array.isArray(expected) && expected.length === 2) {
      return Builder.doesMessageFuzzyMatch(actual, expected[0], expected[1]);
    } else {
      return Builder.doesMessageMatch(actual, expected);
    }
  };
  for (let iActual = 0; iActual < actualMessages.length; iActual++) {
    let actual = actualMessages[iActual];
    let expected = expects[iExpect];
    if (expected !== undefined && doesMatch(actual, expected)) {
      // it matches (possibly fuzzily), so we add the *expected* output
      // to *actuals*, so that when we string diff it later, it matches.
      // otherwise, the fuzzy expectation wouldn't match later.
      actuals.push(expected);
      iExpect++;
    } else if (ignores.some(ignore => doesMatch(actual, ignore))) {
      // ignore it
    } else {
      actuals.push(actual);
    }
  }

  const suggestion = {
    method: assertionMethod,
    args: [actuals, ignores],
  };

  // don't ignore whitespace, since we used JSON.stringify to format
  // both `expects` and `actuals`, whitespace is already normalized
  // and differences are relevant (like within strings).
  const ignoreWhitespace = false;

  return simpleDiffAssertion(
    expects.map(diffable).join('\n'),
    actuals.map(diffable).join('\n'),
    assertLoc,
    reason,
    'messages',
    suggestion,
    ignoreWhitespace,
  );
}

module.exports = {
  default: lspMessagesMatch,
};
