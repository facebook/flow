/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {difference, prettyPrint} = require('../../flowResult');

const {default: simpleDiffAssertion} = require('./simpleDiffAssertion');

import type {AssertionLocation, ErrorAssertion} from './assertionTypes';

function newErrors(
  expected: string,
  assertLoc: ?AssertionLocation,
): ErrorAssertion {
  return (reason: ?string, env) => {
    const diff = difference(env.getNewErrors(), env.getOldErrors());
    const actual = prettyPrint(diff);

    let suggestion = {method: 'noNewErrors', args: []};
    if (!diff.passed) {
      suggestion = {
        method: 'newErrors',
        args: [prettyPrint(diff)],
      };
    }
    return simpleDiffAssertion(
      expected,
      actual,
      assertLoc,
      reason,
      'new errors',
      suggestion,
    );
  };
}

module.exports = {
  default: newErrors,
};
