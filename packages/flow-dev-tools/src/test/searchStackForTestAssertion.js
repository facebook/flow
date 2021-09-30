/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {sync: resolve} = require('resolve');
const {dirname} = require('path');

import type {AssertionLocation} from './assertions/assertionTypes';

function searchStackForTestAssertion(): ?AssertionLocation {
  const oldPrepareStackTrace = Error.prepareStackTrace;
  Error.prepareStackTrace = (_, stack) => stack;
  const stack: Array<Object> = (new Error().stack: any);
  Error.prepareStackTrace = oldPrepareStackTrace;

  for (const callSite of stack) {
    const filename = callSite.getFileName();
    if (filename != null && filename.match(/test.js$/)) {
      return {
        filename,
        line: callSite.getLineNumber(),
        column: callSite.getColumnNumber(),
      };
    }
  }
  return null;
}

module.exports = {
  default: searchStackForTestAssertion,
};
