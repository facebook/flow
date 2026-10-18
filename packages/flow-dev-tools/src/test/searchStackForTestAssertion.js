/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {dirname} = require('path');

import type {AssertionLocation} from './assertions/assertionTypes';

type CallSite = {
  getFileName(): string | null,
  getLineNumber(): number,
  getColumnNumber(): number,
};

function searchStackForTestAssertion(): ?AssertionLocation {
  const oldPrepareStackTrace = Error.prepareStackTrace;
  Error.prepareStackTrace = (_, stack) => stack;
  const stack: Array<CallSite> = new Error().stack as any;
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
