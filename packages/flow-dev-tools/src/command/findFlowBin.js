/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {existsSync} = require('fs');
const {resolve} = require('path');

const {binOptions} = require('./../constants');

function findFlowBin(binArg: ?string): string {
  // Command line arg wins
  if (binArg != null) {
    return resolve(binArg);
  }

  for (const option of binOptions) {
    if (existsSync(option)) {
      return option;
    }
  }

  // Default to whatever is in the path
  return 'flow';
}

module.exports = {default: findFlowBin};
