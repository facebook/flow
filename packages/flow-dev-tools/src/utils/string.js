/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

function splitIntoChunks(str: string, chunkSize: number): Array<string> {
  let result = [];
  for (let i = 0; i < str.length; i += chunkSize) {
    result.push(str.substr(i, chunkSize));
  }
  return result;
}

module.exports = {splitIntoChunks};
