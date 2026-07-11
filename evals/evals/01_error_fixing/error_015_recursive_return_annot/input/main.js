/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

function isEven(n: number) {
  if (n === 0) {
    return true;
  }
  return isOdd(n - 1);
}

function isOdd(n: number) {
  if (n === 0) {
    return false;
  }
  return isEven(n - 1);
}

const result: boolean = isEven(10);
