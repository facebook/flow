/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

function sumValues(dict: Map<string, number>, keys: Array<string>): number {
  let total = 0;
  for (const key of keys) {
    const value = dict.get(key);
    if (value != null) {
      total += value;
    }
  }
  return total;
}

function getOrDefault(dict: Map<string, number>, key: string, defaultVal: number): number {
  const value = dict.get(key);
  return value != null ? value : defaultVal;
}

function maxValue(dict: Map<string, number>): number {
  let max = -Infinity;
  for (const [key, value] of dict) {
    if (value > max) {
      max = value;
    }
  }
  return max;
}
