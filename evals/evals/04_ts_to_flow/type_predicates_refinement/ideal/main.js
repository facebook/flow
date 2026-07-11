/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

function isString(value: unknown): value is string {
  return typeof value === 'string';
}

function isNumber(value: unknown): value is number {
  return typeof value === 'number';
}

type Partitioned = {
  strings: Array<string>,
  numbers: Array<number>,
  other: number,
};

function partition(values: Array<unknown>): Partitioned {
  const strings = values.filter(isString);
  const numbers = values.filter(isNumber);
  const other = values.length - strings.length - numbers.length;
  return {strings, numbers, other};
}

function joinStrings(values: Array<unknown>, separator: string): string {
  return values.filter(isString).join(separator);
}

const mixed: Array<unknown> = ['a', 1, 'b', true, 2, null, 'c'];
const result = partition(mixed);

console.log(result.strings, result.numbers, result.other);
console.log(joinStrings(mixed, ', '));
