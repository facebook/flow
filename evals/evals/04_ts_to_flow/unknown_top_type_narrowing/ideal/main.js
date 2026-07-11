/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

function describe(value: unknown): string {
  if (value === null) {
    return 'null';
  }
  if (typeof value === 'string') {
    return `string of length ${value.length}`;
  }
  if (typeof value === 'number') {
    return value.toFixed(2);
  }
  if (typeof value === 'boolean') {
    return value ? 'yes' : 'no';
  }
  if (Array.isArray(value)) {
    return `array of ${value.length}`;
  }
  if (value instanceof Date) {
    return value.toISOString();
  }
  return typeof value;
}

function getName(value: unknown): string {
  if (value !== null && typeof value === 'object' && 'name' in value) {
    const name = value.name;
    if (typeof name === 'string') {
      return name;
    }
  }
  return 'anonymous';
}

const samples: Array<unknown> = [
  'hello',
  42,
  true,
  [1, 2, 3],
  new Date(0),
  {name: 'Ada'},
  null,
];

for (const sample of samples) {
  console.log(describe(sample), getName(sample));
}
