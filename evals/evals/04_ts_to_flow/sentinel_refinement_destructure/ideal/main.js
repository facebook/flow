/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type FormField =
  | {kind: 'text', value: string}
  | {kind: 'number', value: number};

function serialize(field: FormField): string {
  return match (field) {
    {kind: 'text', const value} => `text:${value.trim()}`,
    {kind: 'number', const value} => `number:${value.toFixed(2)}`,
  };
}

const fields: Array<FormField> = [
  {kind: 'text', value: '  hello  '},
  {kind: 'number', value: 3.14159},
];

for (const field of fields) {
  console.log(serialize(field));
}
