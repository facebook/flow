/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type FormField = {
  name: string,
  value: unknown,
  required: boolean,
};

function getDisplayValue(field: FormField): string {
  const value = field.value;
  if (typeof value === 'string') {
    return field.name + ': ' + value;
  } else if (typeof value === 'number') {
    return field.name + ': ' + String(value);
  } else if (typeof value === 'boolean') {
    return field.name + ': ' + String(value);
  }
  return field.name;
}

function sumNumericFields(fields: Array<FormField>): number {
  let total = 0;
  for (const field of fields) {
    if (typeof field.value === 'number') {
      total += field.value;
    }
  }
  return total;
}

function isFieldEmpty(field: FormField): boolean {
  const value = field.value;
  if (value == null) {
    return true;
  }
  if (typeof value === 'string' && value.length === 0) {
    return true;
  }
  return false;
}

export {getDisplayValue, sumNumericFields, isFieldEmpty};
