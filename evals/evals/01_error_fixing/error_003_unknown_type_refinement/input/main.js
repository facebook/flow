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
  return field.name + ': ' + field.value;
}

function sumNumericFields(fields: Array<FormField>): number {
  let total = 0;
  for (const field of fields) {
    total += field.value;
  }
  return total;
}

function isFieldEmpty(field: FormField): boolean {
  if (field.value == null) {
    return true;
  }
  if (field.value.length === 0) {
    return true;
  }
  return false;
}

export {getDisplayValue, sumNumericFields, isFieldEmpty};
