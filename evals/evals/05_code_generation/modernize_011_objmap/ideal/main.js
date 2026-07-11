/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type FormValues = {
  name: string,
  age: number,
  subscribed: boolean,
};

type FieldHistory = {[K in keyof FormValues]: Array<FormValues[K]>};

export function latestName(history: FieldHistory): string {
  return history.name[history.name.length - 1];
}
