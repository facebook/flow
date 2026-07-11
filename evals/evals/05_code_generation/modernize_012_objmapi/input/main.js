/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Fields = {
  title: string,
  count: number,
};

type LabeledFields = $ObjMapi<Fields, <K, V>(K, V) => [K, V]>;

export function titleKey(labeled: LabeledFields): string {
  return labeled.title[0];
}
