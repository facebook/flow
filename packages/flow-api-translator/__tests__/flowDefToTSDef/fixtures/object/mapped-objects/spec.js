/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

export type FlattenTokens<
  T: {
    +[string]: string | { +default: string, +[string]: string },
  },
> = {
  +[Key in keyof T]: T[Key] extends { +default: infer X, +[string]: infer Y }
    ? X | Y
    : T[Key],
};

type ObjectEntries<Obj: { +[string]: mixed }> = {
  [Key in $Keys<Obj>]?: [Key, Obj[Key]],
};
