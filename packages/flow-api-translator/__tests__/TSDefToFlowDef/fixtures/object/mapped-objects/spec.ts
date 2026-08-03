/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

export type FlattenTokens<
  T extends {
    [k: string]:
      string | {readonly default: string; readonly [k: string]: string};
  },
> = {
  readonly [Key in keyof T]: T[Key] extends {
    readonly default: infer X;
    readonly [l: string]: infer Y;
  }
    ? X | Y
    : T[Key];
};

type ObjectEntries<Obj extends {readonly [k: string]: unknown}> = {
  [Key in keyof Obj]?: [Key, Obj[Key]];
};
