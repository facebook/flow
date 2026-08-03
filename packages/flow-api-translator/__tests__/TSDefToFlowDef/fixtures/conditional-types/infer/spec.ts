/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

type ExtractValues<
  T extends string | {readonly default: string; readonly [k: string]: string},
> = T extends {readonly default: infer X; readonly [k: string]: infer Y}
  ? X | Y
  : T;
