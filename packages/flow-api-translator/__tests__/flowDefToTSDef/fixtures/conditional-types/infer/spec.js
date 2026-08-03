/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type ExtractValues<T: string | { +default: string, +[string]: string }> =
  T[Key] extends { +default: infer X, +[string]: infer Y }
    ? X | Y
    : T[Key];
