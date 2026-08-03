/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

declare namespace React {
  type ReactNode = string;
  function useState<S>(initialState: S | (() => S)): [S, (s: S) => void];
}
