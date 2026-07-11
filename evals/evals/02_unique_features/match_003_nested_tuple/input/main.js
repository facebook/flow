/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Expr =
  | ['num', number]
  | ['add', Expr, Expr]
  | ['mul', Expr, Expr]
  | ['neg', Expr];

// TODO: Implement
