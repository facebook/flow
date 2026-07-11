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

export function evaluateExpr(expr: Expr): number {
  return match (expr) {
    ['num', const n] => n,
    ['add', const left, const right] => evaluateExpr(left) + evaluateExpr(right),
    ['mul', const left, const right] => evaluateExpr(left) * evaluateExpr(right),
    ['neg', const inner] => -evaluateExpr(inner),
  };
}

