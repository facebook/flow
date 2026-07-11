/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export opaque type Cents = number;

export function fromDollars(dollars: number): Cents {
  return Math.round(dollars * 100);
}

export function add(a: Cents, b: Cents): Cents {
  return a + b;
}

export function scale(amount: Cents, factor: number): Cents {
  return Math.round(amount * factor);
}

export function format(amount: Cents): string {
  return '$' + (amount / 100).toFixed(2);
}
