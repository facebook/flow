/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export type Money = {
  amount: number,
  currency: string,
};

export function add(a: Money, b: Money): Money {
  if (a.currency !== b.currency) {
    throw new Error('currency mismatch');
  }
  return {amount: a.amount + b.amount, currency: a.currency};
}

export function format(money: Money): string {
  return `${money.amount.toFixed(2)} ${money.currency}`;
}
