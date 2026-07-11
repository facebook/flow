/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

class PercentOff {
  percent: number;
  constructor(percent: number) {
    this.percent = percent;
  }
}

class AmountOff {
  amount: number;
  constructor(amount: number) {
    this.amount = amount;
  }
}

class BuyOneGetOne {
  constructor() {}
}

type Discount = PercentOff | AmountOff | BuyOneGetOne;

export function applyDiscount(price: number, discount: Discount): number {
  return match (discount) {
    PercentOff {const percent, ...} => price * (1 - percent / 100),
    AmountOff {const amount, ...} => Math.max(0, price - amount),
    BuyOneGetOne {...} => price / 2,
  };
}
