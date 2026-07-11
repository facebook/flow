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

// TODO: Implement
