/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import {add, format} from 'money';
import type {Money} from 'money';

const wallet: Array<Money> = [
  {amount: 10, currency: 'USD'},
  {amount: 5.5, currency: 'USD'},
  {amount: 2.25, currency: 'USD'},
];

const total: Money = wallet.reduce((acc, m) => add(acc, m), {
  amount: 0,
  currency: 'USD',
});

console.log(format(total));
