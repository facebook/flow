/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Plan = 'free' | 'pro' | 'enterprise';

export function monthlyCost(plan: Plan, seats: number): number {
  let perSeat = 0;
  switch (plan) {
    case 'free':
      perSeat = 0;
      break;
    case 'pro':
      perSeat = 12;
      break;
    case 'enterprise':
      perSeat = 8;
      break;
  }
  return perSeat * seats;
}
