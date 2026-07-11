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
  const perSeat = match (plan) {
    'free' => 0,
    'pro' => 12,
    'enterprise' => 8,
  };
  return perSeat * seats;
}
