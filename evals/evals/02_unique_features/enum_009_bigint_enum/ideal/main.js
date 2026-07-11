/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum StorageTier {
  Standard = 1099511627776n,
  Premium = 1125899906842624n,
  Unlimited = 1152921504606846976n,
}

export function parseTier(input: bigint): StorageTier {
  return StorageTier.cast(input) ?? StorageTier.Standard;
}

export function quotaBytes(tier: StorageTier): bigint {
  return tier as bigint;
}

export function allowsArchival(tier: StorageTier): boolean {
  return tier === StorageTier.Premium || tier === StorageTier.Unlimited;
}
