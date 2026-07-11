/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type FeatureFlags = {
  darkMode: boolean,
  betaBanner: boolean,
  newCheckout: boolean,
};

type FeatureName = keyof FeatureFlags;

export function isEnabled(flags: FeatureFlags, name: FeatureName): boolean {
  return flags[name];
}
