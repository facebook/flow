/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

const DEFAULT_SETTINGS = {
  theme: 'light',
  fontSize: 14,
  highContrast: false,
};

export function describeSettings(settings: typeof DEFAULT_SETTINGS): string {
  return `${settings.theme} theme at ${settings.fontSize}px`;
}
