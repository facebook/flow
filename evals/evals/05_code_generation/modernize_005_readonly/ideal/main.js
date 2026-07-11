/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Settings = {
  theme: string,
  fontSize: number,
};

type FrozenSettings = Readonly<Settings>;

export function describe(settings: FrozenSettings): string {
  return settings.theme + ' @ ' + String(settings.fontSize) + 'px';
}
