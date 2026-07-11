/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

export enum Theme of number {
  Light = 1,
  Dark = 2,
  HighContrast = 3,
  Solarized = 4,
}

export function validateThemes(inputs: Array<number>): Array<number> {
  return inputs.filter(input => Theme.isValid(input));
}

export const serializeTheme = (theme: ?Theme): number | void =>
  theme?.valueOf();

export function buildConfig(
  themes: Array<Theme>,
): Array<{name: string, value: number}> {
  return themes.map(theme => ({
    name: Theme.getName(theme),
    value: theme.valueOf(),
  }));
}
