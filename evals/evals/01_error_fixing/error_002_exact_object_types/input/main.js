/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type Theme = {
  primaryColor: string,
  secondaryColor: string,
  fontSize: number,
};

type ThemeOverrides = {
  primaryColor?: string,
  secondaryColor?: string,
  fontSize?: number,
  fontFamily?: string,
};

function applyThemeOverrides(base: Theme, overrides: ThemeOverrides): Theme {
  return {...base, ...overrides};
}

type UserPreferences = {
  primaryColor?: string,
  fontSize?: number,
  ...
};

function applyUserPreferences(base: Theme, prefs: UserPreferences): Theme {
  return {...base, ...prefs};
}

export {applyThemeOverrides, applyUserPreferences};
