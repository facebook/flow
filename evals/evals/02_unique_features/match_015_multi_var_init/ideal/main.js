/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Theme = 'light' | 'dark' | 'system';

export function applyTheme(theme: Theme): string {
  const {bg, fg, accent} = match (theme) {
    'light' => {bg: '#ffffff', fg: '#000000', accent: '#0066cc'},
    'dark' => {bg: '#1a1a2e', fg: '#e0e0e0', accent: '#4da6ff'},
    'system' => {bg: '#f5f5f5', fg: '#333333', accent: '#3399ff'},
  };
  return `background: ${bg}; color: ${fg}; border-color: ${accent}`;
}
