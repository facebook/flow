/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type UIElement =
  | {tag: 'button', style: {variant: 'primary' | 'secondary'}, label: string}
  | {tag: 'input', style: {variant: 'outlined' | 'filled'}, placeholder: string}
  | {tag: 'divider'};

export function getClassName(el: UIElement): string {
  return match (el) {
    {tag: 'button', style: {variant: 'primary'}, const label} =>
      `btn-primary-${label}`,
    {tag: 'button', style: {variant: 'secondary'}, const label} =>
      `btn-secondary-${label}`,
    {tag: 'input', style: {variant: 'outlined'}, ...} =>
      'input-outlined',
    {tag: 'input', style: {variant: 'filled'}, ...} =>
      'input-filled',
    {tag: 'divider'} =>
      'divider',
  };
}
