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

// TODO: Implement
