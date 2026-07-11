/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

const PALETTE = {
  primary: '#0866ff',
  danger: '#fa383e',
  muted: '#65676b',
} as const;

type ColorName = keyof typeof PALETTE;
type ColorValue = (typeof PALETTE)[ColorName];

function colorOf(name: ColorName): ColorValue {
  return PALETTE[name];
}

const SIZES = [8, 16, 24] as const;
type Size = (typeof SIZES)[number];

function pad(size: Size): string {
  return `${size}px`;
}

const direct: '#0866ff' = PALETTE.primary;
const current: ColorValue = colorOf('primary');
const gap: Size = 16;

console.log(direct, current, pad(gap), SIZES.length);
