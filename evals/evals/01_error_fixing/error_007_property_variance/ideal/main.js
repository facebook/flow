/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type BaseStyle = {
  color: string,
  fontSize: number,
  ...
};

type BoldStyle = {
  color: string,
  fontSize: number,
  fontWeight: 'bold',
};

type StyledElement = Readonly<{
  style: BaseStyle,
  label: string,
}>;

type BoldElement = Readonly<{
  style: BoldStyle,
  label: string,
}>;

function renderElement(element: StyledElement): string {
  return element.label + ' [color=' + element.style.color + ']';
}

function main(): void {
  const bold: BoldElement = {
    style: {color: 'red', fontSize: 14, fontWeight: 'bold'},
    label: 'Warning',
  };

  renderElement(bold);
}
