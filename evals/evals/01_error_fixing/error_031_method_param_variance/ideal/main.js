/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type Formatter = {
  format(value: string | number): string,
};

function formatCells(
  cells: Array<string | number>,
  formatter: Formatter,
): string {
  return cells.map(cell => formatter.format(cell)).join(' | ');
}

const upperCase: Formatter = {
  format(value: string | number): string {
    return typeof value === 'string'
      ? value.toUpperCase()
      : value.toString();
  },
};

const cells: Array<string | number> = ['name', 42, 'active'];
console.log(formatCells(cells, upperCase));
