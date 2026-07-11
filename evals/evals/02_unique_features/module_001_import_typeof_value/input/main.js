/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

// TODO: define `Formatter` so it has the same type as the value exported by
// `numberFormat.js`, without pulling that value into this module at runtime.

const registry: Array<Formatter> = [];

export function registerFormatter(formatter: Formatter): void {
  registry.push(formatter);
}

export function formatAll(value: number): Array<string> {
  return registry.map(format => format(value, 2));
}
