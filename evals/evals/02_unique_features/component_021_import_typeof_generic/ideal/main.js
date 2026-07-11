/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import typeof * as PickerModule from 'Picker';

export type PickerComponent = PickerModule['default'];

const registry: Array<PickerComponent> = [];

export function register(picker: PickerComponent): void {
  registry.push(picker);
}

export function count(): number {
  return registry.length;
}
