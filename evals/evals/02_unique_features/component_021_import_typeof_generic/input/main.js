/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import typeof Picker from 'Picker';

// The type of the component that `Picker.js` exports by default.
export type PickerComponent = Picker;

const registry: Array<PickerComponent> = [];

export function register(picker: PickerComponent): void {
  registry.push(picker);
}

export function count(): number {
  return registry.length;
}
