/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Cat = {kind: 'cat', meow: () => string};
type Dog = {kind: 'dog', bark: () => string};
type Pet = Cat | Dog;

function isCat(pet: Pet): boolean %checks {
  return pet.kind === 'cat';
}

export function sound(pet: Pet): string {
  return isCat(pet) ? pet.meow() : pet.bark();
}
