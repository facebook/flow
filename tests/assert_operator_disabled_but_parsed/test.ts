/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// TypeScript assertions do not depend on Flow's experimental assertion option.
function values(value: number | null, present: number): void {
  const nullable: number = value!; // OK
  const alreadyPresent: number = present!; // OK
}

// Calls inside optional chains preserve the branch that can short-circuit.
function chains(value: {method: (() => number) | null} | null): void {
  const maybe: number | undefined = value?.method!(); // OK
  const definite: number = (value?.method)!(); // OK
}
