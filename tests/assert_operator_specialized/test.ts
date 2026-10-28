/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// TypeScript uses unrestricted non-null assertions even in Flow's specialized mode.
function values(value: number | null, present: number): void {
  const nullable: number = value!; // OK
  const alreadyPresent: number = present!; // OK
}

// Assignment assertions in TypeScript are not limited to indexers.
function assignments(value: {prop: number | null}): void {
  value.prop! = 1; // OK
  value.prop! += 1; // OK
  value.prop!++; // OK
}
