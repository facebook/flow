/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// TypeScript permits assertions on nullable and already non-nullable values.
function values(value: number | null | undefined, present: number): void {
  const numberValue: number = value!; // OK
  const alreadyPresent: number = present!; // OK
  const wrong: string = value!; // ERROR
  const stillNullable: number = value; // ERROR: assertion does not refine value
}

// Calls and member assertions remove null and undefined without opting in.
function members(
  value: {prop: number | null, method: (() => number) | null, child: {prop: number} | null} | null,
  callback: (() => number | null) | null,
): void {
  const prop: number = value!.prop!; // OK
  const comparison: boolean = value?.prop! < 1; // OK
  const repeatedComparison: boolean = value?.prop!! < 1; // OK
  const called: number = callback!()!; // OK
  const chained: number = value?.prop!; // OK
  const method: number | undefined = value?.method!(); // OK
  const missing: number = value?.method!(); // ERROR: optional chain can short-circuit
  const parenthesized: number = (value?.method)!(); // OK
  const repeated: number | undefined = value?.method!!(); // OK
  const repeatedMissing: number = value?.method!!(); // ERROR: chain can short-circuit
  const repeatedParenthesized: number = (value?.method)!!(); // OK
  const optionalAgain: number | undefined = value?.child!?.prop; // OK
  const optionalAgainMissing: number = value?.child!?.prop; // ERROR: original chain can short-circuit
  const optionalParenthesized: number = (value?.child)!?.prop; // OK
}

// Ordinary assertions can target non-nullable properties and assignment LHSs.
function assignments(value: {prop: number | null}): void {
  value.prop! = 1; // OK
  value.prop! += 1; // OK
  value.prop!++; // OK
}

// Newline prefix negation remains JavaScript, not an assertion.
function asi(value: number, other: number): void {
  value
  !other; // OK
  value /*
  */ !other; // OK
}
