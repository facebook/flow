/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

function alwaysTrue(value: unknown): value is string {
  return true;
}

function alwaysFalse(value: unknown): value is string {
  return false;
}

const wrongNarrowing = (value: unknown): value is string =>
  typeof value === 'number';

function reassignedGuard(value: unknown): value is string {
  value = 1;
  return typeof value === 'string';
}

function havocedGuard(value: unknown): value is string {
  const reset = () => { value = 1; };
  reset();
  return typeof value === 'string';
}

function emptyAssertion(value: unknown): asserts value is string {}

function bareAssertion(value: unknown): asserts value {}

const arrowAssertion = (value: unknown): asserts value is string => undefined;

function earlyReturnAssertion(value: unknown, skip: boolean): asserts value is string {
  if (skip) {
    return;
  }
  if (typeof value === 'string') {
    throw new Error();
  }
}

function reassignedAssertion(value: unknown): asserts value is string {
  value = 1;
}

function havocedAssertion(value: unknown): asserts value is string {
  const reset = () => { value = 1; };
  reset();
  value;
}

class Receiver {
  isString(): this is this & { value: string } {
    return true;
  }

  assertString(): asserts this is this & { value: string } {}
}

function narrowedByGuard(value: unknown) {
  if (alwaysTrue(value)) {
    const good: string = value;
    const bad: number = value; // ERROR
  }
}

function narrowedByAssertion(value: unknown) {
  emptyAssertion(value);
  const good: string = value;
  const bad: number = value; // ERROR
}

function incompatibleGuard(value: number): value is string { // ERROR
  return true;
}

function incompatibleAssertion(value: number): asserts value is string {} // ERROR

function nonBooleanReturn(value: unknown): value is string {
  return 'yes'; // ERROR
}

function nonVoidReturn(value: unknown): asserts value is string {
  return true; // ERROR
}

function unboundGuard(value: unknown): missing is string { // ERROR
  return true;
}

function unboundAssertion(value: unknown): asserts missing is string {} // ERROR
