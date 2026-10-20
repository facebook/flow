/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

export {};

type Assertion = (value: unknown) => asserts value is string;

declare const plainVoid: (value: unknown) => void;
declare const plainNumber: (value: unknown) => number;
declare const guard: (value: unknown) => value is number;
declare const typedAssertion: (value: unknown) => asserts value is number;
declare const bareAssertion: (value: unknown) => asserts value;
declare const incompatibleParam: (value: number) => void;

const fromVoid: Assertion = plainVoid;
const fromNumber: Assertion = plainNumber;
const fromGuard: Assertion = guard;
const fromTypedAssertion: Assertion = typedAssertion;
const fromBareAssertion: Assertion = bareAssertion;

const badParam: Assertion = incompatibleParam; // ERROR
const badGuard: (value: unknown) => value is string = plainVoid; // ERROR
const badGuardType: (value: unknown) => value is string = guard; // ERROR
const badReturn: (value: unknown) => boolean = typedAssertion; // ERROR

function narrowedByAssignedAssertion(value: unknown) {
  fromVoid(value);
  const good: string = value;
  const bad: number = value; // ERROR
}
