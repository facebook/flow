/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare class Empty {}
declare const empty: Empty;

const missing: {first: string; second: number} = empty; // ERROR: both properties are missing
const optional: {maybe?: string} = empty; // OK

declare class Base {
  inherited: number;
}
declare class Derived extends Base {
  own: string;
}
declare const derived: Derived;

const inherited: {inherited: number; own: string} = derived; // OK
const inheritedMissing: {first: string; second: number} = derived; // ERROR: both properties are missing
const partiallyInherited: {inherited: number; first: string; second: number} = derived; // ERROR: only first and second are missing

declare interface Left {
  left: string;
}
declare interface Right {
  right: number;
}
declare interface Both extends Left, Right {}
declare const both: Both;

const intersectionInherited: {left: string; right: number} = both; // OK
const intersectionMissing: {left: string; right: number; first: string; second: number} = both; // ERROR: only first and second are missing

declare class WithSuggestion {
  first: string;
}
declare const withSuggestion: WithSuggestion;

const suggested: {frist: string; second: number} = withSuggestion; // ERROR: preserve suggestion for `frist`

declare const AnyClass: any;
declare class ExtendsAny extends AnyClass {}
declare const extendsAny: ExtendsAny;

const inheritedFromAny: {first: string; second: number} = extendsAny; // OK
