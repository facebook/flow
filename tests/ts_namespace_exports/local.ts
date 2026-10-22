/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare const implementation: (s: string) => number;
type Local = number;

// Export aliases expose public names without introducing unqualified local bindings.
declare namespace API {
  export { implementation as call, type Local as Item };
  export const value: number;
  export class Member { field: number; }
  export function helper(s: string): number;
  type Unqualified = Item; // ERROR
}

// Exported functions preserve their parameter and return types within the defining file.
API.call('x') satisfies number;
API.call(1); // ERROR
API.call('x') satisfies string; // ERROR
API.helper('x') satisfies number;
API.helper(1); // ERROR

// Qualified type aliases are usable in annotations but have no runtime value.
const item: API.Item = 1;
const badItem: API.Item = 'x'; // ERROR
API.Item; // ERROR

// Ambient value and class exports preserve their member types locally.
API.value satisfies number;
API.value satisfies string; // ERROR
new API.Member().field satisfies number;
new API.Member().field satisfies string; // ERROR

// Export specifiers must resolve to existing value and type bindings.
declare namespace Missing {
  export { unknownValue as call, type UnknownType as Item }; // ERROR
}

// Ambient namespace members cannot have initializers or implementations.
declare namespace Invalid {
  export const initialized: number = 1; // ERROR
  export function implemented(): number { return 1; } // ERROR
}

// A namespace with only type exports supports qualified types without a runtime value.
declare namespace TypeOnly {
  type Local = string;
  export { type Local as Item };
}
const typeItem: TypeOnly.Item = 'x';
const badTypeItem: TypeOnly.Item = 1; // ERROR
TypeOnly; // ERROR
