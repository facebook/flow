/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

declare const implementation: (s: string) => number;
type Local = number;
declare class Original { field: number; }

// Merging value and type exports into a function preserves its call signature.
declare function API(s: string): number;
declare namespace API {
  export type { Local as Item };
  export { type Local as InlineItem, implementation as call };
  export { Original as Member };
  export const value: number;
  export class Nested { field: number; }
  export function helper(s: string): number;
}
API('x') satisfies number;
API(1); // ERROR
API.call('x') satisfies number;
API.call(1); // ERROR
API.call('x') satisfies string; // ERROR
API.helper('x') satisfies number;
API.helper(1); // ERROR

// Exported type aliases remain qualified types rather than runtime properties.
const item: API.Item = 1;
const inlineItem: API.InlineItem = 1;
const badItem: API.Item = 'x'; // ERROR
const badInlineItem: API.InlineItem = 'x'; // ERROR
API.Item; // ERROR

// Ambient values and aliased and nested classes retain their value and type members.
API.value satisfies number;
API.value satisfies string; // ERROR
new API.Member().field satisfies number;
const member: API.Member = new API.Member();
new API.Nested().field satisfies number;
const nested: API.Nested = new API.Nested();

// A type-only namespace can precede the function it merges into.
declare namespace Before {
  export { type Local as Item };
}
declare function Before(s: string): number;
const beforeItem: Before.Item = 1;
const badBeforeItem: Before.Item = 'x'; // ERROR
Before('x') satisfies number;

// A namespace containing only value aliases also merges into a function.
declare function ValuesOnly(): void;
declare namespace ValuesOnly { export { implementation as call }; }
ValuesOnly.call('x') satisfies number;
ValuesOnly.call(1); // ERROR

// Merged classes expose namespace types alongside their constructor and statics.
declare class ClassAPI { field: number; }
declare namespace ClassAPI {
  export type { Local as Item };
  export { type Local as InlineItem, implementation as call };
  export { Original as Member };
  export const value: number;
}
const classItem: ClassAPI.Item = 1;
const classInlineItem: ClassAPI.InlineItem = 1;
const badClassItem: ClassAPI.Item = 'x'; // ERROR
ClassAPI.Item; // ERROR
const classMember: ClassAPI.Member = new ClassAPI.Member();

// Class instances and namespace statics retain their declared types locally.
new ClassAPI().field satisfies number;
new ClassAPI().field satisfies string; // ERROR
ClassAPI.call('x') satisfies number;
ClassAPI.call(1); // ERROR
ClassAPI.value satisfies number;
ClassAPI.value satisfies string; // ERROR

// A generic runtime class keeps inference and its own statics after namespace merging.
class RuntimeAPI<T> {
  field: T;
  constructor(field: T) { this.field = field; }
  static own(s: string): number { return s.length; }
}
declare namespace RuntimeAPI {
  export { implementation as call };
  export type { Local as Item };
}
new RuntimeAPI('x').field satisfies string;
new RuntimeAPI('x').field satisfies number; // ERROR
RuntimeAPI.own('x') satisfies number;
RuntimeAPI.call('x') satisfies number;
RuntimeAPI.call(1); // ERROR
const runtimeItem: RuntimeAPI.Item = 1;

export { API, ClassAPI, ValuesOnly, RuntimeAPI };
