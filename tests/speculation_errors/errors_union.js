/**
 * @format
 * @flow
 */

declare var any: any;
declare opaque type A;
declare opaque type B;
declare opaque type C;
declare opaque type D;

// Error: Group should list three errors in the order: `b`, `a`, `c`
({b: 2, a: 1, c: 3}) as {a: boolean, b: boolean, c: boolean};

// Error: Group should list three errors in the order: `b`, `a`, `c`
({b: 2, a: 1, c: 3}) as {
  a: boolean | string,
  b: boolean | string,
  c: boolean | string,
};

// Error: Group should list three errors in the order: `b`, `a`, `c`
({b: 2, a: 1, c: 3}) as {a: boolean | string, b: boolean, c: boolean};

// Error: Group should list three errors in the order: `b`, `a`, `c`
({b: 2, a: 1, c: 3}) as {a: boolean, b: boolean | string, c: boolean};

// Error: Group should list three errors in the order: `b`, `a`, `c`
({b: 2, a: 1, c: 3}) as {a: boolean, b: boolean, c: boolean | string};

// Error: number ~> boolean
({a: {b: 42}}) as {a: {b: boolean}};

// Error: number ~> boolean. Because of union error scoring we should only see
// one error.
({a: {b: 42}}) as {a: boolean | {b: boolean | {}}};

42 as boolean; // Error: number ~> boolean
42 as {} | {} | {} | boolean; // Error: number ~> boolean
42 as {} | ({} | ({} | boolean)); // Error: number ~> boolean

// Alias Example 1
({x: 123, y: 'abc'}) as {x: number, y: number} | {x: string, y: string}; // Error

// Alias Example 2
type NumberPoint = {x: number, y: number};
type StringPoint = {x: string, y: string};
({x: 123, y: 'abc'}) as number | NumberPoint | StringPoint; // Error

// Alias Example 3
type Point = NumberPoint | StringPoint;
({x: 123, y: 'abc'}) as Point; // Error

true as number | string; // Error
true as number | string | {}; // Error: should not show the {} branch
true as {} | number | string; // Error: should not show the {} branch
true as number | {} | string; // Error: should not show the {} branch

({a: true}) as {a: number | string}; // Error

({
  a: true, // Error: should be grouped
  b: true, // Error: should be grouped, should not show the {} branch
  c: true, // Error: should be grouped, should not show the {} branch
  d: true, // Error: should be grouped, should not show the {} branch
}) as {
  a: number | string,
  b: {} | number | string,
  c: number | {} | string,
  d: number | string | {},
};

// Demonstrates use_op ~> union speculation ~> use_op
({
  a: [true], // Error: should be grouped
  b: [true], // Error: should be grouped, should not show the [{}] branch
  c: [true], // Error: should be grouped, should not show the [{}] branch
  d: [true], // Error: should be grouped, should not show the [{}] branch
}) as {
  a: [number] | [string],
  b: [{}] | [number] | [string],
  c: [number] | [{}] | [string],
  d: [number] | [string] | [{}],
};

true as number | (string | false); // Error: should be flattened
true as (string | false) | number; // Error: should be flattened
true as {} | number | (string | false); // Error: should be flattened
true as number | (string | false) | {}; // Error: should be flattened
true as number | (string | {} | false); // Error: should be flattened

true as number | number | number | string; // Error: should be flattened
true as number | number | (number | string); // Error: should be flattened
true as number | (number | (number | string)); // Error: should be flattened
true as (string | number) | number | number; // Error: should be flattened
true as ((string | number) | number) | number; // Error: should be flattened

true as number | {}; // Error
({a: true, b: true}) as {a: number | {}, b: number | {}}; // Error
({a: true, b: true}) as {a: number | string | {}, b: number | {}}; // Error

// Error: union inside union fun.
any as [[null, number]] as [[null, A] | [null, B]] | [[null, C] | [null, D]];

// Error: union inside union fun, but thanks to scoring we only show three.
any as [[null, number]] as [[null, A] | {}] | [[null, C] | [null, D]];

// Error: union inside union fun, but thanks to scoring we only show two.
any as [[null, number]] as [[null, A] | {}] | [[null, C] | {}];

// Error: union inside union fun, but thanks to scoring we only show one.
any as [[null, number]] as {} | [[null, C] | {}];
