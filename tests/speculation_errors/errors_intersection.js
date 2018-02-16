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
((any: {+b: null, +a: null, +c: null}): {+b: 2, +a: 1, +c: 3});

// Error: Group should list three errors in the order: `a`, `b`, `c`
((any: {
  +b: null & string,
  +a: null & string,
  +c: null & string,
}): {+b: 2, +a: 1, +c: 3});

// Error: Group should list three errors in the order: `a`, `b`, `c`
((any: {+b: null, +a: null & string, +c: null}): {+b: 2, +a: 1, +c: 3});

// Error: Group should list three errors in the order: `b`, `a`, `c`
((any: {+b: null & string, +a: null, +c: null}): {+b: 2, +a: 1, +c: 3});

// Error: Group should list three errors in the order: `c`, `b`, `a`
((any: {+b: null, +a: null, +c: null & string}): {+b: 2, +a: 1, +c: 3});

// Error: number ~> null
((any: {+a: {+b: null}}): {+a: {+b: 42}});

// Error: number ~> null. Because of union error scoring we should only see
// one error.
((any: {+a: null & {+b: null & {}}}): {+a: {+b: 42}});

((any: null): 42); // Error: number ~> null
((any: {} & {} & {} & null): 42); // Error: number ~> null
((any: {} & ({} & ({} & null))): 42); // Error: number ~> null

((any: number & string): true); // Error
((any: number & string & {}): true); // Error: should not show the {} branch
((any: {} & number & string): true); // Error: should not show the {} branch
((any: number & {} & string): true); // Error: should not show the {} branch

((any: {+a: number & string}): {+a: true}); // Error

((any: {
  +a: number & string,
  +b: {} & number & string,
  +c: number & {} & string,
  +d: number & string & {},
}): {
  +a: true, // Error: should be grouped
  +b: true, // Error: should be grouped, should not show the {} branch
  +c: true, // Error: should be grouped, should not show the {} branch
  +d: true, // Error: should be grouped, should not show the {} branch
});

// Demonstrates use_op ~> union speculation ~> use_op
((any: {
  +a: [number] & [string],
  +b: [{}] & [number] & [string],
  +c: [number] & [{}] & [string],
  +d: [number] & [string] & [{}],
}): {
  +a: [true], // Error: should be grouped
  +b: [true], // Error: should be grouped, should not show the [{}] branch
  +c: [true], // Error: should be grouped, should not show the [{}] branch
  +d: [true], // Error: should be grouped, should not show the [{}] branch
});

((any: number & (string & null)): true); // Error: should be flattened
((any: (string & null) & number): true); // Error: should be flattened
((any: {} & number & (string & null)): true); // Error: should be flattened
((any: number & (string & null) & {}): true); // Error: should be flattened
((any: number & (string & {} & null)): true); // Error: should be flattened

((any: number & number & number & string): true); // Error: should be flattened
((any: number & number & (number & string)): true); // Error: should be flattened
((any: number & (number & (number & string))): true); // Error: should be flattened
((any: (string & number) & number & number): true); // Error: should be flattened
((any: ((string & number) & number) & number): true); // Error: should be flattened

((any: number & {}): true); // Error
((any: {+a: number & {}, +b: number & {}}): {+a: true, +b: true}); // Error
((any: {+a: number & string & {}, +b: number & {}}): {+a: true, +b: true}); // Error

// Error: union inside union fun.
((any: [[null, A] & [null, B]] & [[null, C] & [null, D]]): [[null, number]]);

// Error: union inside union fun, but thanks to scoring we only show three.
((any: [[null, A] & {}] & [[null, C] & [null, D]]): [[null, number]]);

// Error: union inside union fun, but thanks to scoring we only show two.
((any: [[null, A] & {}] & [[null, C] & {}]): [[null, number]]);

// Error: union inside union fun, but thanks to scoring we only show one.
((any: {} & [[null, C] & {}]): [[null, number]]);

declare var f1: A | B;
f1(); // Error

declare var f2: A | (B | C);
f2(); // Error

declare var f3: A | (B | (C | D));
f3(); // Error
