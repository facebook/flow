type T0 = {a: 0} | {a: 1, b: void};

// Basic
{
  declare const x: T0;
  if ('b' in x) {
    x as empty; // ERROR
    x as {a: 1, b: void}; // OK
  } else {
    x as empty; // ERROR
    x as {a: 0}; // OK
  }
}

{
  declare const x: T0;
  if ('a' in x) {
    x as empty; // ERROR
    x as T0; // OK
  } else {
    x as empty; // OK
  }
}

// Object proto
{
  declare const x: T0 | {__proto__: null, a: 2};
  if ('toString' in x) {
    x as empty; // ERROR
    x as T0; // OK
  } else {
    x.a as 2; // OK
  }
}

// Function proto
{
  declare const x: {(): boolean, a: 2};
  if ('call' in x) {
    x as empty; // ERROR
    x.a as 2; // OK
  } else {
    x as empty; // OK
  }

  // Object proto is still proto of function proto
  if ('toString' in x) {
    x as empty; // ERROR
    x.a as 2; // OK
  } else {
    x as empty; // OK
  }
}

// Custom proto
{
  declare const x: T0 | {a: 2, __proto__: {c: void}};
  if ('c' in x) {
    x as empty; // ERROR
    x.a as 2; // OK
  } else {
    x as T0; // OK
  }
}

// Proto is union
{
  // Key exists in every member of the union: matches
  declare const x: T0 | {a: 2, __proto__: {c: void} | {c: boolean}};
  if ('c' in x) {
    x as empty; // ERROR
    x.a as 2; // OK
  } else {
    x as T0; // OK
  }
}
{
  // Key doesn't exist in every member of the union: no match
  declare const x: T0 | {a: 2, __proto__: {c: void} | {xxx: void}};
  if ('c' in x) {
    x as empty; // OK
  }
}

// Inexact objects - can have additional properties
{
  declare const x: {a: 0, ...} | {a: 1, b: void, ...};

  if ('b' in x) {
    x as {a: 1, b: void, ...}; // ERROR: could exist in inexact object
  } else {
    x as empty; // ERROR
    x as {a: 0, ...}; // OK
  }
}

// Intersection is refined
{
  // Key exists in at least one member of the intersection
  declare const x: {a: 0, ...} & {xxx: void, ...} | {a: 1};
  if ('a' in x) {
    x as empty; // ERROR
    x.a as 0 | 1; // OK
  } else {
    x as empty; // OK
  }
}

// Optional properties
{
  declare const x: {a: 0} | {a: 1, b?: boolean};
  if ('b' in x) {
    x as empty; // ERROR
    x.a as 1; // OK
  } else {
    x as empty; // ERROR
    x.a as 0; // ERROR: is optional so unknown
    x.a as 0 | 1; // OK
  }
}

// Instances/interfaces - can have additional props like inexact objects
{
  declare const x: interface {a: 0} | interface {a: 1, b: void};
  if ('b' in x) {
    x as interface {a: 1, b: void}; // ERROR: could exist
  } else {
    x as empty; // ERROR
    x as interface {a: 0}; // OK
  }
}
{
  declare const x: interface {m(): void};
  if ('m' in x) {
    x as empty; // ERROR
    x as interface {m(): void}; // OK
  } else {
    x as empty; // OK
  }
}
{
  class B {a: 0}
  class A extends B {}
  declare const x: A;
  if ('a' in x) {
    x as empty; // ERROR
    x as A; // OK
  } else {
    x as empty; // OK
  }
}

// We don't refine arrays/tuples with `in` since it's not useful and we don't
// handle array holes
{
  declare const x: ['a'] | ['a', 'b'];
  if (1 in x) {
    x as ['a'] | ['a', 'b']; // OK: don't refine
  } else {
    x as ['a'] | ['a', 'b']; // OK: don't refine
  }
}
