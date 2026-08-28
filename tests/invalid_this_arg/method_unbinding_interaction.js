// Reading a class method preserves both its receiver type and receiver
// expression, so `.call` may only re-supply that exact receiver.

class A {
  m(): void {}
}

declare const a: A;

a.m.call(a); // ok
a.m.call(new A()); // error: receiver mismatch

// Method shorthand in an object type has an unannotated, trivial `this`, so
// nothing is reported.
type Shorthand = {x: number, m(y: number): void};
declare const s: Shorthand;
declare const other: Shorthand;

s.m.call(other, 1); // ok - `m` accepts any receiver
s.m.bind(other); // ok - `m` accepts any receiver

// Spelling the receiver out brings the check back.
type Annotated = {x: number, m(this: {x: number, ...}, y: number): void};
declare const t: Annotated;
declare const u: Annotated;

t.m.call(u, 1); // error: receiver mismatch

class Static {
  static x: number = 0;
  static m(this: typeof Static, y: number): void {}
}

Static.m.call(Static, 1); // ok
Static.m.call(a, 1); // error: receiver mismatch
