// Class methods carry `dummy_static` statics, so `.call` on them resolves on
// `any` rather than on `Function.prototype`. The function-statics dispatch arm
// records those accesses so the check still applies. Reading a class *instance*
// method as a value additionally trips `method-unbinding`, which rewrites the
// `this` parameter to `any` -- so only `method-unbinding` is reported there.

class A {
  m(): void {}
}

declare const a: A;

a.m.call(a); // error: method-unbinding only
a.m.call(new A()); // error: method-unbinding only

// Method shorthand in an object type is not subject to `method-unbinding`, but
// its `this` is unannotated and therefore trivial, so nothing is reported.
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
