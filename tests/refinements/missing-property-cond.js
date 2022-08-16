// @flow

function foo1(o: { x: number }) {
  if (o.p1) { // Error, testing for unknown property
    o.x;
  }
}

function foo2(o: { x: number }) {
  if (o.p2) { // Error, testing for unknown property
    o.p2.x; // error, since o.p2's type is unknown (e.g., could be `number`)
  }
}

function foo3(o: { x: number }) {
  o.p3.x; // usual error outside conditional
}

function foo4(o: $Exact<{ x: number }>) {
  if (o.p4) { // Error, testing for unknown property
    o.p4.x; // currently OK, should be unreachable
  } else {
    o.p4.x; // error
  }
}

function foo6(o: mixed) {
  if (o.bar) {} // error, any lookup on mixed is unsafe
}

function foo7(o: mixed) {
  if (typeof o.bar === 'string') {} // error
  if (o && typeof o.bar === 'string') {} // ok
  if (o != null && typeof o.bar === 'string') {} // ok
  if (o !== null && o !== undefined && typeof o.bar === 'string') {} // ok
}

function foo8(o: { p: mixed }) {
  if (o.p && o.p.q) {} // this is ok because o.p is truthy, so o.p.q is safe
  if (o.p && o.p.q && o.p.q.r) {}
}

type Foo9Expected = {
  foo: string,
}

function foo10() {
  if (null.q) {} // error: property `q` on null
}

function foo11() {
  declare function invariant(a: mixed): void;
  declare var b: mixed;
  invariant(b != null && b.foo != null); // ok
}

function foo12() {
  declare var a: mixed;
  if (a != null && a.foo instanceof Set) {
    (a.foo: Set<mixed>);
  }
}

function foo13() {
  declare var a: mixed;
  if (a != null && Array.isArray(a.foo)) {
    (a.foo: $ReadOnlyArray<mixed>);
  }
}
