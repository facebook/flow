function test_basic() {
  declare function f<const T>(x: T): T;
  const x1 = f(1);
  x1 as 1; // okay
  2 as typeof x1; // error 2 ~> 1

  const x2 = f({ a: 1, b: "c", d: ["e", 2, true, { f: "g" }] });
  x2 as {+a: 1, +b: "c", +d: $ReadOnly<["e", 2, true, {+f: "g"}]>}; // okay

  const x3 = f({f: 1});
  x3 as {+f: 1}; // okay
  x3 as {f: 1}; // error -f ~> f, x3 should not have a lit reason

  function f2<const T>(x: T): T {
    return x;
  }
  const x4 = f2({f: 1});
  x4 as {+f: 1}; // okay

  const arrow = <const X>(x: X): X => x;
  const x5 = arrow({f: 1});
  x5 as {+f: 1}; // okay
}

function test_complex_input() {
  declare function f<const T>(x: T): T;
  declare function id<X>(x: X): X;

  const x1 = f({a: 1, b: id(42)});
  x1.b as 42; // okay

  const x2 = f({a: 1, b: id({f: 42})});
  x2.b.f as 42; // okay

  const x3 = f({a: 1, b: {...{c: 42}}});
  x3.b.c as 42; // okay - const modifier reaches into spread
}

function test_tparam_deeper_in_type() {
  type A<T> = $ReadOnly<{f: {g: T}}>;
  declare function f1<const X>(x: A<X>): X;
  const x1 = f1({f: {g: {h: 1}}});
  x1 as {+h: 1}; // okay
  x1 as {h: 1}; // error +h ~> h

  declare function f2<const X>(x: () => X): X;
  const x2 = f2(() => ({f: 1}));
  x2 as {+f: 1}; // okay
  x2 as {f: 1}; // error +f ~> f
}

function test_class() {
  declare class C<const X> {
    m(x: X): X;
    n<const Y>(y: Y): Y;
    constructor(): C<X>;
    constructor<const X>(x: X): C<X>;
  }
  const c1 = new C<{f:number}>();
  const c2 = new C(2); // unrelated implicit-instantiation error, c2: C<2>

  c1 as C<{f:1}>; // error number ~> 1
  c1 as C<{f:number}>; // okay
  c2 as C<2>; // error 1 ~> {f:number}

  declare var c: C<{f:number}>;
  const x1 = c.m({f:1})
  x1 as {+f:1}; // error number ~> 1, const has no effect

  const x2 = c.n({f:1})
  x2 as {+f:1}; // okay
}

function test_singletons_are_annot_like() {
  declare function f<const T>(x: T): T;
  const x = f({ a: 1});
  const obj = {prop: x.a};
  obj.prop as 1; // okay, prop is annot-like
  obj.prop = 2; // error 2 ~> 1
}

function test_reference() {
  declare function f<const T>(x: T): T;

  const obj1 = {a: 1}; // infers {a:number}
  const x1 = f(obj1); // does not change type of obj
  x1 as {+a: 1}; // error number ~> 1
  x1 as {+a: number}; // okay
  x1 as {a:number}; // okay x is not readonly

  const a = 2;
  const x2 = f({a});
  x2.a as 2; // okay, same as `as const` behavior
}

function test_rest_params() {
  declare function f1<const T: Array<mixed>>(...args: T): T;
  const x1 = f1(...[1]); // error inferred const array is readonly which is not compatible with Array<mixed>

  declare function f2<const T: $ReadOnlyArray<mixed>>(...args: T): T;
  const x2 = f2(...[1, 'a']); // okay inferred const array is readonly which is compatible with $ReadOnlyArray<mixed>

  declare function f3<const T: $ReadOnlyArray<1|'a'>>(...args: T): T;
  const x3 = f3(...[1, 'a']); // TODO okay - this still requires proper hint
                              // propagation support in rest params (D72695560)
}

function test_inferred_union() {
  declare function f<const T>(obj: { x: T, y: T }): T;
  const x = f({ x: [1, 'x'], y: [2, 'y'] });
  x as $ReadOnly<[1, "x"]> | $ReadOnly<[2, "y"]>; // okay
}

function test_subtyping() {
  type PolyFn = <X>(x: X) => X;
  type PolyFnWithConst = <const X>(x: X) => X;
  type PolyFnWithConst2 = <const Y>(x: Y) => Y;

  declare var polyFn: PolyFn;
  declare var polyFnWithConst: PolyFnWithConst;

  polyFn as PolyFnWithConst; // error const incompatible
  polyFnWithConst as PolyFn; // error const incompatible
  polyFnWithConst as PolyFnWithConst2; // okay
}

function test_invalid() {
  interface I1<const T> { x: T }  // error, modifier cannot appear here
  type T<const X> = X; // error, modifier cannot appear here
}

function test_new() {
  class A<const Size> {
    size: Size;
    constructor(x: Size) {}
  }

  const x = new A(1);
  x.size as 1; // okay
  x.size as 2; // error 1 ~> 2

  class B1<Size> extends A<Size> {
    constructor(x: Size) {
      super(x);
    }
  }

  const y1 = new B1(1);
  y1.size as 1; // error number ~> 1
  y1.size as number; // okay

  class B2<const Size> extends A<Size> {
    constructor(x: Size) {
      super(x);
    }
  }

  const y2 = new B2(1);
  y2.size as 1; // okay
  y2.size as 2; // error 1 ~> 2
}
