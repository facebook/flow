// @flow

type F =
  & (() => void)
  & ((x: string) => void)
  & (<A>(x: A, y: A) => void);

function x0(f: F) {
  f(/* here */)
}

function x1(f: F | void) {
  f?.(/* here */)
}

function x2(f: ?{ m: F }) {
  f?.m(/* here */)
}

function x3(f: ?{ [key: string]: F }) {
  f?.["abc"](/* here */)
}

function x4(f: ?{ m: { n: F } }) {
  f?.m.n(/* here */)
}

function x5(f: { m: ?F }) {
  f.m?.(/* here */)
}

function x6(g: ?{ f: F }) {
  if (g != null) {
    g.f(/* here */)
  }
}

// Nested overloads

type G<V> =
  & (() => void)
  & ((x: V) => void);

type H =
  & G<string>
  & (<A>(x: A, y: A) => void);

function y0(f: H) {
  f(/* here */)
}

function y1(f: H | void) {
  f?.(/* here */)
}

function y2(f: ?{ m: H }) {
  f?.m(/* here */)
}

function y3(f: ?{ [key: string]: H }) {
  f?.["abc"](/* here */)
}

function y4(f: ?{ m: { n: H } }) {
  f?.m.n(/* here */)
}

function y5(f: { m: ?H }) {
  f.m?.(/* here */)
}

function y6(g: ?{ f: H }) {
  if (g != null) {
    g.f(/* here */)
  }
}

// Error cases - no suggestions

function x0(f: ?F, h: ?H) {
  f(/* here */)
  h(/* here */)
}

// simple overload

function test_overload() {
  declare function f(x: 1): 1;
  declare function f(x: 2): 2;
  f(/* here */);
}

// overloaded bounds

function test_overloaded_bound() {
  type T1 = (x: 1) => 1;
  type T2 = (x: 2) => 2;

  function test1<T: T1 & T2>(fn: T) {
    fn(/* here */);
  }

  function test2<X1: T1, X2: T2>(fn: X1 & X2) {
    fn(/* here */);
  }
}

// overloaded coercion (ClassT ~> FunT)

function overloaded_coercion() {
  declare class C1 { (x: 1): void; }
  declare class C2 { (x: 2): void; }

  function test4(fn: C1 & C2) {
    fn(/* here */);
  }
}

function overloaded_coercion_poly() {
  declare class C1 { <X: 1>(x: X): void; }
  declare class C2 { <X: 2>(x: X): void; }

  function test4(fn: C1 & C2) {
    fn(/* here */); // TODO drop generic
  }
}

function overloaded_class_coercion() {
  declare class C1 { static (x: 1): void; }
  declare class C2 { static (x: 2): void; }

  function test4(fn: typeof C1 & typeof C2) {
    fn(/* here */);
  }
}

// overloaded jsdoc

function overloaded_jsdoc() {
  /**
   * first overload
   * @param x - 1st param of first overload
   * @param y - 2nd param of first overload
   */
  declare function foo(x: 10, y: 11): 1;
  /**
   * second overload
   * @param x - 1st param of second overload
   * @param y - 2nd param of second overload
   */
  declare function foo(x: 20, y: 21): 2;

  foo(/* here */); // TODO jsdoc
  foo(10, /* here */); // TODO jsdoc
  foo(20, /* here */); // TODO jsdoc, select the right activeSignature
}
