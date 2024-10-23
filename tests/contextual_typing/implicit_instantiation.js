import * as React from 'react';
import { PolyComponent } from "./poly_react_component_export";

function test1() {
  declare function f<T>(f: (string) => T): T;
  // We should be able to infer that c is string.
  const c = f((a) => a);
  (c: string);
  const d = f(function (a) { return a });
  (d: string);

  let f2: (string) => string = (a) => (a: string); // ok
  f2 = (b) => (b: string); // ok

  const reasonTest: (string) => mixed = (a) => (a: empty); // ok
}

function test2() {
  declare class Foo {
    bar<T>(f: (string) => T): T
  }
  declare var foo: Foo;

  const c = foo.bar((a) => a);
  (c: string);
  const d = foo?.bar((a) => a);
  (d: string);

  class Bar {
    #baz<T>(f: (string) => T): T {
      return f("");
    }

    test() {
      const c = this.#baz((a) => a);
      (c: string);
    }
  }
}

function test3() {
  declare function magic<T>(): T;
  declare class A<T> {}

  const magicResult: string = magic(); // OK
  const map: A<string> = new A(); // OK
}

function test4() {
  declare function foo<T>(x: T): Array<T>;
  declare var n: number;
  const x: Array<?number> = foo(n); // OK

  declare function bar<T>(x?: T): Array<T>;
  const y: Array<number> = bar(); // OK

  const z1: Array<empty> = Array.of(); // ok
  const z2: Array<mixed> = Array.of(); // ok
  const z3: Array<string> = Array.of(); // ok
  const z4: Array<string | number> = Array.of(1); // ok
  const z5: Array<string> = z3.concat(1); // Only one error on 1 ~> string
}

function test5() {
  declare function id<T>(T): T;
  const f1: (string) => string = id(s => (s: string)); // ok

  declare class A<T> {
    constructor(T): void
  }
  const a: A<string=>string> = new A(x => x); // OK

  declare class B<T> {
    constructor<V>(T): void
  }
  const b: B<string=>string> = new B(x => x); // OK

  declare class C<T> {
    constructor(T, number): void;
    constructor(T): void;
  }
  const c1: C<string=>string> = new C(x => x); // OK
  const c2: C<string=>string> = new C(x => x, 1); // OK

  declare function Component<T>({v: T, f: (T) => void}): void;
  <Component v="1" f={(v) => {(v: string)}} />; // OK
  <PolyComponent v="1" f={(v) => {(v: string)}} />; // OK
}

function test6() {
  declare function call<A>(v: A, f: (A) => void): void
  call("ss", s => {}); // ok

  declare function pipe<A, B, C>(a: A, f1: A=>B, f2: B=>C): C;
  const _: string = pipe("s", s => 1, n => "");

  declare class Pipe<A, B> {
    constructor(a: A, f1: A=>B, f2: B=>void): void
  }
  new Pipe("s", s => 1, n => {}); // OK
}

function test7() {
  declare function f<A>(A, A=>void, number): void;
  f(3, (n) => {}, ""); // Error on third argument, but n can still be contextually typed.
}

function test8() {
  declare function f<T=string>(T => string): T;
  f(s => (s: string)); // ok
}

function test9() {
  // This overload selection problem will trigger a nested implicit instantiation.
  // Correctly selecting the overload requires us to flow targ to bound, which is
  // disabled during implicit instantiation.
  type M<O> = {[K in keyof O]: O[K] extends string ? string : boolean};
  declare function id<T>(T): T;
  // A regression test for an earlier bug where the overload is incorrectly selected, and causes
  // an incompatibility.
  const _: M<{| num: number |}> = {num: id(true)}; // ok
}

function test10() {
  type V = {||};
  type Variables = {|+[string]: any|};
  declare opaque type Query<-TVariables: Variables>;
  declare var q: Query<V>;
  declare function poly<TVariables: Variables>(q: Query<TVariables>): void;
  poly(q); // ok
}

function test11() {
  declare var jest: {
    fn<TArguments: $ReadOnlyArray<any> = $ReadOnlyArray<any>>(): (...args: TArguments)=>void
  }
  declare var C1: React.ComponentType<{+f: () => void}>;
  <C1 f={jest.fn()} />; // ok

  declare var C2: React.ComponentType<{f1: () => void, f2: () => void}>
  <C2 f1={jest.fn()} f2={jest.fn()} />; // ok
}

function test12(foo: ?Array<string>) {
  const a = new Set(foo || []);
  const b = new Set(foo ?? []);
  (a: Set<empty>); // error Set<string> ~> Set<empty>
  (b: Set<empty>); // error Set<string> ~> Set<empty>
}

function test13() {
  declare function foo<V>(v: V, f: (V) => void): void;

  foo(
    { elements: [] },
    (value: { elements: Array<string> }) => { (value: empty); }, // error
  )
}

function test14() {
  declare function magic<T>(): T;
  const m: mixed = { foo: magic() }; // ok
  const interfaceVar: interface {[string]: mixed} = { foo: magic() }; // ok
}

function test15() {
  const map: Map<string, string> = [""].reduce((acc, v) => {
    // TODO: This is an unfortunate error when we wrap the hint on logical expression with MaybeT
    //       for LHS. It makes acc to have type `?Map<string, string>`
    acc.set("", ""); // error
    return acc;
  }, new Map()) ?? new Map();
}

function test16() {
  const safeParse = <T>(json: ?string): ?T => null;
  const foo: Array<string> = safeParse('') ?? []; // ok
}

function test17() {
  declare function magic<T>(null): T;
  declare function magic<T>(): T;
  const _: interface {[key: string]: any} = { foo: magic() }; // ok
}

function test18() {
  declare function foo({||}): void;
  foo({bar: new Set([''])}); // error: there should be only one prop-missing error
}

function test19() {
  declare var xs: Array<any>;

  async function foo() {
    const imageHashes: Array<string> = await Promise.all(xs); // ok
  }
}

function test20() {
  type Record<T: {...}> = { ...T, ... };

  declare var fn: <TArguments, TReturn>(
    implementation: (...args: TArguments) => TReturn,
  ) => { (...args: TArguments): TReturn };

  const result: (
    value: mixed,
    records: Record<{...}>,
  ) => any = fn((value, records) => 42); // okay
}

function test21() {
  declare var a: Array<number>;

  function F(): React.MixedElement {
    return (
      <>
        { a.map(_ => 3) }
      </>
    );
  }
}

// Iterable
{
  declare const f: (Iterable<string => mixed>) => void;
  f([x => 1]); // OK
}
