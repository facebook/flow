declare opaque type O1;
declare opaque type O2;
declare opaque type P1;
declare opaque type P2;

type F1 = (x: number) => O1;
type F2 = <A>(x: Array<A>) => O1;
type F3 = (x: string) => O2;

type G1 = (x: number) => P1;
type G2 = <A>(x: Array<A>) => P1;
type G3 = (x: string) => P2;

type Foo = { foo: mixed };
type Bar = { bar: mixed };
type Baz = { baz: () => mixed };

function test1() {
  declare var foo: F1 & F2 & F3;
  const x1 = foo(1);
//           ^
  const x2 = foo([""]);
//           ^
  const x3 = foo("");
//           ^
  const error = foo(true);
//              ^
}

function test2() {
  declare var foo: F1 & (F2 & F3);

  const x1 = foo(1);
//           ^
  const x2 = foo([""]);
//           ^
  const x3 = foo("");
//           ^
  const error = foo(true);
//              ^
}

function test3() {
  declare var foo: (F1 & F2) & F3;

  const x1 = foo(1);
//           ^
  const x2 = foo([""]);
//           ^
  const x3 = foo("");
//           ^
  const error = foo(true);
//              ^
}

function test4() {
  declare var foo: F1 | G1;
  const x = foo(1);
//          ^
}

function test5() {
  declare var foo: (F1 | G1) & F2;

  const x1 = foo(1);
//           ^
  const x2 = foo([""]);
//           ^
  const x2 = foo("");
//           ^
  const error = foo(true);
//              ^
}

function test6() {
  declare function foo<X>(x: X): X;
  const x = foo(1);
//          ^
}

function test7() {
  declare var foo: ?(<X>(x: X) => X);
  const x = foo?.(1);
//          ^
}

function test8() {
  declare var foo:
    | ((F1 & F2) & F3)
    | ((G1 & G2) & G3);

  const x1 = foo(1);
//           ^
  const x2 = foo([""]);
//           ^
  const x3 = foo("");
//           ^
  const error = foo(true);
//              ^
}

function test9() {
  declare class A {
    foo(x: number): O1;
    foo<A>(x: Array<A>): O1;
    foo(x: string): O2;
  }

  declare var a: A;
  const x1 = a.foo(1);
//             ^
  const x2 = a.foo([""]);
//             ^
  const x3 = a.foo("");
//             ^
  const error = a.foo(true);
//                ^
}

function test10() {
  declare class a {
    static foo(x: number): O1;
    static foo<A>(x: Array<A>): O1;
    static foo(x: string): O2;
  }

  const x1 = a.foo(1);
//             ^
  const x2 = a.foo([""]);
//             ^
  const x3 = a.foo("");
//             ^
  const error = a.foo(true);
//                ^
}

function test11() {
  declare var obj: { foo?: ?<X>(x: X) => X } ;
  const x1 = obj.foo?.(1);
//           ^
  const x2 = obj.foo?.(1);
//               ^
}

function test12() {
  declare var obj: {
    foo?: F1 & F2 & F3;
  };
  const x = obj.foo?.(1);
//          ^
}

function test13() {
  declare var obj: any | { f: <V>(x: V) => V };

  const x1 = obj.f(1);
//               ^
}

function test14() {
  declare var o1: any & { f: <V>(x: V) => V };
  declare var o2: { f: <V>(x: V) => V } & any;

  const x1 = o1.f(1); // any
//              ^
  const x2 = o2.f(1); // number => number
//              ^
}

function test15() {
  declare var o1: any & ?{ f: <V>(x: V) => V };
  declare var o2: ?{ f: <V>(x: V) => V } & any;

  const x1 = o1?.f(1); // any
//               ^
  const x2 = o2?.f(1); // number => number
//               ^
}

function test16() {
  declare var x: { f: () => {} } | void;
  x.f(); // (() => void) | any
//  ^
}

function test17() {
  // This test handles a speculation edge case, where we call into statement.ml
  // with a non-empty speculation stack.
  declare function from<A>(iter: A): A;
  declare function from(iter: any): any;

  declare class A {
    setLabel(label: any): any;
  };
  const a = new A().setLabel("hello").setLabel(from(1));
//                  ^
}

function test18() {
  declare var x: (Foo & Bar) & Baz;
  x.baz(); // () => mixed
//  ^
}

function test19() {
  declare var error: mixed;
  error();
// ^
}
