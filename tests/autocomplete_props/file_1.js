// @flow

function test1() {
  type Foo = { x: number };
  type Bar = { foo: Foo };
  const _: Bar = { foo: {   } };
//                        ^
}

function test2() {
  type Foo = { x: number };
  type Bar = { foo?: Foo };
  const _: Bar = { foo: {   } };
//                        ^
}

function test3() {
  type Foo = { x: number };
  type Bar = { foo: ?Foo };
  const _: Bar = { foo: {   } };
//                        ^
}

function test4() {
  type Foo = { x: number };
  type Bar = { foo?: ?Foo };
  const _: Bar = { foo: {   } };
//                        ^
}

function test5() {
  type Foo = { x: number };
  type Bar = ?{ foo: Foo };
  const _: Bar = { foo: {   } };
//                        ^
}

function test6() {
  type Foo = { x: number };
  type Bar = ?{ foo?: Foo };
  const _: Bar = { foo: {   } };
//                        ^
}

function test7() {
  type Foo = { x: number };
  type Bar = { foo: Foo | void | null };
  const _: Bar = { foo: {   } };
//                        ^
}

function test8() {
  type Foo1 = { x: number };
  type Foo2 = { y: number };
  type Bar = { foo: Foo1 | Foo2 };
  const _: Bar = { foo: {   } }; // TODO
//                        ^
}

function test9() {
  type Foo1 = { x: number };
  type Foo2 = { y: number };
  type Bar = ?{ foo?: Foo1 | Foo2 };
  const _: Bar = { foo: {   } }; // TODO
//                        ^
}

function test10() {
  type Foo1 = { x: number };
  type Foo2 = { x: string };
  type Bar = ?{ foo?: Foo1 | Foo2 };
  const _: Bar = { foo: {   } };
//                        ^
}

function test11() {
  type Foo1 = { x: number };
  type Foo2 = { x: string };
  type Bar = ?{ foo?: Foo1 & Foo2 };
  const _: Bar = { foo: {   } };
//                        ^
}

function test12() {
  type Foo = {
    x: number,
  };

  type Bar = {
    foo: Foo,
    foo1: ?Foo,
    foo2?: Foo,
    foo3: Foo | null | void,
    partial: Partial<{foo: Foo}>,
  };

  const _: Bar = {
    foo: {   },
//         ^
    foo1: {   },
//          ^
    foo2: {   },
//          ^
    foo3: {   },
//          ^
    partial: { foo: {   } } };
//                    ^
}

function test13() {
  type Foo1 = { x: number };
  type Foo2 = { y: number };
  type Bar = ?$ReadOnly<{ foo?: $ReadOnly<{ ...Foo1, ...Foo2 }> }>
  const _: Bar = { foo: {   } };
//                        ^
}

function test14() {
  type Foo = { x: number };
  type Bar1 = { foo: Foo };
  type Bar2 = { bar: Foo };
  const _: ?(Bar1 | Bar2) = { foo: {   } };
//                                   ^
}

function test15() {
  type Foo1 = { tag: "1"; x: number };
  type Foo2 = { tag: "2"; x: string };
  type Bar = ?{ foo?: Foo1 | Foo2 };
  const _1: Bar = { foo: { tag: "1",   } };
//                                   ^
  const _2: Bar = { foo: { tag: "2",   } };
//                                   ^
}

function test16() {
  type Foo1 = ?{ x: number };
  type Foo2 = ?{ x: string };
  type Bar1 = ?{ tag: "1"; foo?: Foo1 };
  type Bar2 = ?{ tag: "2", foo?: Foo2 };
  type Bar = Bar1 | Bar2;
  const _1: Bar = { tag: "1", foo: {   } };
//                                   ^
  const _2: Bar = { tag: "2", foo: {   } };
//                                   ^
}

function test17() {
  type Foo1 = { tag: "1"; x?: number };
  type Foo2 = { tag: "2"; y?: number };
  type Bar = ?{ foo: Foo1 | Foo2 };
  const _1: Bar = { foo: { tag: "1",   } }; // TODO
//                                   ^
  const _1: Bar = { foo: { tag: "2",   } }; // TODO
//                                   ^
}

function test18() {
  type Foo = { a1: number, a2: number, b1: number, b2: number };
  type Bar = ?{ foo: Foo };
  const _: Bar = { foo: { a  } };
//                         ^
}

function test19() {
  type Foo = { a1: number, a2: number, b1: number, b2: number };
  type Bar = ?{ foo?: Foo };
  const _: Bar = { foo: { a  } };
//                         ^
}
