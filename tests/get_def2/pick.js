// @flow

type Obj = {foo: string, bar: number};

function locally_defined_pick() {
  type Pick<A, B> = A;

  type T = Pick<Obj, 'foo'>; // no result for locally defined pick
//                     ^
}

type T1 = Pick<Obj, 'foo'>; // jump to foo prop
//                    ^
type T2 = Pick<Obj, 'bar'>; // jump to bar prop
//                    ^
type Foo = 'foo';
type T3 = Pick<Obj, Foo>; // jump to type Foo, instead of jumping to foo prop
//                    ^

type T4 = Pick<Obj, 'foo' | 'bar'>;  // jump to foo prop
//                    ^
type T5 = Pick<Obj, 'foo' | 'bar'>;  // jump to bar prop
//                             ^
type T6 = Pick<Obj, Foo | 'bar'>;  // jump to bar prop
//                          ^
