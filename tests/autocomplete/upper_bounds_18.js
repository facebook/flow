// @flow

type Foo = {
  bar: number,
  baz: number,
};

type K = $Keys<Foo>;

const k: K = ;
//          ^
