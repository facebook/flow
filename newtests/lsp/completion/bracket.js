// @flow

const o = {
  a: 1,
  b: 'hi',
};

type T = {|
  foo: boolean,
  bar: string,
|};

const a = o[];
//         ^
type B = T[]
//        ^
