// @flow

// A member of an object literal is framed as a declaration, and is never
// qualified: the literal has no name to qualify it with, even inside a class.

const o = {
  p: 1,
//^
  m(): number {
//^
    return 1;
  },
  get g(): number {
//    ^
    return 1;
  },
  set s(v: number): void {},
//    ^
  'quoted': 2,
//^
  3: 'three',
//^
  ...{spread: 4},
//    ^
};

class C {
  field: {inner: number} = {inner: 1};
//                          ^
}
