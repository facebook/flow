// @flow

type T = "foo" | "bar" | T;

const x: T = ;
//          ^

type A = "foo" | B;
type B = "bar" | A;

const y: A = ;
//          ^
