//@flow

const x: T = "hello world"; // err
type T = S;
type S = number;

const x: A = "hello world"; // err
opaque type A: S = B;
opaque type B: T = T;
