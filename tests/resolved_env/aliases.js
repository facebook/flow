//@flow

var x: T = "hello world";
type T = S;
type S = number;

var x: A = "hello world";
opaque type A: S = B;
opaque type B: T = T;
