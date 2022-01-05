//@flow

var x: number = C; // should be a TDZ error, but for now just undefined
(x: C); // Error, number ~> C

class C {}
