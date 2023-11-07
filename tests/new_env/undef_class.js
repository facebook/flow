//@flow

var x: number = C; // should be a TDZ error, but for now just undefined
x as C; // Error, number ~> C

class C {}
