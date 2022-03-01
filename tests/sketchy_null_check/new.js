//@flow

opaque type T = number;
declare opaque type S: number;

function f<X: number>(x: ?X, t: ?T, s: ?S) {
    if (x) {}
    else if (t) { }
    else if (s) { }
}
