//@flow


export type RecordOf<Values: Object> = Values;

function f<X: Object>(x: X): X {
    var o = x;
    return o;
}

var y = f();
