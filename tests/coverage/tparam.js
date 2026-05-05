//@flow


export type RecordOf<Values extends Object> = Values;

function f<X extends Object>(x: X): X {
    var o = x;
    return o;
}

var y = f();
