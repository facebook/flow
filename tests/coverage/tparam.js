//@flow


export type RecordOf<Values extends any> = Values;

function f<X extends any>(x: X): X {
    var o = x;
    return o;
}

var y = f();
