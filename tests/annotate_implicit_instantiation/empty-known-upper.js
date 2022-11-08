//@flow


declare function f<T, U>(x: T): {+foo: T, +bar: U};

let x = f(3);
