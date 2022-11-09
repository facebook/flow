//@flow


declare function f<T>(): {current: T | null};

let x = f<_>();
x.current = 3;
