//@flow

declare function f<T = mixed>(): T;

let x = f(); // No error
