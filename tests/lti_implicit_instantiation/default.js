//@flow

declare function f<T = mixed>(): T;
declare function g<T = {}>(): {[K in keyof T]: K};
let x = f(); // No error
let y = g(); // No error
