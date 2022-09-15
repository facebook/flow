//@flow

declare function f<T, U: T>(x: U): T;

f(3); // TODO no error
