//@flow

declare function f<T, U extends T>(x: U): T;

f(3); // no error, T constrained when flowing U <: T
