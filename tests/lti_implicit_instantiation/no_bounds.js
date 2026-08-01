//@flow

declare function f<T, U extends T>(x: U): T;

f(3); // no error, T constrained when flowing U <: T

declare function forward<U extends T, T>(x: U): T;

const forwardResult: number = forward(3); // OK: U constrains later T before T is pinned
forward(3); // OK: no return hint needed
