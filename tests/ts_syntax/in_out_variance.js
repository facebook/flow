declare function f<in A>(x: A): void; // ERROR
declare function f<out A>(): A; // ERROR
declare function f<in out A>(x: A): A; // ERROR

declare function f<in>(x: in): void; // OK
declare function f<out>(x: out): void; // OK
const out = 1; // OK
