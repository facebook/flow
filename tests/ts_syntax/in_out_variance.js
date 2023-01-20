declare function f<in A>(A): void; // ERROR
declare function f<out A>(): A; // ERROR
declare function f<in out A>(A): A; // ERROR

declare function f<in>(in): void; // OK
declare function f<out>(out): void; // OK
const out = 1; // OK
