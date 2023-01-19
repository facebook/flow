type T = mixed;
declare var m: mixed;

m as T; // ERROR

declare var b: boolean;

b || m as T; // ERROR
b && m as T; // ERROR
b === m as T; // ERROR

let x;
x = m as T; // ERROR

class C {}

new C as T; // ERROR

declare var n: number;
n + n as T; // ERROR

n as T + n; // ERROR
n as T && n; // ERROR
n as T || n; // ERROR
n as T === n; // ERROR

n as const; // ERROR

const as = 1; // OK
