// @flow

type A = [+foo: string];
(["s"]: A); // OK
declare var a: A;
(a: [+foo: string]); // OK
(a: [-foo: string]); // ERROR
(a: [foo: string]); // ERROR
(a[0]: string); // OK
a[0] = "s"; // ERROR - can't write

type B = [-foo: string];
(["s"]: B); // OK
declare var b: B;
(b: [-foo: string]); // OK
(b: [+foo: string]); // ERROR
(b: [foo: string]); // ERROR
(b[0]: string); // ERROR - can't read
b[0] = "s"; // OK

type C = [foo: string];
(["s"]: C); // OK
declare var c: C;
(c: A); // ERROR
(c: B); // ERROR

declare class K<T> {}
declare var m: K<[+foo: string]>;
(m: K<[+foo: string]>); // OK
(m: K<[string]>); // ERROR

// We still flow each element, even if variance is invalid
declare function f<K>(Array<[K]>): K;
declare var n: Array<[+n: number]>;
const res = f(n); // ERROR
(res: number); // OK
(res: empty); // ERROR

declare var osr: [+foo: {a: number}];
Object.assign({a: 1}, ...osr); // OK
declare var osw: [-foo: {a: number}];
Object.assign({a: 1}, ...osw); // ERROR - can't read
