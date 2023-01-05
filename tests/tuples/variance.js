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
(c: [+foo: string]); // OK
(c: [-foo: string]); // OK

(c: [foo: string | void]); // ERROR
(c: [-foo: string | void]); // ERROR
(c: [+foo: string | void]); // OK
(["s"]: [foo: string | void]); // OK

declare var d: [string | void];
(d: [foo: string]); // ERROR
(d: [-foo: string]); // OK

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

// $ReadOnly on tuple
type ROC = $ReadOnly<C>;
declare var roc: ROC;
(roc: [+foo: string]); // OK
(roc: [-foo: string]); // ERROR
(roc: [foo: string]); // ERROR
(roc[0]: string); // OK
roc[0] = "s"; // ERROR - can't write

type ROB = $ReadOnly<B>;
declare var rob: ROB;
(rob: [+foo: string]); // OK
(rob: [-foo: string]); // ERROR
(rob: [foo: string]); // ERROR
(rob[0]: string); // OK
rob[0] = "s"; // ERROR - can't write

type Union = $ReadOnly<[string, number] | [string, boolean]>;
declare var u: Union;
u[0] = "s"; // ERROR - can't write
(u[0]: string); // OK
(u[1]: number | boolean); // OK

type Intersection = $ReadOnly<[string, number] & [string, boolean]>;
declare var inter: Intersection;
inter[0] = "s"; // ERROR - can't write
(inter[0]: string); // OK
