type A = [readonly foo: string];
["s"] as A; // OK
declare const a: A;
a as [readonly foo: string]; // OK
a as [writeonly foo: string]; // ERROR
a as [foo: string]; // ERROR
a[0] as string; // OK
a[0] = "s"; // ERROR - can't write

type B = [writeonly foo: string];
["s"] as B; // OK
["s" as "s"] as B; // OK
declare const b: B;
b as [writeonly foo: string]; // OK
b as [readonly foo: string]; // ERROR
b as [foo: string]; // ERROR
b[0] as string; // ERROR - can't read
b[0] = "s"; // OK

type C = [foo: string];
["s"] as C; // OK
declare const c: C;
c as [readonly foo: string]; // OK
c as [writeonly foo: string]; // OK

c as [foo: string | void]; // ERROR
c as [writeonly foo: string | void]; // ERROR
c as [readonly foo: string | void]; // OK
["s"] as [foo: string | void]; // OK

declare const d: [string | void];
d as [foo: string]; // ERROR
d as [writeonly foo: string]; // OK

declare class K<T> {}
declare const m: K<[readonly foo: string]>;
m as K<[readonly foo: string]>; // OK
m as K<[string]>; // ERROR

// We still flow each element, even if variance is invalid
declare function f<K>(x: Array<[K]>): K;
declare const n: Array<[readonly n: number]>;
const res = f(n); // ERROR
res as number; // OK
res as empty; // ERROR

declare const osr: [readonly foo: {a: number}];
// $FlowExpectedError[unsafe-object-assign]
Object.assign({a: 1}, ...osr); // OK
declare const osw: [writeonly foo: {a: number}];
// $FlowExpectedError[unsafe-object-assign]
Object.assign({a: 1}, ...osw); // ERROR - can't read

// $ReadOnly on tuple
type ROC = Readonly<C>;
declare const roc: ROC;
roc as [readonly foo: string]; // OK
roc as [writeonly foo: string]; // ERROR
roc as [foo: string]; // ERROR
roc[0] as string; // OK
roc[0] = "s"; // ERROR - can't write

type ROB = Readonly<B>;
declare const rob: ROB;
rob as [readonly foo: string]; // OK
rob as [writeonly foo: string]; // ERROR
rob as [foo: string]; // ERROR
rob[0] as string; // OK
rob[0] = "s"; // ERROR - can't write

type Union = Readonly<[string, number] | [string, boolean]>;
declare const u: Union;
u[0] = "s"; // ERROR - can't write
u[0] as string; // OK
u[1] as number | boolean; // OK

type Intersection = Readonly<[string, number] & [string, boolean]>;
declare const inter: Intersection;
inter[0] = "s"; // ERROR - can't write
inter[0] as string; // OK

// Tuple elements cannot be subtyped
[1, 2] as [1, 2] as [number, number]; // ERROR
