type A = [+foo: string];
["s"] as A; // OK
declare var a: A;
a as [+foo: string]; // OK
a as [-foo: string]; // ERROR
a as [foo: string]; // ERROR
a[0] as string; // OK
a[0] = "s"; // ERROR - can't write

type B = [-foo: string];
["s"] as B; // OK
["s" as "s"] as B; // OK
declare var b: B;
b as [-foo: string]; // OK
b as [+foo: string]; // ERROR
b as [foo: string]; // ERROR
b[0] as string; // ERROR - can't read
b[0] = "s"; // OK

type C = [foo: string];
["s"] as C; // OK
declare var c: C;
c as [+foo: string]; // OK
c as [-foo: string]; // OK

c as [foo: string | void]; // ERROR
c as [-foo: string | void]; // ERROR
c as [+foo: string | void]; // OK
["s"] as [foo: string | void]; // OK

declare var d: [string | void];
d as [foo: string]; // ERROR
d as [-foo: string]; // OK

declare class K<T> {}
declare var m: K<[+foo: string]>;
m as K<[+foo: string]>; // OK
m as K<[string]>; // ERROR

// We still flow each element, even if variance is invalid
declare function f<K>(x: Array<[K]>): K;
declare var n: Array<[+n: number]>;
const res = f(n); // ERROR
res as number; // OK
res as empty; // ERROR

declare var osr: [+foo: {a: number}];
// $FlowExpectedError[unsafe-object-assign]
Object.assign({a: 1}, ...osr); // OK
declare var osw: [-foo: {a: number}];
// $FlowExpectedError[unsafe-object-assign]
Object.assign({a: 1}, ...osw); // ERROR - can't read

// $ReadOnly on tuple
type ROC = $ReadOnly<C>;
declare var roc: ROC;
roc as [+foo: string]; // OK
roc as [-foo: string]; // ERROR
roc as [foo: string]; // ERROR
roc[0] as string; // OK
roc[0] = "s"; // ERROR - can't write

type ROB = $ReadOnly<B>;
declare var rob: ROB;
rob as [+foo: string]; // OK
rob as [-foo: string]; // ERROR
rob as [foo: string]; // ERROR
rob[0] as string; // OK
rob[0] = "s"; // ERROR - can't write

type Union = $ReadOnly<[string, number] | [string, boolean]>;
declare var u: Union;
u[0] = "s"; // ERROR - can't write
u[0] as string; // OK
u[1] as number | boolean; // OK

type Intersection = $ReadOnly<[string, number] & [string, boolean]>;
declare var inter: Intersection;
inter[0] = "s"; // ERROR - can't write
inter[0] as string; // OK

// Tuple elements cannot be subtyped
[1, 2] as [1, 2] as [number, number]; // ERROR
