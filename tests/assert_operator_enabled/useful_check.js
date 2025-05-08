let x = 10;
x!; // error
x! = 42; // error
(x + 20)!; // error;

declare const y: { a: { b?: { c: {d : number } }}};
y!; // error;
y.a!; // error;
y.a.b!; // ok
y.a!.b; // error
y.a.b?.c!; // ok
y.a.b?.c!.d; // TODO should be error

declare const a1: Array<number>;
declare const a2: $ReadOnlyArray<number>;
declare const a3: [number];
declare const o1: { [string]: string };
declare const o2: { e: number };
declare const o3: { e: number, ... }

a1[0]!; //ok
a2[0]!; // ok
a3[0]!; // error
o1['a']!; // ok
o1.a!; // ok;
o2.e!; // error
o3.e!; // error
