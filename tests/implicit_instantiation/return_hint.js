type MyUnion = {+FOO: unknown} | {+BAR: unknown};
declare const x: MyUnion;

declare function f<T>(x: Readonly<T>): T;

const a = f(x); // OK
a as MyUnion; // OK

const b: MyUnion = f(x); // OK
const c: ?MyUnion = f(x); // OK
