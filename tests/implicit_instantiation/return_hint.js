type MyUnion = {+FOO: mixed} | {+BAR: mixed};
declare const x: MyUnion;

declare function f<T>(x: $ReadOnly<T>): T;

const a = f(x); // OK
(a: MyUnion); // OK

const b: MyUnion = f(x); // OK
const c: ?MyUnion = f(x); // OK
