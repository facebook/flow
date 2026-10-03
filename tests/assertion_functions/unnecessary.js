// flowlint unnecessary-invariant:error

declare function assert(condition: unknown, message?: string): asserts condition;
declare function assertSecond(message: string, condition: unknown): asserts condition;
declare function assertString(value: unknown): asserts value is string;
declare function assertGeneric<T>(condition: T): asserts condition;
declare const ns: {assert(condition: unknown): asserts condition};

declare const t: true;
declare const one: 1;
declare const obj: {};
declare const both: false & true;
declare const b: boolean;
declare const n: number;
declare const a: any;
declare const s: string;

assert(t); // error: unnecessary, names `assert`
assert(one); // error
assert(obj); // error
assert(both); // error: intersection type
assertSecond('msg', t); // error: asserted parameter is not the first
assertGeneric<true>(t); // error
ns.assert(t); // error: names `ns.assert`
assert(b);
assert(n);
assert(a);
assertString(s); // type-guard assertions are not checked for truthiness
