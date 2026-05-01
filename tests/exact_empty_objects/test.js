({} as {}); // OK
({} as {...}); // OK

({} as {a: number}); // ERROR
({} as {a: number, ...}); // ERROR

({} as {a?: number}); // OK
({} as {a: void}); // ERROR

const err = {};
err.xxx; // ERROR
if (err.xxx) {} // ERROR
err.m(); // ERROR
err.a = 1; // ERROR
err['a'] = 1; // ERROR
err['a' as string] = 1; // ERROR
err['a' as any] = 1; // OK: allow unsoundness when key is any.

const spread: {a: number, b: string} = {a: 1, ...{}}; // ERROR

declare function f(a: {a: void}): void;
f({}); // ERROR

function paramDefault(o: {a?: string, b?: number} = {}) {
  const {a, b} = o;
  a as string | void; // OK
  b as number | void; // OK
}
paramDefault(); // OK

function paramDefaultDestructuring({a, b}: {a?: string, b?: number} = {}) {
  a as string | void; // OK
  b as number | void; // OK

  a as empty; // ERROR
}
paramDefaultDestructuring(); // OK

function paramDefaultDestructuringWithInnerDefault({a, b = 1}: {a?: string, b?: number} = {}) {
  a as string | void; // OK
  b as number; // OK
}
paramDefaultDestructuringWithInnerDefault(); // OK
