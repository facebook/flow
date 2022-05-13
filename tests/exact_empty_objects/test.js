({}: {}); // OK
({}: {...}); // OK

({}: {a: number}); // ERROR
({}: {a: number, ...}); // ERROR

({}: {a?: number}); // OK
({}: {a: void}); // ERROR

const err = {};
err.xxx; // ERROR
if (err.xxx) {} // ERROR
err.m(); // ERROR

const spread: {a: number, b: string} = {a: 1, ...{}}; // ERROR

declare function f(a: {a: void}): void;
f({}); // ERROR

function paramDefault(o: {a?: string, b?: number} = {}) {
  const {a, b} = o;
  (a: string | void); // OK
  (b: number | void); // OK
}
paramDefault(); // OK

function paramDefaultDestructuring({a, b}: {a?: string, b?: number} = {}) {
  (a: string | void); // OK
  (b: number | void); // OK

  (a: empty); // ERROR
}
paramDefaultDestructuring(); // OK

function paramDefaultDestructuringWithInnerDefault({a, b = 1}: {a?: string, b?: number} = {}) {
  (a: string | void); // OK
  (b: number); // OK
}
paramDefaultDestructuringWithInnerDefault(); // OK
