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
