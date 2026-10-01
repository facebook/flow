//@flow

//First inexact, second exact, then optional
declare const w: {...{a: number, ...}, ...{c: number}, ...{b?: number}, ...}; // OK
w as null; // Error

//First inexact, second exact, then optional
declare const x: {...{a: number, ...}, c: number, ...{b?: number}, ...}; // OK
x as null; // Error

// First exact, second inexact, then optional
declare const y: {...{a: number}, ...{a: number, ...}, ...{b?: number}, ...}; // OK
y as null; // Error

// 2 inexacts then optional
declare const z: {...{a: number, ...}, ...{a: string, ...}, ...{b?: number}, ...}; // OK
z as null; // Error

// Let's put some slices in before the same patterns now:

//First inexact, second exact, then optional
declare const a: {...{a: number, ...}, ...{c: number}, ...{b?: number}, ...}; // OK
a as null; // Error

//First inexact, second exact, then optional
declare const b: {a: number, ...{a: number, ...}, c: number, ...{b?: number}, ...}; // OK
b as null; // Error

// First exact, second inexact, then optional
declare const c: {a: number, ...{a: number}, ...{a: number, ...}, ...{b?: number}, ...}; // OK
c as null; // Error

// 2 inexacts then optional
declare const d: {a: number, ...{a: number, ...}, ...{a: string, ...}, ...{b?: number}, ...}; // OK
d as null; // Error

type A = {b: number};
type B = {d: number, ...};

declare const x2: {a: number, ...A, c: number, ...B, ...}; // OK
x2 as any;

type C = {a: number, ...};
type D = {b?: number};

declare const y2: {...C, c: number, d: number, ...D, ...}; // OK
y2 as any;

declare const x3: {
  ...{a: number, ...},
  d: number,
  ...{b: number, ...},
  e: number,
  ...{c: number, ...},
  f: number,
 ...};
x3 as any;

declare const x4: {...A, ...B, ...C, ...D, ...}; // OK
x4 as any;

declare const x5: {foo: number, bar: number, ...B, ...}; // OK
x5 as any;
