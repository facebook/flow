//@flow

//First inexact, second exact, then optional
declare var w: {...{a: number}, ...{|c: number|}, ...{|b?: number|}}; // Error
w as null;

//First inexact, second exact, then optional
declare var x: {...{a: number}, c: number, ...{|b?: number|}}; // Error
x as null;

// First exact, second inexact, then optional
declare var y: {...{|a: number|}, ...{a: number}, ...{|b?: number|}}; // Error
y as null;

// 2 inexacts then optional
declare var z: {...{a: number}, ...{a: string}, ...{|b?: number|}}; // Error
z as null;

// Let's put some slices in before the same patterns now:

//First inexact, second exact, then optional
declare var a: {...{a: number}, ...{|c: number|}, ...{|b?: number|}}; // Error
a as null;

//First inexact, second exact, then optional
declare var b: {a: number, ...{a: number}, c: number, ...{|b?: number|}}; // Error
b as null;

// First exact, second inexact, then optional
declare var c: {a: number, ...{|a: number|}, ...{a: number}, ...{|b?: number|}}; // Error
c as null;

// 2 inexacts then optional
declare var d: {a: number, ...{a: number}, ...{a: string}, ...{|b?: number|}}; // Error
d as null;

type A = {|b: number|};
type B = {d: number};

declare var x2: {a: number, ...A, c: number, ...B}; // Error
x2 as any;

type C = {a: number};
type D = {|b?: number|};

declare var y2: {...C, c: number, d: number, ...D}; // Error
y2 as any;

declare var x3: {
  // Error, but message could use improvement.
  ...{a: number},
  d: number,
  ...{b: number},
  e: number,
  ...{c: number},
  f: number,
};
x3 as any;

declare var x4: {...A, ...B, ...C, ...D}; // Error, representative of common case
x4 as any;

declare var x5: {foo: number, bar: number, ...B}; // Error, representative of common case
x5 as any;
