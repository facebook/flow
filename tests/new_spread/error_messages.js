//@flow

declare var x: {a: number, ...{| b: number |}, c: number, ...{d: number}}; // Error
(x: any);


declare var y: {...{a: number}, c: number, d: number, ...{| b?: number |}}; // Error
(y: any);

type A = {| b: number |}
type B = {d: number};

declare var x2: {a: number, ...A, c: number, ...B}; // Error
(x2: any);

type C = {a: number};
type D = {| b?: number |};

declare var y2: {...C, c: number, d: number, ...D}; // Error
(y2: any);


declare var x3: { // Error, but message could use improvement.
  ...{a: number},
  d: number,
  ...{b: number},
  e: number,
  ...{c: number},
  f: number,
};
(x3: any);

declare var x4: {...A, ...B, ...C, ...D}; // Error, representative of common case
(x4: any);

declare var x5: {foo: number, bar: number, ...B}; // Error, representative of common case
(x5: any);
