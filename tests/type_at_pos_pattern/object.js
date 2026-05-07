// @flow

declare opaque type O;
declare const o: O;

declare const c2: O;
declare const d2: O;
declare const e: string;

var {
  a,
  b: b1,
  c = c2,
  d: d1 = d2,
  [e]: { e1 }
} = {
  a: o,
  b: o,
  c: o,
  d: o,
  "e": { a: o },
};
