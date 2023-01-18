// @flow

type INT_SENTINELS =
  | {type: 1, p: number}
  | {type: 2, p: string};

declare var x1: INT_SENTINELS;

if (x1.type === 1) {
  (x1.p: number); // ok
  (x1.p: string); // should error
}

type FLOAT_SENTINELS =
  | {type: 1.0, p: number}
  | {type: 2.0, p: string};

declare var x2: FLOAT_SENTINELS;

if (x2.type === 1.000) {
  (x2.p: number); // ok
  (x2.p: string); // should error
}

type UNION_FLOAT_SENTINELS =
  | {type: 1.0 | 1.1, p: number}
  | {type: 2, p: string};

declare var x3: UNION_FLOAT_SENTINELS;

if (x3.type === 1.000) {
  (x3.p: number); // ok
  (x3.p: string); // should error
}
if (x3.type === 1e0) {
  (x3.p: number); // ok
  (x3.p: string); // should error
}
