//@flow
type X = {
  [string]: number,
};

declare var x: {foo: number, ...};

(x: X); // Error, inexact objects are incompatible with indexed objects
