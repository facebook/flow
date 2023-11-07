//@flow
type X = {
  [string]: number,
};

declare var x: {foo: number, ...};

x as X; // Error, inexact objects are incompatible with indexed objects
