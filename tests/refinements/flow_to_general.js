//@flow

function f(x: any) {
  if (typeof x === 'number') { }
  (x: empty);
};
f((42: any));

type T = 'A' | 'B'

declare var x: { p?: ?T };
let { p } = x;

if (p === 'C') { }
