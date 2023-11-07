//@flow

function f(x: any) {
  if (typeof x === 'number') {
  }
  x as empty;
}
f(42 as any);

type T = 'A' | 'B';

declare var x: {p?: ?T};
let {p} = x;

if (p === 'C') {
}
