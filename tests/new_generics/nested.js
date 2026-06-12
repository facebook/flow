//@flow

declare const n: number;

function f<X, Y extends X, Z extends Y, W>(y: Y, z: X, w: W): X {
  if (n === 0) return y;
  if (n === 1) return z;
  return w; // nope
}

function g<X, Y extends X | number>(a: {y: Y, ...}): {y: Y, ...} {
  if (typeof a.y !== 'number') {
    return a;
  }
  return 42 as any;
}
