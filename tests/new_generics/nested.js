//@flow

declare var n: number;

function f<X, Y: X, Z: Y, W>(y: Y, z: X, w: W): X {
  if (n === 0) return y;
  if (n === 1) return z;
  return w; // nope
}

function g<X, Y: X | number>(a: {y: Y}): {y: Y} {
  if (typeof a.y !== 'number') {
    return a;
  }
  return (42: any);
}
