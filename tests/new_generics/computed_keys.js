//@flow
function f<X: string, Y: 'lit', Z: 'prop'>(
  x: X,
  y: Y,
  z: Z,
  a: {[X]: string},
  b: {prop: number},
) {
  a[x] = 'hi';
  a['foo'] = 'hi'; // nope
  (a[x]: string);
  a['foo']; // nope
  b[x] = 42; // allowed, just as if x: string
  b[y] = 42; // nope
  b[z] = 42;
  b[x];
  b[y]; // nope
  b[z];
}
