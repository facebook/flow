//@flow

function f1<X: number | string, Y: X | boolean>(x: X, y: Y) {
  (x: number | string);
  (x: number | string | boolean);
  (x: X);
  (x: X | string);
  (x: X | boolean);
  (x: number); // nope
  if (typeof x === 'number') {
    (x: X);
    (x: number);
    (x: string); // nope
  }
  if (typeof y === 'boolean') {
    (y: Y);
    (y: X); // nope
  } else {
    (y: X);
    (y: boolean); // nope
  }
}
