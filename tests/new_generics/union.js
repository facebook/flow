//@flow

function f1<X extends boolean, Y extends X | boolean>(x: X, y: Y) {
  x as number | string;
  x as number | string | boolean;
  x as X;
  x as X | string;
  x as X | boolean;
  x as number; // nope
  if (typeof x === 'number') {
    x as X;
    x as number;
    x as string; // nope
  }
  if (typeof y === 'boolean') {
    y as Y;
    y as X; // nope
  } else {
    y as X;
    y as boolean; // nope
  }
}
