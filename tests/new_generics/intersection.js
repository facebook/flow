//@flow

function f1<X: number & string, Y: X & boolean>(x: X, y: Y) {
  x as number & string;
  x as number;
  x as X;
  y as X;
  x as number & string & boolean; // nope
}
