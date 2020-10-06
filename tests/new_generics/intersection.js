//@flow

function f1<X: number & string, Y: X & boolean>(x: X, y: Y) {
  (x: number & string);
  (x: number);
  (x: X);
  (y: X);
  (x: number & string & boolean); // nope
}
