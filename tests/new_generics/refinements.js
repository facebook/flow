//@flow

class C {
  x: number;
}

function f<X>(x: X) {
  if (typeof x === 'number') {
    x as X;
    x as number;
    x as string; // nope
  }
  x as number; // nope
  var c = C;
  if (x instanceof C) {
    x as C;
    x as X;
    x as string; // nope
  }
  x as C; // nope
}
