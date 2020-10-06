//@flow

class C {
  x: number;
}

function f<X>(x: X) {
  if (typeof x === 'number') {
    (x: X);
    (x: number);
    (x: string); // nope
  }
  (x: number); // nope
  var c = C;
  if (x instanceof C) {
    (x: C);
    (x: X);
    (x: string); // nope
  }
  (x: C); // nope
}
