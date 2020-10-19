//@flow

function f<X: number, Y: ?number, Z: ?X>(
  x: ?X,
  y: Y,
  z: Z,
  xu?: X,
  yu?: Y,
  zu?: Z,
) {
  (x: null | void | X);
  (x: ?X);
  (x: X | ?X);
  (x: ?Y | ?X);
  (x: X); //nope
  (x: ?number);
  if (typeof x === 'number') {
    (x: X);
    (x: ?X);
    (x: null | void); // nope
  }

  (y: null | void | number);
  (y: Y);
  (y: Y | X);
  (y: ?Y);
  (y: ?number);
  if (typeof y === 'number') {
    (y: Y);
    (y: number);
    (y: null | void); // nope
  }

  (z: null | void | Z);
  (z: null | void | X);
  (z: ?X);
  (z: ?Z);
  (z: X); // nope
  (z: Z);
  (z: ?number);
  if (typeof z === 'number') {
    (z: X);
    (z: ?X);
    (z: null | void); // nope
  }

  (xu: void | X);
  (xu: X | (X | void));
  (xu: ?Y | ?X);
  (xu: X); //nope
  (xu: void | number);
  if (typeof xu === 'number') {
    (xu: X);
    (xu: ?X);
    (xu: void); // nope
  }

  (yu: null | void | number);
  (yu: void | number); // nope
  (yu: Y); // nope
  (yu: Y | X); // nope
  (yu: ?Y);
  (yu: ?number);
  if (typeof yu === 'number') {
    (yu: Y);
    (yu: number);
    (yu: null | void); // nope
  }

  (zu: null | void | Z);
  (zu: null | void | X);
  (zu: ?X);
  (zu: ?Z);
  (zu: X); // nope
  (zu: Z | void);
  (zu: Z); // nope
  (zu: ?number);
  if (typeof zu === 'number') {
    (zu: X);
    (zu: ?X);
    (zu: null | void); // nope
  }

  // FilterOptionalT
  declare var a: {x?: X};
  var b = {};
  Object.assign(b, a);
  (b.x: X);
  (b.x: Y); //nope

  //FilterMaybeT
  declare var c: $NonMaybeType<X>;
  (c: X);
  (c: number);
  (c: null | void); // nope
}
