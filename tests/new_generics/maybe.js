//@flow

function f<X: number, Y: ?number, Z: ?X>(
  x: ?X,
  y: Y,
  z: Z,
  xu?: X,
  yu?: Y,
  zu?: Z,
) {
  x as null | void | X;
  x as ?X;
  x as X | ?X;
  x as ?Y | ?X;
  x as X; //nope
  x as ?number;
  if (typeof x === 'number') {
    x as X;
    x as ?X;
    x as null | void; // nope
  }

  y as null | void | number;
  y as Y;
  y as Y | X;
  y as ?Y;
  y as ?number;
  if (typeof y === 'number') {
    y as Y;
    y as number;
    y as null | void; // nope
  }

  z as null | void | Z;
  z as null | void | X;
  z as ?X;
  z as ?Z;
  z as X; // nope
  z as Z;
  z as ?number;
  if (typeof z === 'number') {
    z as X;
    z as ?X;
    z as null | void; // nope
  }

  xu as void | X;
  xu as X | (X | void);
  xu as ?Y | ?X;
  xu as X; //nope
  xu as void | number;
  if (typeof xu === 'number') {
    xu as X;
    xu as ?X;
    xu as void; // nope
  }

  yu as null | void | number;
  yu as void | number; // nope
  yu as Y; // nope
  yu as Y | X; // nope
  yu as ?Y;
  yu as ?number;
  if (typeof yu === 'number') {
    yu as Y;
    yu as number;
    yu as null | void; // nope
  }

  zu as null | void | Z;
  zu as null | void | X;
  zu as ?X;
  zu as ?Z;
  zu as X; // nope
  zu as Z | void;
  zu as Z; // nope
  zu as ?number;
  if (typeof zu === 'number') {
    zu as X;
    zu as ?X;
    zu as null | void; // nope
  }

  // FilterOptionalT
  declare var a: {x?: X};
  var b = {...a};
  b.x as X | void;
  b.x as Y | void; //nope

  //FilterMaybeT
  declare var c: NonNullable<X>;
  c as X;
  c as number;
  c as null | void; // nope
}
