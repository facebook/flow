//@flow

function a<X extends {...}, Y extends {...}, Z extends {...}>(x: X, y: Y) {
  ({...x, ...y}) as {...X, ...}; // nope, which is different than old generics
  ({...y, ...x}) as {...X, ...}; // yup
  ({...x}) as {...Y, ...X, ...}; // nope
  ({...y, ...x}) as {...X, ...Y, ...}; // nope, which is different than old generics
  ({...x, ...y}) as {...X, ...Y, ...}; // yup
  ({...x, ...y}) as {...X, ...Y, ...Y, ...}; // yup
  ({...x, ...y}) as {...Y, ...X, ...Y, ...}; // yup
  ({...x, ...y}) as {...X, ...Z, ...Y, ...}; // nope
}

// interchanging between spread and basic representation
// with an object upper bound
function t1<X extends {a: number, ...}>(x: X, sp_x: {...X, ...}) {
  x as {...X, ...}; // ok
  sp_x as X; // ok
}

// interchanging between spread and basic representation
// with a mixed upper bound
function t2<X>(x: X, sp_x: {...X, ...}) {
  x as {...X, ...}; // nope
  sp_x as X; // this probably should be banned--imagine if X=number--but is currently allowed
}

// all below ok exept as noted
//(Bound _, Spread (id2, []))
function f1<X extends {...}>(x: X): {...X, ...} {
  return x;
}

//(Bound (_, Some id1), Spread _)
function f2<X extends {...}, Y extends {...}, Z extends {...X, ...Y, ...}>(z: Z): {...X, ...Y, ...} {
  return z;
}

//(Bound (_, None), Spread _)
function f3<X extends {...}, Y extends {...}>(y: Y): {...X, ...Y, ...} {
  return y; // should error
}

//(Spread ids, Bound _)
function f4a<X extends {...}, Y extends {...}>(y: {...X, ...Y, ...}): Y {
  return y;
}
function f4b<X extends {...}, Y extends {...}>(y: {...Y, ...X, ...}): Y {
  return y; // should be an error!
}

function f<X extends {...}, Y extends {...}, Z extends X & Y>(x: {...X, ...}, xy: {...X, ...Y, ...}, z: {...Z, ...}) {
  x as {...X & Y, ...}; // nope
  xy as {...X & Y, ...}; // ok
  z as {...X & Y, ...}; // ok
}
