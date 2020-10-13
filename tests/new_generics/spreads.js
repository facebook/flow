//@flow

function a<X: {}, Y: {}, Z: {}>(x: X, y: Y) {
  ({...x, ...y}: {...X}); // nope, which is different than old generics
  ({...y, ...x}: {...X}); // yup
  ({...x}: {...Y, ...X}); // nope
  ({...y, ...x}: {...X, ...Y}); // nope, which is different than old generics
  ({...x, ...y}: {...X, ...Y}); // yup
  ({...x, ...y}: {...X, ...Y, ...Y}); // yup
  ({...x, ...y}: {...Y, ...X, ...Y}); // yup
  ({...x, ...y}: {...X, ...Z, ...Y}); // nope
}

// interchanging between spread and basic representation
// with an object upper bound
function t<X: {a: number}>(x: X, sp_x: {...X}) {
  (x: {...X}); // ok
  (sp_x: X); // ok
}

// interchanging between spread and basic representation
// with a mixed upper bound
function t<X>(x: X, sp_x: {...X}) {
  (x: {...X}); // nope
  (sp_x: X); // this probably should be banned--imagine if X=number--but is currently allowed
}

// all below ok exept as noted
//(Bound _, Spread (id2, []))
function f1<X: {}>(x: X): {...X} {
  return x;
}

//(Bound (_, Some id1), Spread _)
function f2<X: {}, Y: {}, Z: {...X, ...Y}>(z: Z): {...X, ...Y} {
  return z;
}

//(Bound (_, None), Spread _)
function f3<X: {}, Y: {}>(y: Y): {...X, ...Y} {
  return y; // should error
}

//(Spread ids, Bound _)
function f4a<X: {}, Y: {}>(y: {...X, ...Y}): Y {
  return y;
}
function f4b<X: {}, Y: {}>(y: {...Y, ...X}): Y {
  return y; // should be an error!
}

function f<X: {}, Y: {}, Z: X & Y>(x: {...X}, xy: {...X, ...Y}, z: {...Z}) {
  (x: {...(X & Y)}); // nope
  (xy: {...(X & Y)}); // ok
  (z: {...(X & Y)}); // ok
}
