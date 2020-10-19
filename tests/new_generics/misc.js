//@flow

function a<X: number>(x: X) {
  // AdderT
  (x + x: X); // nope
  (x + x: number);
  // UnaryMinusT
  (-x: X); // nope
  (-x: number);
  // AssertArithOperandT
  (x * x: X);
  (x * x: number);
  // coercion
  (`blah ${x}`: string);
}

function b<X: number, Y: string>(x: X, y: Y) {
  //EqT, StrictEqT, CompartatorT
  (x == x: boolean);
  (x === x: boolean);
  (x == y: boolean); // nope
  (x === y: boolean);
  (x < x: boolean);
  (x < y: boolean); // nope
}

function c<S: string, X: {[string]: mixed}, Y: Array<number>>(
  s: S,
  x: X,
  y: Y,
) {
  // AssertForInRHST
  for (const _ in x) {
  }
  // AssertBinaryIn[L|R]HST
  s in x;
  42 in y;
}

// ShapeT
function d<X: {a: number}>(x: X): $Shape<{a: number}> {
  return x;
}

// MatchingPropT
function e<X: {a: 'T'} | {a: 'S'}>(x: X) {
  if (x.a === 'T') {
  }
}

//TestPropT
function f<X>(x: Array<X>) {
  if (x[0] != null && x[0].id === 'a') {
    // This seems questionable but models current behavior for mixed
  }
}

// fast path for TupleMap
function h<X: [number]>(x: X): $TupleMap<X, (number) => string> {
  return ['a']; // existing unsoundness
}
