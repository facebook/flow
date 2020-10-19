//@flow

declare function takes_num_arr(n: number, ...x: Array<number>): void;

function f<X: Array<number>, R>(fn: (...X) => R, x: X, arr: Array<number>) {
  (fn(...x): R);
  (fn(...arr): R); // nope

  takes_num_arr(...x);
}

function g<X: Array<number>>(A: X): X {
  var B = [...A];
  if ((A: any)) {
    return B;
  } else {
    return [...B, 42]; // nope
  }
}

function h<X: Array<string>>(x: X) {
  var y: Array<string> = ['3', ...x];
  var z: X = ['3', ...x]; // nope
}

function w<A: [number, number]>(x: A, f: (number, ...A) => void) {
  f(...x); // nope
  f(42, 42, 42); // nope
  f(42, ...x);
  f(42, ...x, ...x); // nope
}
function w2<A: Array<number>>(x: A, f: (number, ...A) => void) {
  f(...x);
  f(42, 42, 42); // nope
  f(42, 42, 42, ...x); // should be nope
  f(42, ...x);
  f(42, ...x, ...x);
}
function w3<A: Array<number>>(x: A, f: A => void) {
  f([42, 42, 42]); // nope
  f([42, ...x]); // should be nope
  f([...x]);
  f([...x, ...x]);
}

function l<X: $ReadOnlyArray<number>, Y: X>(x: X, y: Y) {
  ([...x, ...y]: Y); // should be nope
  ([...x, ...y]: X); // should be nope
}

function l2<X: Array<number>, Y: X>(x: X, y: Y) {
  ([...x, ...y]: Y); // should be nope
  ([...x, ...y]: X); // ok
}

function a<X: $ReadOnlyArray<number>>(x: X): X {
  return [...x];
}

function b<X: $ReadOnlyArray<number>>(x: X): X {
  return [...x, ...x]; // nope, because X could be a tuple
}

function c<X: $ReadOnlyArray<number>>(x: X): X {
  return [42, ...x]; // error
}

function d<X: [number, number]>(x: X): X {
  return [...x, ...x]; // error
}
