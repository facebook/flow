//@flow

var x = 42;

function f1<X>(a: X): X {
  x = a; // error, X ~> escape
  return a;
}

function tostring(x): string {
  if (typeof x === 'string') return x;
  return 'a';
}

function h<X>(x: X, a: X => string = (name: X) => tostring(name)): string {
  // errors, X ~> x and X ~> refinement
  return a(x);
}

declare function ng(value: mixed): mixed;
class ObjectUtils {
  static stableCopy<T: mixed>(value: T): T {
    return ng(value); // no escape errors
  }
}

function makeEqualFn<T>(customEqual?: (T, T) => boolean): (T, T) => boolean {
  return customEqual
    ? function(a, b) {
        return a === b || customEqual(a, b);
      }
    : (42: any);
}

function o<X>(x: X) {
  // error, X ~> return
  return x;
}

var a = 42;

function f2<X>(b: boolean): X => X {
  if (b) {
    a = (x: X) => x;
  }
  if (typeof a === 'number') {
    return (x: X) => x;
  } else {
    return a;
  }
}

var xa = [];
function fa<T>(a: T, b: boolean): T {
  if (b) {
    xa[0] = a;
  }
  return xa[0];
}


function hh<X>({a}: {a: X}): X {
  return a;
}
