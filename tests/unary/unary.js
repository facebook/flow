/* @flow */

function x0(y: string): number {
  return +y; // ok, + exists solely for coercion
}

function x1(y: string): number {
  return -y; // error, we don't allow coercion here
}

function x3(y: string) {
  return ~y;  // error, we don't allow coercion here
}

function x4(y: string): boolean {
  return !y; // ok, coercion is allowed
}

(-1: void); // error, number ~> void

type A<X> = X;

function x5(a: A<false>): true {
  return !a; // ok
};

function x6(a: A<false>): false {
  return !a; // error, true ~> false
};

function x7(a: false & false): true {
  return !a; // ok
};

function x8(a: false & false): false {
  return !a; // error, true ~> false
};
