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

function x9(): number {
  return -10; // ok
}

function x10(y: number): number {
  return -y; // ok
}

function x11(y: number): number {
  return +y; // ok
}

function x12(y: number): number {
  return ~y; // ok
}

function x13(): bigint {
  return -10n; // ok
}

function x14(y: bigint): bigint {
  return -y; // ok
}

function x15(y: bigint) {
  return +y; // error, bigint cannot be coerced to number
}

function x16(y: bigint): bigint {
  return ~y; // ok
}

function x17(y: any) {
  (+y: number); // ok, + coerces to number
  (+y: bigint); // error, bigint ~> number
  (-y: number); // ok, any
  (-y: bigint); // ok, any
  (~y: number); // ok, any
  (~y: bigint); // ok, any
}

function x18(y: empty) {
  (+y: empty);
  (-y: empty);
  (~y: empty);
}
