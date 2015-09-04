/* @flow */

function x0(y: string): number {
  return +y; // ok, + exists solely for coersion
}

function x1(y: string): number {
  return -y; // error, we don't allow coersion here
}

function x2(y: string) {
  y++; // error, we don't allow coersion here
  y--; // error, we don't allow coersion here
  ++y; // error, we don't allow coersion here
  --y; // error, we don't allow coersion here
  ~y;  // error, we don't allow coersion here
}

function x3(y: string): boolean {
  return !y; // ok, coersion is allowed
}
