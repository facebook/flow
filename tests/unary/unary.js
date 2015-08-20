/* @flow */

function x(y: string): number {
  return +y; // ok, + exists solely for coersion
}

function x(y: string): number {
  return -y; // error, we don't allow coersion here
}

function x(y: string) {
  y++; // error, we don't allow coersion here
  y--; // error, we don't allow coersion here
  ++y; // error, we don't allow coersion here
  --y; // error, we don't allow coersion here
  ~y;  // error, we don't allow coersion here
}

function x(y: string): boolean {
  return !y; // ok, coersion is allowed
}
