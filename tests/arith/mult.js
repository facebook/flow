/* @flow */

function num(x: number) {}

function bignum(x: bigint) {}

num(null * 1);
num(1 * null);

bignum(null * 1n);
bignum(1n * null);

let x: number = 2 * 3;
x *= 4;

let y: string = "123";
y *= 2; // error

let z: bigint = 2n * 3n;
z *= 4n; // TODO: incorrect
