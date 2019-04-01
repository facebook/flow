/* @flow */

let x: number = 2 ** 3;
x **= 4;

let y: string = "123";
y **= 2; // error

let z: bigint = 2n ** 3n;
z **= 4n; // TODO: incorrect

1 + 2 ** 3 + 4;
2 ** 2;
(-2) ** 2;

1n + 2n ** 3n + 4n;
2n ** 2n;
(-2n) ** 2n;
