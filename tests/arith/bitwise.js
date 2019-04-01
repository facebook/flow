//@flow

const bitwise_bigint = [
  ~1n, // ok
  1n & 2n, // ok
  1n | 2n, // ok
  1n ^ 4n, // ok
  4n << 1n, // ok
  5n >> 2n, // ok
  50n >>> 30n // error
];

let x: bigint = 50n;

x &= 2n; // TODO: incorrect
x |= 2n; // TODO: incorrect
x ^= 4n; // TODO: incorrect
x <<= 1n; // TODO: incorrect
x >>= 5n; // TODO: incorrect
x >>>= 30n; // TODO: incorrect

const bitwise_number = [
  ~1, // ok
  1 & 2, // ok
  1 | 2, // ok
  1 ^ 4, // ok
  4 << 1, // ok
  5 >> 2, // ok
  50 >>> 30 // ok
];

let y: number = 2;

y &= 2;
y |= 2;
y ^= 4;
y <<= 1;
y >>= 5;
y >>>= 30;
y >>= 5n; // error
