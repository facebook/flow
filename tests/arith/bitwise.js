//@flow

let z: bigint = 1n;

const unary_plus = [
  +'',
  +1,
  +1n,
]

const bitwise_bigint: bigint[] = [
  ~1n, // ok
  ~z, // ok
  1n & 2n, // ok
  z & z, // ok
  1n | 2n, // ok
  z | z, // ok
  1n ^ 4n, // ok
  z ^ z, // ok
  4n << 1n, // ok
  z << z,
  5n >> 2n, // ok
  z >> z,
  50n >>> 30n, // error
  z >>> z,
];

let x: bigint = 50n;

let s = [
  x &= 2n,
  x &= z,
  x |= 2n,
  x |= z,
  x ^= 4n,
  x ^= z,
  x <<= 1n,
  x <<= z,
  x >>= 5n,
  x >>= z,
  x >>>= 30n,
  x >>>= z,
];

let w: number = 1;

const bitwise_number: number[] = [
  ~1, // ok
  ~w, // ok
  ~'', // error
  1 & 2, // ok
  w & w,
  1 | 2, // ok
  w | w,
  1 ^ 4, // ok
  w ^ w,
  4 << 1, // ok
  w << w,
  5 >> 2, // ok
  w >> w,
  50 >>> 30, // ok
  w >>> w,
];

let y: number = 2;

y &= 2;
y |= 2;
y ^= 4;
y <<= 1;
y >>= 5;
y >>>= 30;
y >>= 2n;
