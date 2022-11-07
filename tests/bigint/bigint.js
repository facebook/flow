// @flow

let x = 10n;

let y: bigint = 10n;

let z: 10n = 10n;

x = y; // ok, bigint <: bigint

y = z; // ok, 10n <: bigint

z = x; // error, bigint </: 10n
