// @flow

let x = 10n;

let y: bigint = 10n;

let z: 10n = 10n;

x = y; // ok, bigint <: bigint

y = z; // ok, 10n <: bigint

z = x; // error, bigint </: 10n

(BigInt(3): bigint); // ok
(BigInt("3"): bigint); // ok
(BigInt.asIntN(3, y): bigint); // ok
(BigInt.asUintN(3, y): bigint); // ok
(x.valueOf(): bigint); // ok
(BigInt(3n): bigint); // ok

BigInt(null); // error

declare var foo: mixed;
if (typeof foo === "bigint") {
    (foo : bigint);
    (foo : empty); // error
}

type U = { x: 0n, a: "foo" } | { x: 1n, a: "bar" };
declare var bar: U;
if (bar.x === 0n) {
    (bar.a : "foo");
} else {
    (bar.a : "bar");
}

(0n < 0n : boolean); // ok
(0n > ""); // error

declare var b: ?bigint;
if (b) {} // error
