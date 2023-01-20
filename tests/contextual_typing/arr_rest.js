// @flow

type t = [ Set<string> | Array<string> ];

declare var p: t;
const x: t =
[ [
    ...Array.from(p[0]), // No error
] ];

const [...y] = new Set() // still error

declare function arb<T>(): T;

const z: [number, number, number] = [1, ...arb()]; // still no error

const w: [number, number, string, number] = [...z, arb(), ...arb()]; // incompat and instantiation errors
