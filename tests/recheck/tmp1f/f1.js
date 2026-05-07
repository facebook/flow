// @flow

type T = { x: number, ... };
type S = { x: string, ... };

declare const a: T;
declare const b: S;
declare const c: S;

module.exports = { a, b, c };
