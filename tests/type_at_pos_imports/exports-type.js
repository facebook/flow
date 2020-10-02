// @flow

export type MyUnionType = 'A' | 'B' | 'C';
export type MyStringType = 'A';
export type MyPolyType<T> = { f: T };

declare export function foo(): MyUnionType;

const f = foo();
declare var x: MyUnionType;
