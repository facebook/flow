// @flow

export type MyUnionType = 'A' | 'B' | 'C';

declare export function foo(): MyUnionType;

const f = foo();
declare var x: MyUnionType;
