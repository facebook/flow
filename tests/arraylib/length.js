/* @flow */

declare const a: Array<number>;
a.length = 5;

declare const r: ReadonlyArray<number>;
r.length = 6; //ng

declare const t: [number];
t.length = 7; //ng
