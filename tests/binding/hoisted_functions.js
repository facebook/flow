// @flow

// All forms of functions are hoisted

f1;
f2;
f3;
f4;

function f1() {}
export function f2() {}
export default function f3() {}
declare export function f4(): void
