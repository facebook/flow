// @flow

export default function<T: number, U: T, V: U> (a: V) { return a; }
export function mono(a: number, b: {c: number}) { return a + b.c; };
