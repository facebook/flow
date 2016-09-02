// @flow

export function mono(a: number, b: {c: number}) { return a + b.c; };
export function poly<T: number, U: T, V: U> (a: V) { return a; }
