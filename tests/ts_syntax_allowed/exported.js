export const a: ReadonlyArray<number> = [3]; // ok
export const b: ReadonlySet<number> = new Set([3]); // ok
export const c: ReadonlyMap<number, string> = new Map([[3, '']]); // ok
export const d: NonNullable<string | null> = ''; // ok
export const e: Readonly<{foo: string}> = {foo: ''}; // ok
export function f<T extends string>(t: T): void {} // ok

export class Covariant<out T> {
    readonly prop: T;
    -bad: T; // error
}
export class Contravariant<in T> {
    -prop: T;
    +bad: T; // error
}
export class Invariant<in out T> {
    prop: number;
}
export type RO<out T> = {readonly [K in keyof T]: T[K]};

class A {
    foo: string = '';
}

class B extends A {
    bar: string = '';
}

export type K = keyof B;

declare export const readonly_tuple: readonly [string, number];
declare export const readonly_array: readonly string[];
