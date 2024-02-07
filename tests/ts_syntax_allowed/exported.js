export const a: ReadonlyArray<number> = [3]; // ok
export const b: ReadonlySet<number> = new Set([3]); // ok
export const c: ReadonlyMap<number, string> = new Map([[3, '']]); // ok
export const d: NonNullable<string | null> = ''; // ok
export const e: Readonly<{foo: string}> = {foo: ''}; // ok
export function f<T extends string>(t: T): void {} // ok
