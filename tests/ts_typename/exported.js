export const a: ReadonlyArray<number> = [3]; // error: ts-syntax
export const b: ReadonlySet<number> = new Set([3]); // error: ts-syntax
export const c: ReadonlyMap<number, string> = new Map([[3, '']]); // error: ts-syntax
export const d: NonNullable<string | null> = ''; // error: ts-syntax
export const e: Readonly<{foo: string}> = {foo: ''}; // error: ts-syntax
