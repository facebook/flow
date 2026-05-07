// @flow

declare const obj: {
    [key: string]: number,
    f: boolean,
    g: string,
};

declare const k: keyof typeof obj;

export type T = typeof k.length;

declare const exact_intersection: $Exact<{ f: string, ... } & { g: number, ... }>;
export const f = exact_intersection.f;
f as empty; // error
exact_intersection as {f: string, g: number}; // ok
