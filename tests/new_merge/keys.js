// @flow

declare var obj: {
    [key: string]: number,
    f: boolean,
    g: string,
};

declare var k: $Keys<typeof obj>;

export type T = typeof k.length;

declare var exact_intersection: $Exact<{ f: string, ... } & { g: number, ... }>;
export const f = exact_intersection.f;
f as empty; // error
exact_intersection as {|f: string, g: number|}; // ok
