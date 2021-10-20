// @flow

declare var obj: {
    [key: string]: number,
    f: boolean,
    g: string,
};

declare var k: $Keys<typeof obj>;

export type T = typeof k.length;
