// @flow

declare const obj: {
    f: 'f',
    g: 'g',
 ...};

declare const k: Values<typeof obj>;

export type T = typeof k.length;
