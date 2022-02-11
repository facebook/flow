// @flow

declare var obj: {
    f: 'f',
    g: 'g',
};

declare var k: $Values<typeof obj>;

export type T = typeof k.length;
