// @flow

type T = {
    m: number,
    n: string,
}

export type A = $PropertyType<T, 'm'>;
