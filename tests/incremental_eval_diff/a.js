// @flow

type T = ?{
    m: number,
    n: number,
};

export type A = NonNullable<T>;
