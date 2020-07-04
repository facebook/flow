// @flow

type T = ?{
    m: number,
    n: number,
};

export type A = $NonMaybeType<T>;
