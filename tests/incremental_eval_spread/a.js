// @flow

type T = {
    t: number,
}

export type A = {
    ...T,
    f: string,
    ...
}
