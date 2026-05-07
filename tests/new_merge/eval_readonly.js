// @flow

export type U = { f: number, ... } | { f: string, ... };

declare const u: Readonly<U>;

export const x = u.f;

type O = { f: string, ... };

export type ROE = Readonly<$Exact<O>>;
declare const roe: ROE;

const s = { ...roe, f: 1 };
export const y = s.f;
export const z = roe.f;
