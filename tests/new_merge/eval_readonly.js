// @flow

export type U = { f: number } | { f: string };

declare var u: $ReadOnly<U>;

export const x = u.f;

type O = { f: string };

export type ROE = $ReadOnly<$Exact<O>>;
declare var roe: ROE;

const s = { ...roe, f: 1 };
export const y = s.f;
export const z = roe.f;
