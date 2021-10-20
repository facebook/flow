// @flow


export type ROE = $ReadOnly<$Exact<{ f: string }>>;
declare var roe: ROE;

export const x = { ...roe, f: 1 };

declare var t: null | { a: number };
export const y = { ...t };

declare var o: { f: string } | {...};

export const z = {
  // $FlowExpectedError[exponential-spread]
  ...o, ...o
};
