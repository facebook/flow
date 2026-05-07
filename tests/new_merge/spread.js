// @flow


export type ROE = Readonly<{ f: string }>;
declare const roe: ROE;

export const x = { ...roe, f: 1 };

declare const t: null | { a: number, ... };
export const y = { ...t };

declare const o: { f: string, ... } | {...};

export const z = {
  // $FlowExpectedError[exponential-spread]
  ...o, ...o
};
