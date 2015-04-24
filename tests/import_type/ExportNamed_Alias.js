/**
 * @flow
 */

export type AliasFoo3  = {
  givesANum(): number
};
export type AliasFoo4 = {
  givesAStr(): string
};
export function givesAFoo3(): AliasFoo3 {
  return {
    givesANum(): number { return 42; }
  };
};
