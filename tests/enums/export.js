// @flow

export enum E {
  A,
  B,
}

export default enum D {
  A,
  B,
}

export const F = E;

export enum U {
  A,
  B,
  ...
}

// Enum<>
declare const X: Enum<E>;
export const x = X.A;
