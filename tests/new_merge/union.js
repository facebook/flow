// @flow

declare class C1 {
  f: number;
}

declare class C2 {
  f: string;
}

declare const c: C1 | C2;

declare export const x: c; // error: value-as-type
