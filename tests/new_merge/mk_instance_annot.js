// @flow

declare class C {
  m(): void;
}

declare const x: typeof C;

export type T = x;
