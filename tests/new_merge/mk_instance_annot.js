// @flow

declare class C {
  m(): void;
}

declare var x: typeof C;

export type T = x;
