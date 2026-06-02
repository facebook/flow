// @flow

export type Foo = {
  readonly f?: {
    readonly g: {
      readonly h: ?ReadonlyArray<{
        readonly i: {},
      }>,
    },
  },
};

declare export function foo(): Foo;

export type Bar = {
  readonly f: {
    readonly g: {
      readonly h: ReadonlyArray<{
        readonly i: {},
      }>,
    },
  },
};

declare export function bar(): Bar;
