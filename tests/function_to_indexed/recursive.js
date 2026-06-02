//@flow
export type Value = void | null | boolean | number | string | Aggregate;

export type Aggregate = {
  readonly [string]: Value,
};

(() => {}) as Value;
