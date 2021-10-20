// @flow

class C<X> {}

export type T = {
  // $FlowFixMe[missing-type-arg]
  c: C,
};
