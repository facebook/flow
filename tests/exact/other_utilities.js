/* @flow */

type DeepNestedMaybes = {|
  a: ?{|
    b: ?string,
  |},
  c: {|
    d: ?{|
      e: ?number,
    |},
  |},
  g: null,
|};

type ExtractNonNull<T> = $NonMaybeType<T>;

type G = ExtractNonNull<DeepNestedMaybes['g']>;

type C = ExtractNonNull<DeepNestedMaybes['c']>;
let c1: C = {
  f: '', // Should cause an error
  d: null,
};
let c2: $Exact<C> = {
  f: '',
  d: null,
};
