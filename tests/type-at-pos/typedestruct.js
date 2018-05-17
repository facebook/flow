// @flow

type C = {|
  f: ?string,
|};

type D = {|
  f: $PropertyType<C, 'f'>,
|};
