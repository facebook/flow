// @flow

export type Enum = 'a' | 'b' | 'c';

export type Obj = {
  f0?: ?Enum,
};

export type Props1 = {
  ...Obj,
  ...
};

type Props2 = {
  ...$Exact<Props1>,
  ...Obj,
  ...
};

declare var props: Props1;
(props: Props2);
