// @flow

// This test would cause a "recursion limit exceeded" error due to misses in the Eval
// cache. The reason for the miss was that we include the use_op of the EvalTypeDestructorT
// in the key, and were updating the use_op at some point during checking. This use_op
// should not be considered part of EvalTypeDestructorT, but rather part of the
// underlying destructor. The name of the field has bene changed to destructor_use_op
// to make this more clear.

import * as React from 'react';

export type PropsP<T> = {
  value: T,
  render: (value: $NonMaybeType<T>) => React.Node,
};

class Foo<T> extends React.Component<PropsP<T>> {
  static defaultProps: Partial<PropsP<T>> = {};
}

declare var id: ?string;

function LetterboxBusinessUnitInvariant() {
  return (
    <Foo
      value={id}
      render={(_: any) => {
        return (
          <Foo
            value={0}
            render={(_: any) => {}}
          />
        );
      }}
    />
  );
}
