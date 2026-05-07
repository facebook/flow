import * as React from 'react';

function create<P extends any>(
  Component: React.ComponentType<{...P, ...{...}, ...}>,
): React.ComponentType<P> {
  return Component;
}

export type Props = {
  x: {...},
 ...};

function create1<P extends any>(
  Component: React.ComponentType<P & Props>,
): React.ComponentType<P> {
  return Component;
}

class Foo extends React.Component<Props> {}

const Foo1 = create(create1<{}>(Foo));
<Foo1 />;
