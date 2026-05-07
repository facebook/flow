// @flow

import * as React from 'react';
declare const Comp: (props: {}) => $FlowFixMe;
class C<X> {}
declare const withStore: <Props extends {...}>(
  C: React.ComponentType<Props>,
) => C<Omit<Props, empty>>;

withStore(Comp) as C<{}>; // okay
