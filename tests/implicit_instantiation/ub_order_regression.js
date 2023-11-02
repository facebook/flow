// @flow

import * as React from 'react';
declare var Comp: (props: {}) => $FlowFixMe;
class C<X> {}
declare var withStore: <Props: {...}>(C: React$ComponentType<Props>) => C<$Diff<Props, {...}>>;

(withStore(Comp): C<{}>); // okay
