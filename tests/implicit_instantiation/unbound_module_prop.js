//@flow

// When explicit type args with `_` reference a nonexistent module property,
// the error should not be suppressed.

import * as React from 'react';

declare function deferredLoadComponent<
  Config extends {...},
  Renders extends React.Node,
>(
  deferredResource: component(...Config) renders Renders,
): component(...Config) renders Renders;

declare component Foo();

deferredLoadComponent<React.MISSING, _>(Foo); // Error: MISSING is not a property of react
deferredLoadComponent<React.MISSING<1>, _>(Foo); // Error: MISSING is not a property of react

declare function bar<A extends {...}, B extends React.Node>(x: A, y: B): void;
bar<React.MISSING, _>({}, null); // Error
bar<_, React.MISSING>({}, null); // Error
