import * as React from 'react';

declare function viaComponentProps<P extends {...} = {...}>(
  c: component(...React.ComponentProps<component(...P)>),
): void;

declare component Foo(name: string);

viaComponentProps(Foo); // ok
