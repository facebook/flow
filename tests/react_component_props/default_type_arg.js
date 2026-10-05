import * as React from 'react';

declare function viaElementConfig<P extends {...} = {...}>(
  c: component(...React.ElementConfig<component(...P)>),
): void;
declare function viaComponentProps<P extends {...} = {...}>(
  c: component(...React.ComponentProps<component(...P)>),
): void;

declare component Foo(name: string);

viaElementConfig(Foo); // ok
viaComponentProps(Foo); // ok
