import * as React from 'react';

declare class PolyComponent<T> extends React.Component<{foo: T, ...}> {}

{
  declare const config: React.ElementConfig<typeof PolyComponent>; // T instantiated to mixed
  config.foo as string; // error: mixed ~> string
}

{
  declare const ref: React.ElementRef<typeof PolyComponent>; // T instantiated to mixed
  ref.props.foo as string; // error: mixed ~> string
}
