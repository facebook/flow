import * as React from 'react';

declare class PolyComponent<T> extends React.Component<{foo: T}> {}

{
  declare var config: React.ElementConfig<typeof PolyComponent>; // T instantiated to mixed
  (config.foo: string); // error: mixed ~> string
}

{
  declare var props: React.ElementProps<typeof PolyComponent>; // T instantiated to mixed
  (props.foo: string); // error: mixed ~> string
}

{
  declare var ref: React.ElementRef<typeof PolyComponent>; // T instantiated to mixed
  (ref.props.foo: string); // error: mixed ~> string
}
