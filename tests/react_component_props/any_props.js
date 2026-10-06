import * as React from 'react';

// $FlowFixMe[unclear-type]
declare class AnyProps extends React.Component<any> {}

declare const viaElementConfig: React.ElementConfig<typeof AnyProps>;
declare const viaComponentProps: React.ComponentProps<typeof AnyProps>;

viaElementConfig as {a: number}; // ok
viaComponentProps as {a: number}; // ok
