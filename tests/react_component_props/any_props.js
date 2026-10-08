import * as React from 'react';

// $FlowFixMe[unclear-type]
declare class AnyProps extends React.Component<any> {}

declare const viaComponentProps: React.ComponentProps<typeof AnyProps>;

viaComponentProps as {a: number}; // ok
