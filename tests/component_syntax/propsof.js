import * as React from 'react';
type Props = React.PropsOf<A>;
component A(foo: string, bar: number) {
  return <div />;
}
component B(...props: Props) {
  let { foo, bar } = props;
  (foo: empty); // foo is string
  (bar: empty); // bar is number
  return <div />;
}
type IntrinsicProp = React.PropsOf<'meta'>;
declare const intrinsicProp: IntrinsicProp;
(intrinsicProp.children: empty); // children is ?React.Node
