import * as React from 'react';
type Props = React.PropsOf<A>;
component A(foo: string, bar: number, ref: React.RefSetter<AInstance>) {
  return <div />;
}
component B(...props: Props) {
  let { foo, bar } = props;
  (foo: empty); // foo is string
  (bar: empty); // bar is number
  return <div />;
}
type IntrinsicProp = React.PropsOf<'div'>;
declare const intrinsicProp: IntrinsicProp;
(intrinsicProp.children: empty); // children is ?React.Node

type Str = React.PropOf<A, 'foo'>;
const s: Str = 42; // Str is string
type PStringBoolNumber = React.PropOf<'meta', 'data-'>;
let p: PStringBoolNumber = 42;
if (s) {
  p = 'a';
} else {
  (p: ?(boolean | number | string)); // all ok
}

component C(ref: React.RefSetter<React.RefOf<A>>) {
  let a = <a ref={ref} />; // ok, AInstance
  let b = <meta ref={ref} />; // error
  return null;
}
