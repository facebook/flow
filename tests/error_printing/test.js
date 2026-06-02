//@flow

const React = require('react');

type Props = {
  readonly a: boolean,
  readonly b: boolean,
  readonly c: boolean,
  readonly d: boolean,
  readonly e : boolean,
  readonly f: boolean,
  readonly g: boolean,
  readonly h: boolean,
};
declare function Foo(props: Props): React.Node;

<Foo
  a={3}
  b={true}
  c={true}
  d={true}
  e={true}
  f={true}
  g={true}
  h={true}
/>
