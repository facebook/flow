//@flow

const React = require('react');

type Props = {|
  +a: boolean,
  +b: boolean,
  +c: boolean,
  +d: boolean,
  +e : boolean,
  +f: boolean,
  +g: boolean,
  +h: boolean,
|};
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
