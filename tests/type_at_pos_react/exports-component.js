// @flow

const React = require('react');

type Props = {| foo: number |};

declare function Component(x: Props): React.Node;
declare function Component1(x: Props): React.Element<typeof Component>;

export default Component1;
