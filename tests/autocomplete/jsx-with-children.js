// @flow

var React = require('react');

type Props = $ReadOnly<{x: number, children: React.Node}>;

function C(props: Props): React.Node {
  return props.children;
}
<C // space
