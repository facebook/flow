// @flow

var React = require('react');

type Props = $ReadOnly<{x: number, y: string}>;

const C: React.AbstractComponent<Props> = (props: Props) => {};
<C  // space
// ^
