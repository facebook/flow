// @flow

var React = require('react');

type Props = $ReadOnly<{x: number, y: string}>;

const C: React.ComponentType<Props> = (props: Props) => {};
<C  // space
// ^
