// @flow

var React = require('react');

type Props = Readonly<{x: number, y: string}>;

const C: React.ComponentType<Props> = (props: Props) => {};
<C  // space
// ^
