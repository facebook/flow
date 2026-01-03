// @flow

var React = require('react');

type Props = Readonly<{x: number, y: string}>;

class C extends React.Component<Props> {
}
<C  // space
// ^
