/* @flow */

var React = require('react');

type DefaultProps = {
  foo: number,
}

type Props = {
  foo: number,
}

type State = {}

class MyReactThing extends React.Component<DefaultProps, Props, State> {
  getFoo(): number { return this.props.foo; }
}

<MyReactThing />; // works
<MyReactThing foo={undefined} />; // also works
