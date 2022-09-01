// @flow
import * as React from 'react';

class Bar extends React.Component<{}> {}

class Foo extends React.Component<{}> {
  render(): React.Node {
    const Cmp = Math.random() < 0.5 ? 'div' : Bar;
    return (<Cmp/>);
  }
}
