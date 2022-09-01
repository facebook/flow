// @flow
import * as React from 'react';

class Bar extends React.Component<{test: number}> {
  render(): React.Node {
    return (
      <div>
        {this.props.test}
      </div>
    )
  }
}

class Foo extends React.Component<{}> {
  render(): React.Node {
    const Cmp = Math.random() < 0.5 ? 'div' : Bar;
    return (<Cmp/>);
  }
}
