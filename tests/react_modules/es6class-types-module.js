/* @flow */
import React from 'react';

type Props = {name: string};

class Hello extends React.Component {
  props: Props;
  static defaultProps: {};

  render(): React.Element {
    return <div>{this.props.name}</div>;
  }
}

module.exports = Hello;
