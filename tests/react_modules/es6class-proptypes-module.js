/* @flow */
import React from 'react';

class Hello extends React.Component {
  static defaultProps: {};

  render(): React.Element {
    return <div>{this.props.name}</div>;
  }
}

Hello.propTypes = {
  name: React.PropTypes.string.isRequired,
};

module.exports = Hello;
