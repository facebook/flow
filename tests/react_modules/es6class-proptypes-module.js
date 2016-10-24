/* @flow */
import React from 'react';

class Hello extends React.Component {
  render(): React.Element {
    return <div>{this.props.name}</div>;
  }
}

Hello.propTypes = {
  name: React.PropTypes.string.isRequired,
};

Hello.defaultProps = {};

module.exports = Hello;
