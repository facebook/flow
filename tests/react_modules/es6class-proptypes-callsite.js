/* @flow */
import React from 'react';
import Hello from './es6class-proptypes-module';

class HelloLocal extends React.Component {
  static defaultProps: {};

  render(): React.Element {
    return <div>{this.props.name}</div>;
  }
}

HelloLocal.propTypes = {
  name: React.PropTypes.string.isRequired,
};

class Callsite extends React.Component {
  render(): React.Element {
    return (
      <div>
        <Hello />
        <HelloLocal />
      </div>
    );
  }
}

module.exports = Callsite;
