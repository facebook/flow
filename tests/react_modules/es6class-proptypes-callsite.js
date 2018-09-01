/* @flow */
import React from 'react';
import Hello from './es6class-proptypes-module';
import type {Node} from 'react';

class HelloLocal extends React.Component<{name: string}> {
  defaultProps = {};
  propTypes = {
    name: React.PropTypes.string.isRequired,
  };
  render() {
    return <div>{this.props.name}</div>;
  }
}

class Callsite extends React.Component<{}> {
  render() {
    return (
      <div>
        <Hello />
        <HelloLocal />
      </div>
    );
  }
}

module.exports = Callsite;
