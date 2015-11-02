/* @flow */
import React from 'react';
import Hello from './es6class-proptypes-callsite';

type Props = {name: string};

class HelloLocal extends React.Component {
  props: Props;
  static defaultProps: {};

  render(): React.Element {
    return <div>{this.props.name}</div>;
  }
}

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
