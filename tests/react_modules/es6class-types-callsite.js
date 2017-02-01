/* @flow */
import React from 'react';
import Hello from './es6class-types-module';

type Props = {name: string};

class HelloLocal extends React.Component<void, Props, void, void> {
  props: Props;

  render(): React.Element<*> {
    return <div>{this.props.name}</div>;
  }
}

class Callsite extends React.Component<void, Props, void, void> {
  render(): React.Element<*> {
    return (
      <div>
        <Hello />
        <HelloLocal />
      </div>
    );
  }
}

module.exports = Callsite;
