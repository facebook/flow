/* @flow */
import React from 'react';
import Hello from './es6class-types-module';
import type {Node} from 'react';

type Props = {name: string};

class HelloLocal extends React.Component<Props> {
  render(): Node {
    return <div>{this.props.name}</div>;
  }
}

class Callsite extends React.Component<Props> {
  render(): Node {
    return (
      <div>
        <Hello />
        <HelloLocal />
      </div>
    );
  }
}

module.exports = Callsite;
