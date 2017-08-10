// @flow
import React from 'react';

class Bar extends React.Component<void, {}> {}

class Foo extends React.Component<void, {}> {
  render() {
    const Cmp = Math.random() < 0.5 ? 'div' : Bar;
    return (<Cmp/>);
  }
}
