// @flow

import * as React from 'react';
import type {ChildrenArray} from 'react';

class Child extends React.Component<{
  value1: string,
}> {}

class Parent extends React.Component<{
  children: ChildrenArray<ExactReactElement_DEPRECATED<typeof Child>>,
}> {
  render(): React.Node {
    React.Children.map(this.props.children, child => {
      child as ExactReactElement_DEPRECATED<typeof Child>; // ok
    });
    return null;
  }
}
