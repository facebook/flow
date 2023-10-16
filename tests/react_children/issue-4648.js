// @flow

import * as React from 'react';
import type {ChildrenArray, Element} from 'react';

class Child extends React.Component<{
  value1: string
}> {}

class Parent extends React.Component<{
  children: ChildrenArray<Element<typeof Child>>
}> {
  render(): React.Node {
    React.Children.map(this.props.children, (child) => {
      ((child: Element<typeof Child>)); // ok
    });
    return null;
  }
}
