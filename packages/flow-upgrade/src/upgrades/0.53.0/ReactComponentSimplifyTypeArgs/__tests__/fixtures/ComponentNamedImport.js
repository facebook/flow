// @flow

import {Component} from 'react';

class MyComponent extends Component<DefaultProps, Props, State> {
  static defaultProps: DefaultProps = {};
  state: State = {};

  defaultProps: T;
  static props: T;
  static state: T;
  a: T;
  b = 5;
  c: T = 5;
  method() {}
}

const expression = () =>
  class extends Component<DefaultProps, Props, State> {
    static defaultProps: DefaultProps = {};
    state: State = {};

    defaultProps: T;
    static props: T;
    static state: T;
    a: T;
    b = 5;
    c: T = 5;
    method() {}
  }
