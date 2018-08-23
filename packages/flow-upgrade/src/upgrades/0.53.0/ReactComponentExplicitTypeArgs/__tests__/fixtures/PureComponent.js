// @flow

import React from 'react';

class MyComponent extends React.PureComponent {
  static defaultProps: DefaultProps = {};
  props: Props;
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
  class extends React.PureComponent {
    static defaultProps: DefaultProps = {};
    props: Props;
    state: State = {};

    defaultProps: T;
    static props: T;
    static state: T;
    a: T;
    b = 5;
    c: T = 5;
    method() {}
  }
