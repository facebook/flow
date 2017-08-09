// @flow

const React = require('react');

class MyComponent1 extends React.Component {
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

class MyComponent2 extends Component {
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
