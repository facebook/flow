// @flow

import React from 'react';

class MyComponent1 extends React.Component {
  constructor(props) {}

  defaultProps: T;
  static props: T;
  static state: T;
  a: T;
  b = 5;
  c: T = 5;
  method() {}
}

class MyComponent2 extends React.Component {
  props: Props;

  constructor(props) {}

  defaultProps: T;
  static props: T;
  static state: T;
  a: T;
  b = 5;
  c: T = 5;
  method() {}
}

const expression1 = () =>
  class extends React.Component {
    constructor(props) {}

    defaultProps: T;
    static props: T;
    static state: T;
    a: T;
    b = 5;
    c: T = 5;
    method() {}
  }

const expression2 = () =>
  class extends React.Component {
    props: Props;

    constructor(props) {}

    defaultProps: T;
    static props: T;
    static state: T;
    a: T;
    b = 5;
    c: T = 5;
    method() {}
  }
