// @flow

import React from 'react';

class MyComponent extends React.Component {
  constructor() {}

  defaultProps: T;
  static state: T;
  a: T;
  b = 5;
  c: T = 5;
  method() {}
}

const expression = () =>
  class extends React.Component {
    constructor() {}

    defaultProps: T;
    static state: T;
    a: T;
    b = 5;
    c: T = 5;
    method() {}
  }
