// @flow

import React from 'react';

class MyComponent extends React.Component<DefaultProps, Props, State> {
  static defaultProps: BadDefaultProps = {};
  props: BadProps;
  state: BadState = {};
}

const expression = () =>
  class extends React.Component<DefaultProps, Props, State> {
    static defaultProps: BadDefaultProps = {};
    props: BadProps;
    state: BadState = {};
  }
