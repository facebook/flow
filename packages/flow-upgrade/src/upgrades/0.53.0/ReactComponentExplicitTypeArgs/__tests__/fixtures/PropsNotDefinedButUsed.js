// @flow

import React from 'react';

class MyComponent1 extends React.Component {
  state: State = {
    initialValue: this.props.value,
  };
}

class MyComponent2 extends React.Component {
  render() {
    return this.props.children;
  }
}

class MyComponent3 extends React.Component {
  render() {
    const props = {};
    return props;
  }
}

const expression1 = () =>
  class extends React.Component {
    state: State = {
      initialValue: this.props.value,
    };
  }

const expression2 = () =>
  class extends React.Component {
    render() {
      return this.props.children;
    }
  }

const expression3 = () =>
  class extends React.Component {
    render() {
      const props = {};
      return props;
    }
  }
