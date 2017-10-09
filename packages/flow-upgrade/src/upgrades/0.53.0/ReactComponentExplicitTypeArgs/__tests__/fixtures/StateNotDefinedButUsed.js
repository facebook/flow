// @flow

import React from 'react';

class MyComponent1 extends React.Component {
  render() {
    return this.state.children;
  }
}

class MyComponent2 extends React.Component {
  render() {
    const state = {};
    return state;
  }
}

const expression1 = () =>
  class extends React.Component {
    render() {
      return this.state.children;
    }
  }

const expression2 = () =>
  class extends React.Component {
    render() {
      const state = {};
      return state;
    }
  }
