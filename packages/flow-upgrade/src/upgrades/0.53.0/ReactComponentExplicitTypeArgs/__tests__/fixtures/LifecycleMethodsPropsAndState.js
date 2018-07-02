// @flow

import React from 'react';

class MyComponent1 extends React.Component {
  componentWillReceiveProps(nextProps: Props) {}
}

class MyComponent2 extends React.Component {
  shouldComponentUpdate(prevProps: Props, prevState: State) {}
}

class MyComponent3 extends React.Component {
  componentWillUpdate(prevProps: Props, prevState: State) {}
}

class MyComponent4 extends React.Component {
  componentDidUpdate(prevProps: Props, prevState: State) {}
}

const expression1 = () =>
  class extends React.Component {
    componentWillReceiveProps(nextProps: Props) {}
  }

const expression2 = () =>
  class extends React.Component {
    shouldComponentUpdate(prevProps: Props, prevState: State) {}
  }

const expression3 = () =>
  class extends React.Component {
    componentWillUpdate(prevProps: Props, prevState: State) {}
  }

const expression4 = () =>
  class extends React.Component {
    componentDidUpdate(prevProps: Props, prevState: State) {}
  }
