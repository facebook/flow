// @flow

import React from 'react';

class MyComponent1 extends React.Component {
  componentWillReceiveProps(nextProps: Props) {}
}

class MyComponent2 extends React.Component {
  shouldComponentUpdate(prevProps: Props) {}
}

class MyComponent3 extends React.Component {
  componentWillUpdate(prevProps: Props) {}
}

class MyComponent4 extends React.Component {
  componentDidUpdate(prevProps: Props) {}
}

const expression1 = () =>
  class extends React.Component {
    componentWillReceiveProps(nextProps: Props) {}
  }

const expression2 = () =>
  class extends React.Component {
    shouldComponentUpdate(prevProps: Props) {}
  }

const expression3 = () =>
  class extends React.Component {
    componentWillUpdate(prevProps: Props) {}
  }

const expression4 = () =>
  class extends React.Component {
    componentDidUpdate(prevProps: Props) {}
  }
