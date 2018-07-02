// @flow

import React from 'react';

class MyComponent1 extends React.Component {
  componentWillReceiveProps(nextProps) {}
}

class MyComponent2 extends React.Component {
  shouldComponentUpdate(prevProps, prevState) {}
}

class MyComponent3 extends React.Component {
  componentWillUpdate(prevProps, prevState) {}
}

class MyComponent4 extends React.Component {
  componentDidUpdate(prevProps, prevState) {}
}

const expression1 = () =>
  class extends React.Component {
    componentWillReceiveProps(nextProps) {}
  }

const expression2 = () =>
  class extends React.Component {
    shouldComponentUpdate(prevProps, prevState) {}
  }

const expression3 = () =>
  class extends React.Component {
    componentWillUpdate(prevProps, prevState) {}
  }

const expression4 = () =>
  class extends React.Component {
    componentDidUpdate(prevProps, prevState) {}
  }
