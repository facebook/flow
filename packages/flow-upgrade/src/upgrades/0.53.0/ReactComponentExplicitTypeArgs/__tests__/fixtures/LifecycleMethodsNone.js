// @flow

import React from 'react';

class MyComponent1 extends React.Component {
  componentWillReceiveProps() {}
}

class MyComponent2 extends React.Component {
  shouldComponentUpdate() {}
}

class MyComponent3 extends React.Component {
  componentWillUpdate() {}
}

class MyComponent4 extends React.Component {
  componentDidUpdate() {}
}

const expression1 = () =>
  class extends React.Component {
    componentWillReceiveProps() {}
  }

const expression2 = () =>
  class extends React.Component {
    shouldComponentUpdate() {}
  }

const expression3 = () =>
  class extends React.Component {
    componentWillUpdate() {}
  }

const expression4 = () =>
  class extends React.Component {
    componentDidUpdate() {}
  }
