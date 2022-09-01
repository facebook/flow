// @flow

import * as React from 'react';

{
  const {ConcurrentMode} = React;

  <ConcurrentMode>
    <div />
  </ConcurrentMode>
}

{
  const {Component, ConcurrentMode} = React;

  class ClassExample extends Component<{||}> {
    render(): React.Node {
      return null;
    }
  }

  <ConcurrentMode>
    <ClassExample />
  </ConcurrentMode>
}

{
  const {ConcurrentMode} = React;

  function FunctionExample() {
    return null;
  }

  <ConcurrentMode>
    <FunctionExample />
  </ConcurrentMode>
}
