/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

type Props = {
  step: number,
};

type State = {
  count: number,
};

class Counter extends React.Component<Props, State> {
  state: State = {count: 0};

  increment: () => void = () => {
    this.setState(prev => ({count: prev.count + this.props.step}));
  };

  render(): React.Node {
    return (
      <div>
        <span>{this.state.count}</span>
        <button onClick={this.increment}>Add {this.props.step}</button>
      </div>
    );
  }
}

export default Counter;
