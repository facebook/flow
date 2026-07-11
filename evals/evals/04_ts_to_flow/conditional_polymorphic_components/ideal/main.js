/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

type AlertSpec =
  | {level: 'info', message: string}
  | {level: 'warning', message: string, dismissible: boolean}
  | {level: 'error', message: string, code: number};

component Alert(spec: AlertSpec) {
  return match (spec) {
    {level: 'info', const message} => <div className="info">{message}</div>,
    {level: 'warning', const message, const dismissible} => (
      <div className="warning">
        {message}
        {dismissible && <button type="button">Dismiss</button>}
      </div>
    ),
    {level: 'error', const message, const code} => (
      <div className="error">
        [{code}] {message}
      </div>
    ),
  };
}

component App() {
  return (
    <div>
      <Alert spec={{level: 'info', message: 'All good'}} />
      <Alert spec={{level: 'warning', message: 'Careful', dismissible: true}} />
      <Alert spec={{level: 'error', message: 'Boom', code: 500}} />
    </div>
  );
}

export default App;
