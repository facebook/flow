/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

// TODO: Implement the `Card` component and the `renderWithOverrides` function
// used below.

export default component App() {
  const base = {
    title: 'Welcome',
    body: <p>Getting started is easy.</p>,
  };
  return (
    <div>
      {renderWithOverrides(base, {})}
      {renderWithOverrides(base, {variant: 'highlight'})}
    </div>
  );
}
