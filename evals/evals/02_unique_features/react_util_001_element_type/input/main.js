/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

// TODO: Implement `FieldSpec`, `serverRenderableLabels`, `Sparkline`, and
// `Avatar`, used below.

export default component App() {
  const specs: Array<FieldSpec> = [
    {label: 'Name', render: 'input'},
    {label: 'Trend', render: Sparkline},
    {label: 'Bio', render: 'textarea'},
    {label: 'Photo', render: Avatar},
  ];
  return (
    <ul>
      {serverRenderableLabels(specs).map((label) => (
        <li key={label}>{label}</li>
      ))}
    </ul>
  );
}
