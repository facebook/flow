/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export default component StatCard(
  title: string,
  count: number,
  subtitle?: string,
) {
  return (
    <div>
      <h3>{title}</h3>
      {subtitle != null ? <p>{subtitle}</p> : null}
      <span>{count}</span>
    </div>
  );
}
