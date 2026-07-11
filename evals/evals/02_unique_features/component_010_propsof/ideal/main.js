/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export component Badge(
  text: string,
  color: string = "gray",
  size: 'sm' | 'md' | 'lg' = 'md',
) {
  const fontSize = size === 'sm' ? '12px' : size === 'lg' ? '24px' : '16px';
  return <span style={{color, fontSize}}>{text}</span>;
}

function createBadgePresets(
  configs: ReadonlyArray<React.PropsOf<Badge>>,
): ReadonlyArray<React.Node> {
  return configs.map((config, i) => <Badge key={i} {...config} />);
}

export component App() {
  const badges = createBadgePresets([
    {text: 'New', color: 'green', size: 'sm'},
    {text: 'Warning', color: 'orange', size: 'md'},
    {text: 'Critical', color: 'red', size: 'lg'},
  ]);
  return <div>{badges}</div>;
}
