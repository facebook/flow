/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export default component MenuItem(
  'data-id' as dataId: string,
  'aria-label' as ariaLabel: string,
  onSelect: (id: string) => void,
  children: React.Node,
) {
  return (
    <li data-id={dataId} aria-label={ariaLabel} onClick={() => onSelect(dataId)}>
      {children}
    </li>
  );
}
