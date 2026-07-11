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
  'data-id': string,
  'aria-label': string,
  onSelect: (id: string) => void,
  children: React.Node,
};

function MenuItem({
  'data-id': dataId,
  'aria-label': ariaLabel,
  onSelect,
  children,
}: Props): React.Node {
  return (
    <li data-id={dataId} aria-label={ariaLabel} onClick={() => onSelect(dataId)}>
      {children}
    </li>
  );
}

export default MenuItem;
