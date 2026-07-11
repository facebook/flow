/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

type Props<T> = {
  items: ReadonlyArray<T>,
  renderItem: (item: T) => React.Node,
};

function List<T>(props: Props<T>): React.Node {
  return (
    <ul>
      {props.items.map((item, i) => (
        <li key={i}>{props.renderItem(item)}</li>
      ))}
    </ul>
  );
}

export default List;
