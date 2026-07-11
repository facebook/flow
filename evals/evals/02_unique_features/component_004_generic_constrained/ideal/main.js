/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export default component DataTable<T extends {id: string, label: string, ...}>(
  items: Array<T>,
  renderRow: (item: T) => React.Node,
  sortBy: 'id' | 'label' = 'id',
) {
  const sorted = [...items].sort((a, b) => {
    const aVal = sortBy === 'id' ? a.id : a.label;
    const bVal = sortBy === 'id' ? b.id : b.label;
    return aVal.localeCompare(bVal);
  });
  return (
    <table>
      <thead>
        <tr>
          <th>ID</th>
          <th>Content</th>
        </tr>
      </thead>
      <tbody>
        {sorted.map((item) => (
          <tr key={item.id}>
            <td>{item.id}</td>
            <td>{renderRow(item)}</td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}
