/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

type Column<T> = {
  header: string,
  render: (row: T) => React.Node,
};

function createTable<T>(
  columns: ReadonlyArray<Column<T>>,
): React.ComponentType<{rows: ReadonlyArray<T>}> {
  return ({rows}: {rows: ReadonlyArray<T>}) => (
    <table>
      <thead>
        <tr>
          {columns.map((col, i) => (
            <th key={i}>{col.header}</th>
          ))}
        </tr>
      </thead>
      <tbody>
        {rows.map((row, ri) => (
          <tr key={ri}>
            {columns.map((col, ci) => (
              <td key={ci}>{col.render(row)}</td>
            ))}
          </tr>
        ))}
      </tbody>
    </table>
  );
}

type User = {
  id: number,
  name: string,
  email: string,
};

const UserTable = createTable<User>([
  {header: 'Name', render: (u) => u.name},
  {header: 'Email', render: (u) => u.email},
]);

const users: Array<User> = [
  {id: 1, name: 'Alice', email: 'alice@example.com'},
  {id: 2, name: 'Bob', email: 'bob@example.com'},
];

component App() {
  return <UserTable rows={users} />;
}

export default App;
