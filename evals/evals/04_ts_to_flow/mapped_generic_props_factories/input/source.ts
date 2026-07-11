import React, { ReactNode } from "react";

interface Column<T> {
  header: string;
  render: (row: T) => ReactNode;
}

function createTable<T>(columns: Column<T>[]) {
  return function Table({ rows }: { rows: T[] }) {
    return (
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
  };
}

interface User {
  id: number;
  name: string;
  email: string;
}

const UserTable = createTable<User>([
  { header: "Name", render: (u) => u.name },
  { header: "Email", render: (u) => u.email },
]);

const users: User[] = [
  { id: 1, name: "Alice", email: "alice@example.com" },
  { id: 2, name: "Bob", email: "bob@example.com" },
];

export default function App() {
  return <UserTable rows={users} />;
}
