The file `source.ts` contains a TypeScript factory `createTable` that builds a
strongly-typed table component for a given row type and a set of column
descriptors.

Convert it to idiomatic Flow in `main.js`, preserving the runtime behavior and
keeping the row type flowing through to each column's `render` callback. The
result must pass `flow check` with zero errors.
