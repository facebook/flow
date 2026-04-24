# flow-typed-oxidized

Forked verbatim from `xplat/static_h/tools/hermes-parser/js/flow-typed`. Pure
Flow `declare module` shims for third-party packages (`@babel/parser`,
`@babel/code-frame`, `espree`, `glob`, `jest`, `jest-specific-snapshot`,
`mkdirp`, `node`, `prettier`, `@typescript-eslint/parser`,
`@typescript-eslint/visitor-keys`).

There is no JS code in this package — only type declarations for upstream
third-party modules. Nothing was renamed; the module names refer to the
external packages, not to hermes-parser/hermes-estree.

Consumed via `[libs] ./flow-typed-oxidized` in a sibling package's
`.flowconfig`, mirroring the upstream consumption pattern.
