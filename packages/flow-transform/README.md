# flow-transform

A package for traversing and transforming a Flow AST. Forked from upstream
[`hermes-transform`](https://github.com/facebook/hermes/tree/main/tools/hermes-parser/js/hermes-transform)
with the sibling deps rewired:

- `hermes-parser` -> `flow-parser`
- `hermes-estree` -> `flow-estree`
- `hermes-eslint` -> `flow-eslint`

`flow-transform` itself prints with Prettier's built-in parsers and does not
depend on the prettier plugin fork.

The Flow package build reuses the upstream `genTransform*.js` generators in
their Flow target mode. That mode writes `flow-estree` imports, Flow readonly
syntax, and the Flow-specific `DeclareVariable` shape. The generators retain
their existing Hermes output by default.

This package is pieced together and inspired by code from:
- [`@babel/traverse`](https://github.com/babel/babel/tree/35ec4394a72e6fc88553ce7dcf0fb1c91d9505a9/packages/babel-traverse)
- [`eslint`](https://github.com/eslint/eslint/tree/e926b1735c77bf55abc1150b060a535a6c4e2778)
- [`prettier`](https://github.com/prettier/prettier/tree/7054df719e1789a71ce9e28b5a5452bc60fd42e7)

## Known gaps in this fork

- **Internal Symbol.for markers**: `src/detachedNode.js` uses
  `Symbol.for('hermes-transform - ...')` runtime markers. These are private
  to this package (not import paths), preserved verbatim from upstream to
  keep the fork a pure literal rename.
- **Partial vendor in flow-parser**: an early-Phase-E vendor of the
  print/comments pipeline lives at
  `flow/packages/flow-parser/oxidized-src/transform/print/` (with `print.js`,
  `detachedNodeTypes.js`, `comments/`). It predates this fork and is left
  untouched. Consumers should migrate to `flow-transform` over time;
  a follow-up can either delete the partial vendor or re-export from this
  package.
