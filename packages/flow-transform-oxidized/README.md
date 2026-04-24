# flow-transform-oxidized

A package for traversing and transforming a Flow AST. Forked from upstream
[`hermes-transform`](https://github.com/facebook/hermes/tree/main/tools/hermes-parser/js/hermes-transform)
with the sibling deps rewired:

- `hermes-parser` -> `flow-parser-oxidized`
- `hermes-estree` -> `flow-estree-oxidized`
- `hermes-eslint` -> `flow-eslint-oxidized`
- `prettier-plugin-hermes-parser` -> `prettier-plugin-flow-parser-oxidized`
  (optional peerDep)

This package is pieced together and inspired by code from:
- [`@babel/traverse`](https://github.com/babel/babel/tree/35ec4394a72e6fc88553ce7dcf0fb1c91d9505a9/packages/babel-traverse)
- [`eslint`](https://github.com/eslint/eslint/tree/e926b1735c77bf55abc1150b060a535a6c4e2778)
- [`prettier`](https://github.com/prettier/prettier/tree/7054df719e1789a71ce9e28b5a5452bc60fd42e7)

## Known gaps in this fork

- **Generated files preserved as-is**: `src/generated/{node-types.js,
  special-case-node-types/, TransformCloneSignatures.js.flow,
  TransformModifySignatures.js.flow, TransformReplaceSignatures.js.flow}` are
  marked `@generated` by upstream codegen scripts that live OUTSIDE this
  package (at `xplat/static_h/tools/hermes-parser/js/scripts/genTransform*.js`).
  Their contents still reference `hermes-estree` import paths. Retargeting the
  codegen so it emits `flow-estree-oxidized` references is a separate
  follow-up. Until then, consumers who hit those imports can rely on the
  Yarn-workspace resolution chain (the upstream `hermes-estree` package is
  still present alongside the fork).
- **Internal Symbol.for markers**: `src/detachedNode.js` uses
  `Symbol.for('hermes-transform - ...')` runtime markers. These are private
  to this package (not import paths), preserved verbatim from upstream to
  keep the fork a pure literal rename.
- **Partial vendor in flow-parser-oxidized**: an early-Phase-E vendor of the
  print/comments pipeline lives at
  `flow/packages/flow-parser-oxidized/src/transform/print/` (with `print.js`,
  `detachedNodeTypes.js`, `comments/`). It predates this fork and is left
  untouched. Consumers should migrate to `flow-transform-oxidized` over time;
  a follow-up can either delete the partial vendor or re-export from this
  package.
