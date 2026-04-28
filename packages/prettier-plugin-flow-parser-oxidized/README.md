# prettier-plugin-flow-parser-oxidized

Forked from `xplat/static_h/tools/hermes-parser/js/prettier-plugin-hermes-parser`.
A Prettier plugin that consumes the hermes/flow ESTree AST shape — intended to
pair with the Flow Rust parser stack (`flow-parser-oxidized`,
`flow-estree-oxidized`).

## Bundle status

The `index.bundle.mjs` shipped here is the prebuilt, minified plugin bundle
from the upstream fork (see `facebook/INTERNAL_DEV.md`). It is `@generated`
and is preserved as-is from upstream during this fork — **do not hand-edit
it**. The package entrypoint `index.mjs` stays in the same bundle-shaped form,
but adds a small local postlude that patches comment attachment for
`prettier-ignore` on `MemberExpression`, `MatchOrPattern`, and component
param/rest nodes.

A future regeneration milestone needs to retarget the upstream build pipeline
([`pieterv/prettier`'s `flow-fork` branch](https://github.com/pieterv/prettier/tree/flow-fork)
and `hermes-parser/js/scripts/build-prettier.sh`) to emit a bundle that calls
into the Flow Rust parser instead of the hermes WASM parser and bakes in that
comment-handling delta. Until that regeneration path exists, the bundle still
references `hermes` as the prettier parser name, which is why
`__tests__/*.js` keep `parser: 'hermes'` in their options.

## Usage

More details on using Prettier plugins: https://prettier.io/docs/en/plugins.html#using-plugins

```
// .prettierrc
{
  "plugins": ["prettier-plugin-flow-parser-oxidized"],
  "overrides": [
    {
      "files": ["*.js", "*.jsx", "*.flow"],
      "options": {
        "parser": "hermes"
      }
    }
  ]
}
```
More details on configuring Prettier parsers: https://prettier.io/docs/en/configuration.html#setting-the-parserdocsenoptionshtmlparser-option
