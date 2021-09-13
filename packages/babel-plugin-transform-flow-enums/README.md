babel-plugin-transform-flow-enums
=================================

This Babel transform turns [Flow Enums](https://flow.org/en/docs/enums/) `EnumDeclaration` nodes into calls to the [Flow Enums runtime](https://www.npmjs.com/package/flow-enums-runtime).

This plugin requires the [Flow syntax plugin](https://babeljs.io/docs/en/babel-plugin-syntax-flow), with `{enums: true}` enabled.

Read more about how to [enable Flow Enums in your project](https://flow.org/en/docs/enums/enabling-enums/).

## Options
- `getRuntime`:
  - Optional function. Called with [Babel types](https://babeljs.io/docs/en/babel-types) as the first argument.
  - If supplied, will be called to produce a Babel AST node which will be a reference to the Flow Enums runtime.
  - If omitted, `require('flow-enums-runtime')` will be used.
