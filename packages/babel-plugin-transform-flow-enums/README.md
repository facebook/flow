babel-plugin-transform-flow-enums
=================================

This Babel transform turns `EnumDeclaration` nodes into calls to the Flow Enums runtime.

This plugin requires the Flow syntax plugin, with `{enums: true}` enabled.

## Options
- `getRuntime`:
  - Optional function. Called with Babel types as the first argument.
  - If supplied, will be called to produce a Babel AST node which will be a reference to the Flow Enums runtime.
  - If omitted, `require('flow-enums-runtime')` will be used.
