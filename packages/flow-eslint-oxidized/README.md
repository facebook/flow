# flow-eslint-oxidized

`flow-eslint-oxidized` is a custom parser for [ESLint](https://eslint.org/). It is the recommended parser for use for linting with Flow code.

This package is a fork of [`hermes-eslint`](https://github.com/facebook/hermes/tree/main/tools/hermes-parser/js/hermes-eslint) — see `xplat/static_h/tools/hermes-parser/js/hermes-eslint`. It rewires the upstream `hermes-parser` and `hermes-estree` dependencies onto the in-tree `flow-parser-oxidized` and `flow-estree-oxidized` workspaces; no behavioral changes were made. The upstream TypeScript-ESLint scope-manager license (`TYPESCRIPT_ESLINT_SCOPE_MANAGER_LICENSE`) is preserved verbatim.

## Usage

To use `flow-eslint-oxidized` as the parser for ESLint in your project you must specify `"flow-eslint-oxidized"` as the `"parser"` in your `.eslintrc` configuration file:

```json
{
  "parser": "flow-eslint-oxidized"
}
```

The ESLint documentation provides more information about [how to configure ESLint](https://eslint.org/docs/user-guide/configuring/), including [how to specify a custom parser](https://eslint.org/docs/user-guide/configuring/plugins#specifying-parser).

### Options

You may provide additional configuration for `flow-eslint-oxidized` by passing an object containing configuration options as the `"parserOptions"` in your ESLint configuration file. This object may contain the following properties:

```ts
type ParserOptions = {
  /**
   * The identifier that's used for JSX Element creation (after transpilation).
   * This should not be a member expression - just the root identifier (i.e. use "React" instead of "React.createElement").
   *
   * To use the new global JSX transform function, you can explicitly set this to `null`.
   *
   * Defaults to `"React"`.
   */
  jsxPragma?: string | null,

  /**
   * The identifier that's used for JSX fragment elements (after transpilation).
   * If `null`, assumes transpilation will always use a member on `jsxFactory` (i.e. React.Fragment).
   * This should not be a member expression - just the root identifier (i.e. use "h" instead of "h.Fragment").
   *
   * Defaults to `null`.
   */
  jsxFragmentName?: string | null,

  /**
   * The source type of the script.
   *
   * Defaults to `"module"`.
   */
  sourceType?: 'script' | 'module',

  /**
   * Ignore <fbt /> JSX elements when adding references to the module-level `React` variable.
   * FBT is JSX that's transformed to non-JSX and thus references differently
   *
   * https://facebook.github.io/fbt/
   */
  fbt?: boolean,
};
```

```json
{
  "parser": "flow-eslint-oxidized",
  "parserOptions": {
    "sourceType": "module"
  }
}
```
