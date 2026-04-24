# babel-plugin-syntax-flow-parser-oxidized

Fork of upstream [`babel-plugin-syntax-hermes-parser`](https://github.com/facebook/hermes/tree/main/tools/hermes-parser/js/babel-plugin-syntax-hermes-parser) rewired to use `flow-parser-oxidized` (the Flow Rust parser compiled to WASM) instead of `hermes-parser`. This plugin switches Babel to use `flow-parser-oxidized` instead of the `@babel/parser`, providing full syntax support for Flow.

## Install

Using npm:

```sh
npm install --save-dev babel-plugin-syntax-flow-parser-oxidized
```

or using yarn:

```sh
yarn add babel-plugin-syntax-flow-parser-oxidized --dev
```

# Usage

The plugin can be enabled via:

```
// babel.config.json
{
  "plugins": ["babel-plugin-syntax-flow-parser-oxidized"]
}
```

If parser options need to be provide you can do so via the `parserOpts` config:

```
// babel.config.json
{
  "plugins": ["babel-plugin-syntax-flow-parser-oxidized"],
  "parserOpts": {
    "allowReturnOutsideFunction": true
  }
}
```
