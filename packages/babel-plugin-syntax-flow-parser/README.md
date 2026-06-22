# babel-plugin-syntax-flow-parser

Hermes parser plugin for [Babel](https://babeljs.io/). This plugin switches Babel to use `flow-parser` instead of the `@babel/parser`. The Flow parser uses Rust compiled to WASM for full Flow syntax support.

## Install

Using npm:

```sh
npm install --save-dev babel-plugin-syntax-flow-parser
```

or using yarn:

```sh
yarn add babel-plugin-syntax-flow-parser --dev
```

# Usage

The plugin can be enabled via:

```
// babel.config.json
{
  "plugins": ["babel-plugin-syntax-flow-parser"]
}
```

If parser options need to be provide you can do so via the `parserOpts` config:

```
// babel.config.json
{
  "plugins": ["babel-plugin-syntax-flow-parser"],
  "parserOpts": {
    "allowReturnOutsideFunction": true
  }
}
```
