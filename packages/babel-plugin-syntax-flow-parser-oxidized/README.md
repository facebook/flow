# babel-plugin-syntax-flow-parser-oxidized

Hermes parser plugin for [Babel](https://babeljs.io/). This plugin switches Babel to use `flow-parser-oxidized` instead of the `@babel/parser`. Since Hermes parser uses C++ compiled to WASM it is significantly faster and provides full syntax support for Flow.

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
