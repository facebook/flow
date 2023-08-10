---
title: Babel
slug: /tools/babel
---

Flow and [Babel](http://babeljs.io/) are designed to work great together. It
takes just a few steps to set them up together.

If you don't have Babel setup already, you can do that by following
[this guide](http://babeljs.io/docs/setup/).

Once you have Babel setup, install `@babel/preset-flow` and `babel-plugin-syntax-hermes-parser` with either
[Yarn](https://yarnpkg.com/) or [npm](https://www.npmjs.com/).

```sh
yarn add --dev @babel/preset-flow babel-plugin-syntax-hermes-parser
# or
npm install --save-dev @babel/preset-flow babel-plugin-syntax-hermes-parser
```

Then add the `@babel/preset-flow` preset and `babel-plugin-syntax-hermes-parser` plugin to your Babel config.

```json
{
  "presets": ["@babel/preset-flow"],
  "plugins": ["babel-plugin-syntax-hermes-parser"],
}
```

You can read the documentation of [babel-plugin-syntax-hermes-parser](https://github.com/facebook/hermes/blob/main/tools/hermes-parser/js/babel-plugin-syntax-hermes-parser/README.md) to see how to configure it. [This website's Babel config](https://github.com/facebook/flow/blob/baa74a889dc81fe36f0fd362db6d3e27d44d961d/website/babel.config.js#L10-L17) provides an example with custom parser options.
