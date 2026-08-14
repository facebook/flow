---
title: Babel
slug: /tools/babel
description: "How to set up Babel to strip Flow type annotations from your code."
---

Flow and [Babel](http://babeljs.io/) are designed to work great together. It
takes just a few steps to set them up together.

If you don't have Babel setup already, you can do that by following
[this guide](http://babeljs.io/docs/setup/).

Once you have Babel setup, install `@babel/preset-flow` and `flow-parser` with either
[Yarn](https://yarnpkg.com/) or [npm](https://www.npmjs.com/).

```sh
yarn add --dev @babel/preset-flow flow-parser
# or
npm install --save-dev @babel/preset-flow flow-parser
```

Then add the `@babel/preset-flow` preset and `flow-parser/babel-plugin` plugin to your Babel config.

```json
{
  "presets": ["@babel/preset-flow"],
  "plugins": ["flow-parser/babel-plugin"]
}
```

You can read the [flow-parser Babel plugin documentation](https://github.com/facebook/flow/tree/main/packages/flow-parser#babel-plugin) to see how to configure it, including custom parser options.

## See Also {#toc-see-also}

- [Getting Started](../getting-started.md) — full setup guide including Babel configuration
