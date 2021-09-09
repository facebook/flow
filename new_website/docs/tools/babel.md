---
title: Babel
slug: /tools/babel
---

Flow and [Babel](http://babeljs.io/) are designed to work great together. It
takes just a few steps to set them up together.

If you don't have Babel setup already, you can do that by following
[this guide](http://babeljs.io/docs/setup/).

Once you have Babel setup, install `@babel/preset-flow` with either
[Yarn](https://yarnpkg.com/) or [npm](https://www.npmjs.com/).

```sh
yarn add --dev @babel/preset-flow
# or
npm install --save-dev @babel/preset-flow
```

Then add `flow` to your Babel presets config.

```json
{
  "presets": ["@babel/preset-flow"]
}
```
