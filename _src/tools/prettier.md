---
title: Prettier
slug: /tools/prettier
---

All Flow features are supported by [Prettier](https://prettier.io/).
See [these instructions](https://prettier.io/docs/install) on how to install Prettier to your project.

To enable full support for the latest Flow features (e.g. pattern matching and type guards) make
sure to also install the [@prettier/plugin-hermes](https://www.npmjs.com/package/@prettier/plugin-hermes) plugin,
for example with:
```
yarn add --dev @prettier/plugin-hermes
```
and include
```
plugins:
  - "@prettier/plugin-hermes"
```
to your `.prettierrc` file.
