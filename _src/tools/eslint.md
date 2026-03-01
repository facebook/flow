---
title: ESLint
slug: /tools/eslint
---

ESLint is a static analysis tool which can help you quickly find and
fix bugs and stylistic errors in your code. The rules ESLint provide
complement the checks provided by Flow's type system.

You can quick-start setup ESLint, install `hermes-eslint` with either
[Yarn](https://yarnpkg.com/) or [npm](https://www.npmjs.com/).

```sh
yarn add --dev eslint hermes-eslint eslint-plugin-ft-flow
# or
npm install --save-dev eslint hermes-eslint eslint-plugin-ft-flow
```

Then create a `eslint.config.(js|mjs|cjs)` or `.eslintrc.js` file in your project root with the following:

```js
module.exports = {
  root: true,
  parser: 'hermes-eslint',
  plugins: [
    'ft-flow'
  ],
  extends: [
    'eslint:recommended',
    'plugin:ft-flow/recommended',
  ],
};
```

In the above config the order of the `extends` plugins is important as `plugin:ft-flow/recommended`
disables some rules from `eslint:recommended` so it needs to be defined after to work correctly.

For more information about configuring ESLint, [check out the ESLint docs](https://eslint.org/).

You can then lint your codebase with:

```sh
yarn run eslint . --ext .js,.jsx
# or
npm run eslint . --ext .js,.jsx
```

### Usage With Prettier {#toc-usage-with-prettier}

If you use [`prettier`](https://www.npmjs.com/package/prettier), there is also
a helpful config to help ensure ESLint doesn't report on formatting issues that
prettier will fix: [`eslint-config-prettier`](https://www.npmjs.com/package/eslint-config-prettier).

Using this config by adding it to the **_end_** of your `extends`:

```diff
  module.exports = {
    root: true,
    parser: 'hermes-eslint',
    plugins: [
      'ft-flow'
    ],
    extends: [
      'eslint:recommended',
      'plugin:ft-flow/recommended',
+     'prettier',
    ],
  };
```


### Additional Plugins {#toc-additional-plugins}

ESLint plugins provide additional rules and other functionality on top of ESLint.
Below are just a few examples that might be useful:

- Helpful language rules by the Flow team: [`eslint-plugin-fb-flow`](https://www.npmjs.com/package/eslint-plugin-fb-flow)
- React best practices: [`eslint-plugin-react`](https://www.npmjs.com/package/eslint-plugin-react)
  and [`eslint-plugin-react-hooks`](https://www.npmjs.com/package/eslint-plugin-react-hooks)
- Jest testing: [`eslint-plugin-jest`](https://www.npmjs.com/package/eslint-plugin-jest)
- Import/export conventions : [`eslint-plugin-import`](https://www.npmjs.com/package/eslint-plugin-import)
- NodeJS best practices: [`eslint-plugin-node`](https://www.npmjs.com/package/eslint-plugin-node)
- ESLint comment restrictions: [`eslint-plugin-eslint-comments`](https://www.npmjs.com/package/eslint-plugin-eslint-comments)

Every plugin that is out there includes documentation on the various configurations and rules they offer.
A typical plugin might be used like:

```diff
  module.exports = {
    root: true,
    parser: 'hermes-eslint',
    plugins: [
      'ft-flow'
+     'jest',
    ],
    extends: [
      'eslint:recommended',
      'plugin:ft-flow/recommended',
+     'plugin:jest/recommended',
    ],
  };
```

Search ["eslint-plugin" on npm](https://www.npmjs.com/search?q=eslint-plugin) for more.
