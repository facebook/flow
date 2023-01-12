---
title: Installation
slug: /install
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

## Setup Compiler

First you'll need to setup a compiler to strip away Flow types. You can
choose between [Babel](http://babeljs.io/) and
[flow-remove-types](https://github.com/facebook/flow/tree/master/packages/flow-remove-types).

<Tabs
  defaultValue="babel"
  values={[
    {label: 'Babel', value: 'babel'},
    {label: 'flow-remove-types', value: 'flow-remove-types'},
  ]}>
<TabItem value="babel">

[Babel](http://babeljs.io/) is a compiler for JavaScript code that has
support for Flow. Babel will take your Flow code and strip out any type
annotations.

First install `@babel/core`, `@babel/cli`, and `@babel/preset-flow` with
either [Yarn](https://yarnpkg.com/) or [npm](https://www.npmjs.com/).

```bash npm2yarn
npm install --save-dev @babel/core @babel/cli @babel/preset-flow
```

Next you need to create a `.babelrc` file at the root of your project with
`"@babel/preset-flow"` in your `"presets"`.

```json
{ "presets": ["@babel/preset-flow"] }
```

If you then put all your source files in a `src` directory you can compile them
to another directory by running:

```bash
./node_modules/.bin/babel src/ -d lib/
```

You can add this to your `package.json` scripts easily.

```json
{
  "name": "my-project",
  "main": "lib/index.js",
  "scripts": {
    "build": "babel src/ -d lib/",
    "prepublish": "{{include.package_manager}} run build"
  }
}
```

> **Note:** You'll probably want to add a `prepublish` script that runs this
> transform as well, so that it runs before you publish your code to the npm
> registry.

</TabItem>
<TabItem value="flow-remove-types">

[flow-remove-types](https://github.com/facebook/flow/tree/master/packages/flow-remove-types) is a small
CLI tool for stripping Flow type annotations from files. It's a lighter-weight
alternative to Babel for projects that don't need everything Babel provides.

First install `flow-remove-types` with either
[Yarn](https://yarnpkg.com/) or [npm](https://www.npmjs.com/).

```bash npm2yarn
npm install --save-dev flow-remove-types
```

If you then put all your source files in a `src` directory you can compile them
to another directory by running:

```sh
./node_modules/.bin/flow-remove-types src/ -d lib/
```

You can add this to your `package.json` scripts easily.

```json
{
  "name": "my-project",
  "main": "lib/index.js",
  "scripts": {
    "build": "flow-remove-types src/ -d lib/",
    "prepublish": "{{include.package_manager}} run build"
  }
}
```

> **Note:** You'll probably want to add a `prepublish` script that runs this
> transform as well, so that it runs before you publish your code to the npm
> registry.

</TabItem>
</Tabs>

## Setup Flow

Flow works best when installed per-project with explicit versioning rather than
globally.

Luckily, if you're already familiar with `npm` or `yarn`, this process should
be pretty familiar!

### Add a `devDependency` on the `flow-bin` npm package

```bash npm2yarn
npm install --save-dev flow-bin
```

### Add a `"flow"` script to your `package.json`

```json
{
  "name": "my-flow-project",
  "version": "1.0.0",
  "devDependencies": {
    "flow-bin": "^0.158.0"
  },
  "scripts": {
    "flow": "flow"
  }
}
```

### Run Flow

The first time, run:

```bash npm2yarn
npm run flow init
```

```
> my-flow-project@1.0.0 flow /Users/Projects/my-flow-project
> flow "init"
```

After running `flow` with `init` the first time, run:

```bash npm2yarn
npm run flow
```

```
> my-flow-project@1.0.0 flow /Users/Projects/my-flow-project
> flow

No errors!
```
