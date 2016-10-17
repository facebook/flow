---
id: running
title: Running Flow code
permalink: /docs/running.html
prev: third-party.html
next: troubleshooting.html
---

Since types are not part of the JavaScript specification, we need to strip them out before sending the file to the user. We recommend using [Babel](http://babeljs.io/) to do this.

For integration with various other tools and build systems, check out this [really thorough set of docs](http://babeljs.io/docs/setup/) detailing how to set up Babel to work with various popular tools like Browserify, Broccoli, Webpack, Node.js, etc.

## Babel Quick Start

First, install the Babel CLI:

```bash
$> npm install -g babel-cli
```

Next, install the Flow transform and add a `.babelrc` file to the root of your project to tell Babel to strip Flow annotations:

```bash
$> cd /path/to/my/project
$> mkdir -p node_modules && npm install babel-plugin-transform-flow-strip-types
$> echo '{"plugins": ["transform-flow-strip-types"]}' > .babelrc
```
You can now simply run the transpiler in the background using the `babel` command:

```bash
$> babel --watch=./src --out-dir=./build
```

This will run in the background, pick up any changes to files in `src/`, and create their pure JavaScript version in `build/`.

For more detailed documentation on the `babel` CLI utility, check out its [docs](https://babeljs.io/docs/usage/cli/).

## flow-remove-types Quick Start

While Babel is the recommended way of running Flow code, [`flow-remove-types`](https://github.com/leebyron/flow-remove-types) is a limited but simpler alternative.

First, install `flow-remove-types`:

```bash
$> npm install -g flow-remove-types
```

Then run your code through to remove the types, creating a pure JavaScript version in `build/`.

```bash
$> flow-remove-types src/ --out-dir build/
```

Or run the code directly by substituting `flow-node` for `node`.

```bash
$> flow-node main.js
```

For more detailed documentation on `flow-remove-types`, check out its [docs](https://github.com/leebyron/flow-remove-types).
