---
id: running
title: Running Flow code
permalink: /docs/running.html
prev: third-party.html
next: troubleshooting.html
---

Since types are not part of the JavaScript specification, we need to strip them out before sending the file to the user. There are a few ways to do so:

* Normally we recommend that you use [Babel](http://babeljs.io/) to strip away any type annotations from your files during development and deployment.
* For quick prototyping, you can also [run the transforms directly in the browser](#using-the-in-browser-transform)

For integration with various other tools and build systems, check out this [really thorough set of docs](http://babeljs.io/docs/setup/) detailing how to set up Babel to work with various popular tools like Browserify, Broccoli, Webpack, Node.js, etc.

## Using the offline transform tool

The offline transform tool is the recommended workflow for production. First, install the Babel CLI:

```bash
$> npm install -g babel-cli
```

Next, install the babel flow transform and add a `.babelrc` file to the root your project to tell Babel to strip Flow annotations:

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

For more detailed documentation on the `babel` CLI utility, check out it's own [docs](https://babeljs.io/docs/usage/cli/).

## Using the in-browser transform

This is **not** recommended for production because it is not as performant as the offline transform tool. However, it is a convenient way to get started with quick prototyping.

All you have to do is install the `babel-browser` npm package, include the browser transformer in your document, and just use a special MIME type for your Flow scripts:

```bash
$> mkdir -p node_modules && npm install babel-browser
```

```html
<head>
  <script src="node_modules/babel-core/browser.js"></script>
</head>
<body>
  <script type="text/babel">
  /* @flow */
  // ... Here you can put inline JS with Flow type syntax! ...
  </script>
  
  <!-- Additionally you can just include files indirectly -->
  <script type="text/babel" src="main.js"></script>
</body>
```

Your script will then be transformed to plain JavaScript when it is loaded by the browser.

## Transpiler in action

You can try out the live Flow transpiler below. Just edit the Flow script in the top window, and the transformed JavaScript will update in the second window.

<script>var ___tm = window.setTimeout; window.setTimeout = function(fn) { ___tm(fn, 0)}; // remove the stupid setTimeout in JSX live editor</script>
<!--[if lte IE 8]>
<script type="text/javascript" src="http://facebook.github.io/react/js/html5shiv.min.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/es5-shim.min.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/es5-sham.min.js"></script>
<![endif]-->
<script type="text/javascript" src="http://facebook.github.io/react/js/codemirror.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/javascript.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/react.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/react-dom.js"></script>
<!-- Right now JSXTransformer on the React website is too old. So I built
it from master on the React repo and copy/pasted it here. Whenever we ship
the next version of React we can just use it and remove the local one -->
<script type="text/javascript" src="/static/JSXTransformer.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/live_editor.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/showdown.js"></script>
<link rel="stylesheet" href="http://facebook.github.io/react/css/codemirror.css" />

<div id="jsxCompiler"></div>
<script src="/static/transformer.js"></script>
