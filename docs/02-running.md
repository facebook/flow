---
id: running
title: Running Flow code
layout: docs
permalink: /docs/running.html
prev: third-party.html
next: troubleshooting.html
---

Since types are not part of the JavaScript specification, we need to strip them out before sending the file to the user. There are a few ways to do so:

* You can use the [JSX transform tool](https://www.npmjs.com/package/jstransform), or a [third party transform tool](https://github.com/facebook/flow/wiki/3rd-party-tools#transformers), to strip away any type annotations from your files.
* For quick prototyping, you can [run the transforms directly in the browser](#using-the-in-browser-transform)

## Using the offline transform tool

This is the recommended workflow for production. First, you need to install the React tools:

```bash
$> npm install -g jstransform
```

You can then simply run the transpiler in the background using the `jsx` command:

```bash
$> jstransform --strip-types --harmony --watch src/ build/
```

This will run in the background, pick up any changes to files in `src/`, and create their pure JavaScript version in `build/`.

## Using the in-browser transform

This is **not** recommended for production, because it is not as performant as the offline transform tool. However it is a good way to get started with quick prototyping.

All you have to do is include the JSX transformer in your document (version 0.13.2 or later), and use a special MIME type for your Flow scripts:

```html
<head>
  <script src="https://fb.me/JSXTransformer-0.13.2.js"></script>
</head>
<body>
  <script type="text/jsx;stripTypes=true;harmony=true">
  /* @flow */
  // ...
  </script>
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
<!-- Right now JSXTransformer on the React website is too old. So I built
it from master on the React repo and copy/pasted it here. Whenever we ship
the next version of React we can just use it and remove the local one -->
<script type="text/javascript" src="/static/JSXTransformer.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/live_editor.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/showdown.js"></script>
<link rel="stylesheet" href="http://facebook.github.io/react/css/codemirror.css" />

<div id="jsxCompiler"></div>
<script src="/static/transformer.js"></script>
