---
id: transpiler
title: Transpiler
layout: docs
permalink: /docs/transpiler.html
prev: library.html
next: troubleshooting.html
---

Since types are not part of the JavaScript specification, we need to strip them out before sending the file to the user. There are two ways to do so:

* You can use the JSX transform tool (part of the React tools) to translate your files to plain Javascript
* For quick prototyping, you can run the transforms directly in the browser

## Using the offline transform tool

This is the recommended workflow for production. First, you need to install the React tools:

```
npm install -g react-tools
```

You can then simply run the transpiler in the background using the `jsx` command:

```
jsx --strip-types --harmony --watch src/ build/
```

This will run in the background, pick up any changes to files in `src/`, and create their pure Javascript version in `build/`.

## Using the in-browser transform

This is **not** recommended for production, because it is not as performant as the offline transform tool. However it is a good way to get started with quick prototyping.

All you have to do is include the JSX transformer in your document, and use a special MIME type for your Flow scripts:

```html
<head>
  <script src="build/JSXTransformer.js"></script>
</head>
<body>
  <script type="text/jsx;stripTypes=true;harmony=true">
  /* @flow */
  // ...
  </script>
</body>
```

Your script will then be transformed to plain Javascript when it is loaded by the browser.

## Transpiler in action

You can try out the live Flow transpiler below: just edit the Flow script in the top window, and the transformed Javascript will update in the second window.

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
