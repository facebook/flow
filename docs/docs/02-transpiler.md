---
id: transpiler
title: Transpiler
layout: docs
permalink: /docs/transpiler.html
prev: library.html
next: type-annotations.html
---

Since types are not part of the JavaScript specification, we need to strip them out before sending the file to the user. You can use the JSX tool in order to do so.

In order to improve the debugging experience, all the type annotations are replaced by spaces, this way you line and column numbers are preserved.

<!--[if lte IE 8]>
<script type="text/javascript" src="http://facebook.github.io/react/js/html5shiv.min.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/es5-shim.min.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/es5-sham.min.js"></script>
<![endif]-->
<script type="text/javascript" src="http://facebook.github.io/react/js/codemirror.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/javascript.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/react.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/JSXTransformer.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/live_editor.js"></script>
<script type="text/javascript" src="http://facebook.github.io/react/js/showdown.js"></script>
<link rel="stylesheet" href="http://facebook.github.io/react/css/syntax.css" />
<link rel="stylesheet" href="http://facebook.github.io/react/css/codemirror.css" />
<link rel="stylesheet" href="http://facebook.github.io/react/css/react.css" />

<div id="jsxCompiler"></div>
<script src="/flow/static/transformer.js"></script>
