---
id: primitives
title: Primitives
layout: docs
permalink: /docs/primitives.html
prev: nullable-types.html
next: functions.html
---

Primitives are global definitions (e.g., `Array`) that are automatically in 
scope. Their type declarations can be found in the file [lib](https://github.com/facebook/flow/tree/master/lib).js in the Flow 
installation directory. (In fact, type errors may point into this file, e.g. 
for ill-typed calls of primitive methods, so you would need to look at this 
file in order to understand those errors.)
