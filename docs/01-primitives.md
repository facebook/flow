---
id: primitives
title: Primitives
layout: docs
permalink: /docs/primitives.html
prev: nullable-types.html
next: functions.html
---

Primitives are global definitions (e.g., `Array`) that are automatically in 
scope, because they are part of the JavaScript language or common libraries like the DOM. You can find those declarations in the  [lib](https://github.com/facebook/flow/tree/master/lib) directory in the Flow 
installation directory. In fact, type errors may point into this file, e.g. 
for ill-typed calls of primitive methods, so you may need to look at this 
file in order to understand some type errors.
