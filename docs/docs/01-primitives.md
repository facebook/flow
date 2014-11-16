---
id: primitives
title: Primitives
layout: docs
permalink: /docs/primitives.html
prev: nullable-types.html
next: functions.html
---

Primitives are global definitions (e.g., `Array`) that are automatically in 
scope. Their type declarations can be found in the file `[lib](https://github.com/facebook/flow/tree/master/lib).js` in the Flow 
installation directory. (In fact, type errors may point into this file, e.g. 
for ill-typed calls of primitive methods, so you would need to look at this 
file in order to understand those errors.)

The set of primitives currently understood well by Flow cover the common ones 
but are overall quite limited. In particular, not all primitives have precise 
type declarations: several primitives (e.g., `XMLHttpRequest`) are merely 
declared with type any so that Flow does not complain about not resolving the 
corresponding global names, but such primitives would otherwise not contribute 
to type errors yet. Their type declarations will eventually be filled out, at 
which point you can expect more type errors.
