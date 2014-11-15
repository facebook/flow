---
id: flow-basics
title: Flow Basics
layout: docs
permalink: /docs/flow-basics.html
next: getting-started.html
---

## Background

Learn about the [type system](01-type-annotations.md). You should have a basic idea of what kinds of type annotatations are available in the syntax, and what they mean (i.e., which sets of values they represent, and which kinds of operations are admitted on such sets of values by the typechecker).

The best strategy with Flow is to try cutting down the number of errors to 0 as quickly as possible, and then keeping the status that way from that point onwards. There are lots of ways of cutting down errors. Sometimes the errors are real, and looking at the error locations gives a good idea of what the error is and how to fix it. On the other hand, sometimes the errors are due to imprecision in the analysis, in which case you should feel free to use explicit dynamic typing (`any`) to silence those errors and move on. The reason why this is the best strategy is that when you get down to 0 errors, it is likely that Flow knows a lot about what's going on in your code (e.g., what the types of various expressions in the code are) and can, from that point onwards, guide you to understand, document, and maintain it. On the other hand, while there are errors, this information is in flux and possibly unreliable when thinking about invariants.
