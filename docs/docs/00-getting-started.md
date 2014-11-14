---
id: getting-started
title: Getting Started with Flow
layout: docs
permalink: /docs/getting-started.html
prev: about-flow.html
---

Please follow the installation instructions **[TODO]**. This should put a binary called `flow` in your path. 

The simplest way to get started is to run `cd <root>; touch .flowconfig; flow check` on the command line. This performs a one-time check of all files under `<root>` and exits; since at this point none of those files are opted-in, ideally this should give you no errors. (If you do get errors, see the troubleshooting instructions **[TODO]**.)

Next, run `flow start`. This again performs a one-time check of all files under `<root>`, but this time, it starts up a server in the background that monitors all files under `<root>`.

Next, pick a file (typically, one of the entry points) and add the phrase `@flow` somewhere in its header in comments, e.g. like so:

```javascript
/* @flow */
...
```

Flow will perform type checking of this file in the background. To view the results, run `flow status`. This may show you a bunch of type errors.

(The full set of `flow` commands are explained elsewhere **[TODO]**.)

## Background

Learn about the type system **[TODO]**. You should have a basic idea of what kinds of type annotatations are available in the syntax, and what they mean (i.e., which sets of values they represent, and which kinds of operations are admitted on such sets of values by the typechecker).

The best strategy with Flow is to try cutting down the number of errors to 0 as quickly as possible, and then keeping the status that way from that point onwards. There are lots of ways of cutting down errors. Sometimes the errors are real, and looking at the error locations gives a good idea of what the error is and how to fix it. On the other hand, sometimes the errors are due to imprecision in the analysis, in which case you should feel free to use explicit dynamic typing (`any`) to silence those errors and move on. The reason why this is the best strategy is that when you get down to 0 errors, it is likely that Flow knows a lot about what's going on in your code (e.g., what the types of various expressions in the code are) and can, from that point onwards, guide you to understand, document, and maintain it. On the other hand, while there are errors, this information is in flux and possibly unreliable when thinking about invariants.

## Common errors and how to fix them

### Global not found

These errors are due to global references in your code, and possibly also due to typos. If the former, you can declare them if you know that they are going to be available when you run the code.

```javascript
/* @flow */
declare var foo: <type>
```

Alternatively, if you want to have a common set of global declarations so that they are available to multiple files at once, create a directory (say `globals_lib`), put a file in there (say `globals.js`), and do the declaration there. Then rerun Flow with option `--lib globals_lib` so that Flow knows where to find them.

### Required module not found

These errors are due to `require(...)` statements in your code that don't resolve to the set of modules exported by files under `<root>`. To specify additional code directories to Flow, add the following lines to `.flowconfig` under `<root>`.

```javascript
[include]
<path1>
<path2>
```

Alternatively, you may not have the code available for those modules, or otherwise want to specify declarations for them. In that case, put another file under `globals_lib` (say `modules.js`), and declare the module interface there.

```javascript
/* @flow */
declare module Bar {
  ...
}  
```

Note that if both an implementation and a declaration is found for a module, Flow will choose the implementation if it has been opted-in, the declaration otherwise.

## What to do if there are too many errors





