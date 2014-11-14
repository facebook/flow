---
id: getting-started
title: Getting Started with Flow
layout: docs
permalink: /docs/getting-started.html
prev: about-flow.html
---

Please follow the installation instructions **[TODO]**. This should put a binary called `flow` in your path. 

Run `cd <root>; flow check` on the command line. This performs a one-time check of all files under `<root>` and exits; since at this point none of those files are opted-in, ideally this should give you no errors. (If you do get errors, see the troubleshooting instructions **[TODO]**.)

Next, run `flow start`. This again performs a one-time check of all files under `<root>`, but this time, it starts up a server in the background that monitors all files under `<root>`.

Next, pick a file (typically, one of the entry points) and add the phrase `@flow` somewhere in its header in comments, e.g. like so:

```javascript
/* @flow */
...
```

Flow will perform type checking of this file in the background. To view the results, run `flow status`. This may show you a bunch of type errors.

## Common errors and how to fix them

The best strategy with Flow is to try cutting down the number of errors to 0 as quickly as possible, and then keeping the status that way from that point onwards. There are lots of ways of cutting down errors. Sometimes the errors are real, and looking at the error locations gives a good idea of what the error is and how to fix it. On the other hand, sometimes the errors are due to imprecision in the analysis, in which case you should feel free to use explicit dynamic typing to silence those errors (explained below) and move on. The reason why this is the best strategy is that when you get down to 0 errors, it is likely that Flow knows a lot about what's going on in your code (e.g., what the types of various expressions in the code are) and can, from that point onwards, guide you to understand, document, and maintain it. On the other hand, while there are errors, this information is in flux and possibly unreliable when thinking about invariants.

## What to do if there are too many errors





