---
id: getting-started
title: Getting Started with Flow
layout: docs
permalink: /docs/getting-started.html
prev: about-flow.html
---

Please follow the [installation instructions](00-installing-flow.md). This should put a binary called `flow` in your path. 

The simplest way to get started is to run `cd <root>; touch .flowconfig; flow check` on the command line. This performs a one-time check of all files under `<root>` and exits; since at this point none of those files are opted-in, ideally this should give you no errors. (If you do get errors, see the [troubleshooting instructions](02-troubleshooting.md).)

Next, run `flow start`. This again performs a one-time check of all files under `<root>`, but this time, it starts up a server in the background that monitors all files under `<root>`.

Next, pick a file (typically, one of the entry points) and add the phrase `@flow` somewhere in its header in comments, e.g. like so:

```javascript
/* @flow */
...
```

Flow will perform type checking of this file in the background. To view the results, run `flow status`. This may show you a bunch of type errors.

The full set of `flow` commands and how to use them effectively is explained in the [user guide](02-flow-basics.md).

## Tutorial

