---
id: new-project
title: Running Flow on a new project
layout: docs
permalink: /docs/new-project.html
prev: getting-started.html
next: library.html
---

## Setting up your project

To set up a new project using Flow, all you need to do is create a magic file `.flowconfig` to tell Flow to start typechecking files in your project:

```
$> touch .flowconfig
$> flow check
```

This works but is not the most efficient way to use Flow. As you're changing files in your project you only want Flow to recheck the files that have changed. To achieve this, Flow is built as a client/server architecture: you can start a Flow server that will run in the background and type check files as they change. To do this simply run:

```
$> flow start   # This starts a server and returns

$> flow status  # This will list all current type errors

... <change some files> ...

$> flow status  # List all type errors with your latest changes
```

## Typechecking your files

As with all the examples in the Flow tutorial, you need to add

{% highlight javascript linenos %}
/* @flow */
{% endhighlight %}

at the top of each file you'd like Flow to typecheck. You don't have to do this for all files at once, since Flow will just ignore files that do not have this comment. That way you can convert one file at a time and get the benefit of typing gradually without too much upfront effort.

## Common Issues

As Flow starts to typecheck your files you may run into type errors. Check out the [Troubleshooting](troubleshooting.html) section for common errors and how to resolve them. The goal is to get the number of errors down to zero as fast as possible.

In some cases, errors may be due to inherent imprecision of the analysis - it won't always get it right, and can give errors that are false positives. In those cases you can either try to refactor your code to help Flow understand it, or use the `any` type to explicitly tell Flow about values that should not be checked.
