---
id: new-project
title: Starting a new Flow project
layout: docs
permalink: /docs/new-project.html
prev: five-simple-examples.html
next: existing.html
---

One of the most powerful ways to use Flow is to have it checking your code from the first line you write. This allows you to ensure type safety from the very beginning and reduce the likelihood of errors, even while prototyping.

## Setting up your project

To set up a new project using Flow, all you need to do is initialize the root of the folder to tell Flow to start typechecking files within your project:

```bash
$> mkdir new_project
$> cd new_project
$> flow init
```

All this actually does is create a hidden file `.flowconfig` at the root of the folder. You might have noticed that this file was already present in each of the examples we looked at earlier.

Once the `.flowconfig` file is present, you can run ad-hoc checks on the code within it and its subfolders:

```bash
$> flow check
```

Although this works, it is not the most efficient way to use Flow: the check is running across the whole project's file structure every time.

## Using the Flow server

For a large project, you probably only want Flow to recheck the files that have changed each time. Flow uses a client/server architecture, and you are able to start a Flow server that will run in the background and type check files as they change. To do this simply run:

```bash
$> flow start   # This starts a server and returns
```

You can then check the status of the server:

```bash
$> flow         # This will list all current type errors
```

And once you've changed some files, you can quickly run it again:

```bash
$> flow status  # List all type errors with your latest changes
```

This reduces the overhead of starting up the check each time and allows for a much faster incremental workflow - not to mention a better integration with IDEs and other tools.

When you've finished working on your code, you can shutdown the server:

```bash
$> flow stop
```

## Typechecking your files

As with all the examples in the Flow tutorial, you need to add

{% highlight javascript linenos %}
/* @flow */
{% endhighlight %}

at the top of each file you'd like Flow to typecheck. You don't have to do this for all files at once, since Flow will just ignore files that do not have this comment. That way you can convert one file at a time and get the benefit of typing gradually without too much upfront effort.

It is possible to indicate on the command line that you would like Flow to check all JavaScript files, regardless of whether they have the `/* @flow */` declaration at the top. To do this, use the `--all` flag:

```bash
$> flow check --all
```

However, use this command cautiously, particularly if you are running Flow against a large existing project or one with many large third party frameworks or test suites. The chances are that the checker will find a *lot* of errors, and it can be quite demoralizing!

A far more valuable approach is to incrementally check - and fix - a file at a time, by manually adding the `/* @flow */` declaration as you go.

Read more about testing existing libraries or code in the [Running Flow on existing code](existing.html) section of this guide.

## Common Issues

As Flow starts to typecheck your files you may run into type errors. Check out the [Troubleshooting](troubleshooting.html) section for common errors and how to resolve them. The goal is to get the number of errors down to zero as fast as possible.

In some cases, errors may be due to inherent imprecision of the analysis - it won't always get it right, and can give errors that are false positives. In those cases you can either try to refactor your code to help Flow understand it, or use the `any` type to explicitly tell Flow about values that should not be checked.

TODO: a bit more here
