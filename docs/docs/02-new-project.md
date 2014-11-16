---
id: new-project
title: Creating a project using Flow
layout: docs
permalink: /docs/new-project.html
prev: troubleshooting.html
next: library.html
---

## Setting up your project

To set up a new project using Flow, all you need to do is create a magic file `.flowconfig` to tell Flow to start typechecking files in your project:

```
touch .flowconfig
flow check
```

This works but is not the most efficient way to use Flow. As you're changing files in your project you only want Flow to recheck the files that have changed. To achieve this, Flow is built as a client/server architecture: you can start a Flow server that will run in the background and type check files as they change. To do this simply run:

```
flow start   # This starts a server and returns

flow status  # This will list all current type errors

... <change some files> ...
flow status  # List all type errors with your latest changes
```

## Typechecking your files

As with all the examples in the Flow tutorial, you need to add 

```
/* @flow */
```

at the top of each file you'd like Flow to typecheck. You don't have to do this for all files at once, since Flow will just ignore files that do not have this comment. That way you can convert one file at a time and get the benefit of typing gradually without too much upfront effort. 
