---
id: start-server
title: Start Server
layout: docs
permalink: /docs/start-server.html
prev: declarations.html
---

Other typechecking the Flow cli tool comes with a set of tools that can help with tools like
linters, autocomplete etc.

Starting a server with Flow is quite simple. If you run the `flow` command in a folder which has
a `.flowconfig` file in it, flow will try to type-check the entire folder. It will also start a flow server if one hasn't already been started yet.

However there is a more explicit way to start a flow server:

```
flow start
```

This command, will start a flow server in the current directory and will give you the path to a logs file.
