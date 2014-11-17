---
id: underscore
title: Flow, meet Underscore
layout: docs
permalink: /docs/underscore.html
prev: react-example.html
next: type-annotations.html
---

## Introduction

[Underscore](http://underscorejs.org/) is a well-established JavaScript library that provides a number of useful functions for client- and server-side developers alike.

Running Flow on existing code can sometimes be a [daunting experience](existing.html) due to the volume of notices and warnings generated. It's also possible to use [type definitions](third-party.html) to make it easy to write typed new code *against* legacy libraries like Underscore.

But for the purposes of understanding how Flow works, it's an interesting exercise to see what it takes to be able to type check such a library itself cleanly.

## Assessment

Firstly, check out a clean Underscore repo:

```bash
$> git clone https://github.com/jashkenas/underscore.git
```

We need to initialize this repo folder as a Flow project, and start up the server

```bash
$> cd underscore
$> flow init
```
```bash
$> flow
```
```bbcode
Flow server launched for underscore
Spawned flow server (child pid=78756)
Logs will go to underscore.log
The flow server will be ready in a moment
No errors!
```

The main target file for this exercise is going to be `underscore.js` in the root directory. It's not advisable to try checking the whole repo, since it contains minified versions of the script, JSON files and unit test scripts that we're not interested in. We're also going to use weak mode - which assumes that all un-annotated variable are of type `any` - and which dramatically reduces the dauntingness of the initial warnings.

Opt the file in to be weakly checked with the declaration:

{% highlight javascript linenos %}
// @flow weak
// Underscore.js 1.7.0
...
{% endhighlight %}
