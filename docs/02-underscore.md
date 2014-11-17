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

The main target file for this exercise is going to be `underscore.js` in the root directory. It's not advisable to try checking the whole repo, since it contains minified versions of the script, JSON files and unit test scripts that we're not interested in. We're also going to use weak mode - which assumes that all un-annotated variable are of type `any` - and which dramatically reduces the number of daunting warnings.

Opt the file in to be weakly checked with the declaration:

{% highlight javascript linenos %}
// @flow weak
// Underscore.js 1.7.0
...
{% endhighlight %}

Now when we run Flow again, we get a small number of errors:

```bbcode
underscore.js:16:28,33: property _
Property cannot be accessed on global object

underscore.js:51:5,14: assignment of property _
Property cannot be assigned on global object
...
Found 26 errors
```

Quite manageable. Let's look at each of these in turn and modify the file as required. (Note that the line numbers in this article may not match your own results as Underscore itself may have been updated since it was written)

## Global objects

Our first two warnings are early on in the file, because Underscore tries to associate itself with the global object passed in to the self-executing anonymous function.

```bbcode
underscore.js:16:28,33: property _
Property cannot be accessed on global object

underscore.js:51:5,14: assignment of property _
Property cannot be assigned on global object
```

We can easily mitigate Flow's warnings about the `_` property on this object by declaring that the `root` variable is of type `any`:

{% highlight javascript linenos=table %}
// ...`window` in the browser, or `exports` on the server.
var root: any = this;
{% endhighlight %}

The final error message, where Underscore is trying to see if RequireJS is present fails similarly:

```bbcode
Unknown global name: define
```

We can add a declaration at the top of the file to show that this too is of type `any`:

{% highlight javascript linenos=table %}
declare var define: any;

(function() {
  ...
{% endhighlight %}

Three errors down already.

## Instantiation

Another common type of error reported is this:

```bbcode
underscore.js:1255:17,37: function call
Property not found in
  [LIB] core.js:118:14,120:5: object type
```

This is brought about by the use of class names without `new` instantiation. We simply update lines like:

{% highlight javascript linenos=table %}
  results = Array(length),
{% endhighlight %}

to

{% highlight javascript linenos=table %}
  results = new Array(length),
{% endhighlight %}

This brings our error count down considerably!

## TODO:

```bbcode
underscore.js:124:19,37: property length
Property not found in
  [LIB] core.js:47:1,65: Boolean

underscore.js:128:27,37: access of computed property/element
Computed property/element cannot be accessed on
  underscore.js:123:16,41: boolean

underscore.js:142:19,37: property length
Property not found in
  [LIB] core.js:47:1,65: Boolean

underscore.js:146:25,37: access of computed property/element
Computed property/element cannot be accessed on
  underscore.js:141:16,41: boolean

underscore.js:149:27,37: access of computed property/element
Computed property/element cannot be accessed on
  underscore.js:141:16,41: boolean

underscore.js:160:18,36: property length
Property not found in
  [LIB] core.js:47:1,65: Boolean

underscore.js:164:25,37: access of computed property/element
Computed property/element cannot be accessed on
  underscore.js:159:16,42: boolean

underscore.js:167:27,37: access of computed property/element
Computed property/element cannot be accessed on
  underscore.js:159:16,42: boolean

underscore.js:207:19,37: property length
Property not found in
  [LIB] core.js:47:1,65: Boolean

underscore.js:210:27,37: access of computed property/element
Computed property/element cannot be accessed on
  underscore.js:206:16,41: boolean

underscore.js:222:19,37: property length
Property not found in
  [LIB] core.js:47:1,65: Boolean

underscore.js:225:27,37: access of computed property/element
Computed property/element cannot be accessed on
  underscore.js:221:16,41: boolean

underscore.js:676:36,81: function call
Unknown global name

underscore.js:679:33,37: identifier bound
Unknown global name

underscore.js:853:16,42: call of method apply
Method cannot be called on possibly null value
  underscore.js:855:30,33: null

underscore.js:1312:34,51: call of method call
Method cannot be called on possibly undefined value
  underscore.js:1308:34,39: undefined
```
