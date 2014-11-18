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

Now when we run Flow again, we get a small page of errors:

```bbcode
...
Found 26 errors
```

This seems quite manageable. Let's look at each of these errors in turn and modify the file as required. (Note that the line numbers in this article may not match your own results as Underscore itself may have been updated since it was written).

## Global objects

Our first two warnings are early on in the file, because Underscore tries to associate itself with the global object passed in to the self-executing anonymous function.

```bbcode
underscore.js:16:28,33: property _
Property cannot be accessed on global object

underscore.js:51:5,14: assignment of property _
Property cannot be assigned on global object
```

We can easily mitigate Flow's warnings about the `_` property on this object by declaring that the `root` variable is of type `any`:

{% highlight javascript %}
// ...`window` in the browser, or `exports` on the server.
var root: any = this;
{% endhighlight %}

The final error message (where Underscore is trying to see if RequireJS is present) fails similarly:

```bbcode
Unknown global name: define
```

We can add a declaration just ahead of the usage to show that this too is of type `any`:

{% highlight javascript %}
declare var define: any;
if (typeof define === 'function' && define.amd) {
  ...
{% endhighlight %}

We're three errors down already.

## Instantiation

Another common type of error reported is this:

```bbcode
underscore.js:673:19,31: function call
Callable signature not found in
  [LIB] core.js:235:1,236:1: statics of TypeError
```

A glimpse at the lines mentioned make it clear it is about the instantiation of classes without the `new` keyword. We simply update lines like:

{% highlight javascript %}
if (!_.isFunction(func)) throw TypeError('Bind must be called on a function');
{% endhighlight %}

to

{% highlight javascript %}
if (!_.isFunction(func)) throw new TypeError('Bind must be called on a function');
{% endhighlight %}

At the time of writing, Underscore contains seven `Array` instantations and a `TypeError`. When updated, this brings our error count down considerably.

## Type inference in conditions

Another type of common issue reported by Flow on Underscore (and indeed many concisely-written JavaScript libraries) is that encountered when a variable is both used as, and assigned in, conditions. In these instances a variable's type is typically deduced to be either a boolean `false` or an assigned value of another type. Subsequent code then assumes this hybrid type. For example, in several places in Underscore, Flow encounters code like this:

```javascript
var keys = obj.length !== +obj.length && _.keys(obj),
    length = (keys || obj).length,
    ...
  currentKey = keys ? keys[index] : index;
```

Here, the variable `keys` could be a boolean (if the first part of the condition is false), or it could be an array. At run time in the former case, the `length` variable will then take its value from `obj` - which is fine - and in the latter case, from `keys`, which is also fine (since that variable is an array in that case). The later `currentKey` assignment here is also relying on `keys` being either falsy or an array.

Unfortunately Flow is not currently able to deduce the type of `keys` in complex situations like this, and will report that the `length` condition is not available on booleans.

```bbcode
underscore.js:124:19,37: property length
Property not found in
  [LIB] core.js:47:1,65: Boolean
```

To resolve this, we have a few options. We can rework this logic to be more explicit, so that Flow can correctly infer the types in play. This is one alternative:

```javascript
var useKeys = obj.length !== +obj.length,
    keys = useKeys ? _.keys(obj) : [],
    length = useKeys ? keys.length : obj.length,
    ...
  currentKey = useKeys ? keys[index] : index;
```

Another is to demonstrate that we are paying more attention to the type at run time by using the type itself in the subsequent ternary conditions:

```javascript
var keys = obj.length !== +obj.length && _.keys(obj),
    length = (keys instanceof Array) ? keys.length : obj.length,
    ...
  currentKey = (keys instanceof Array) ? keys[index] : index;
```

Suffice to say, when making changes like this, it's worth checking the unit tests to make sure you've updated the behavior correctly.

If rewriting this logic seems like too much work, we can also just tell Flow that we know this local variable can be of multiple types and that it shouldn't check the validity of any properties accessed on it. To do this, simply annotate the variable's first declaration to be `any`:

```javascript
var keys: any = obj.length !== +obj.length && _.keys(obj),
```

At the time of writing, there are five examples of this pattern in use. Fixing them brings down our error count to just three outstanding issues.

## Final nits

One of our remaining issues is this simple error:

```bbcode
underscore.js:851:16,42: call of method apply
Method cannot be called on possibly null value
  underscore.js:853:30,33: null
```

This is a simple case of needing to ensure that a function is can be called, by turning:

```javascript
if (--times > 0) {
  memo = func.apply(this, arguments);
}
```

into

```javascript
if (--times > 0 && func instanceof Function) {
  memo = func.apply(this, arguments);
}
```

Similarly this error is alerting us to calling a variable that may not represent a function. Underscore's author's do indeed check as much in this case, but Flow is not able to determine that that is what the `_.isFunction` is doing.

```bbcode
underscore.js:1310:34,51: call of method call
Method cannot be called on possibly undefined value
  underscore.js:1306:34,39: undefined
```

To placate Flow, we can be explicit and just turn:

```javascript
return _.isFunction(value) ? value.call(object) : value;
```

into

```javascript
return (value instanceof Function) ? value.call(object) : value;
```

One to go. The final error is:

```bbcode
underscore.js:677:33,37: identifier bound
Unknown global name
```

This is actually a known issue with Flow, caused by a returned function being given a name and the innards of the function not recognizing that variable. This takes place in `_.bind`:

```javascript
return function bound() {
  return executeBound(func, bound, context, this, args.concat(slice.call(arguments)));
};
```

For now, this is easily fixed by assigning the function to a variable more explicitly:

```javascript
var bound = function () {
  return executeBound(func, bound, context, this, args.concat(slice.call(arguments)));
};
return bound;
```

(The same pattern is used in `_.partial` too.)

## All done

With those small changes, we now have a clean bill of health in weak mode:

```bbcode
Found 0 errors
```

It is tempting to remove the `weak` declaration and start to work on some of the additional type issues that Flow asserts. It is important to understand though that will appear a somewhat daunting task until you actually start adding annotations on both the arguments and return types of Underscore's many functions.

In the meantime, hopefully this article has provided an insight into the sort of things that Flow is checking for, and some of the idioms used in popular JavaScript libraries that Flow finds harder to type check. And over time, as Flow improves, the nature of the issues reported will also change. Good luck!

## Appendix

For reference, here is the diff for the changes made in this walkthough.

```diff
@@ -0,0 +1 @@
+// @flow weak
@@ -12 +13 @@
-  var root = this;
+  var root: any = this;
@@ -120 +121 @@
-    var keys = obj.length !== +obj.length && _.keys(obj),
+    var keys: any = obj.length !== +obj.length && _.keys(obj),
@@ -122 +123 @@
-        results = Array(length),
+        results = new Array(length),
@@ -138 +139 @@
-    var keys = obj.length !== +obj.length && _.keys(obj),
+    var keys: any = obj.length !== +obj.length && _.keys(obj),
@@ -156 +157 @@
-    var keys = obj.length !== + obj.length && _.keys(obj),
+    var keys: any = obj.length !== + obj.length && _.keys(obj),
@@ -203 +204 @@
-    var keys = obj.length !== +obj.length && _.keys(obj),
+    var keys: any = obj.length !== +obj.length && _.keys(obj),
@@ -218 +219 @@
-    var keys = obj.length !== +obj.length && _.keys(obj),
+    var keys: any = obj.length !== +obj.length && _.keys(obj),
@@ -317 +318 @@
-    var shuffled = Array(length);
+    var shuffled = new Array(length);
@@ -566 +567 @@
-    var results = Array(length);
+    var results = new Array(length);
@@ -641 +642 @@
-    var range = Array(length);
+    var range = new Array(length);
@@ -673 +674 @@
-    if (!_.isFunction(func)) throw TypeError('Bind must be called on a function');
+    if (!_.isFunction(func)) throw new TypeError('Bind must be called on a function');
@@ -675 +676 @@
-    return function bound() {
+    var bound = function () {
@@ -677,0 +679 @@
+    return bound;
@@ -685 +687 @@
-    return function bound() {
+    var bound = function () {
@@ -693,0 +696 @@
+    return bound;
@@ -849 +852 @@
-      if (--times > 0) {
+      if (--times > 0 && func instanceof Function) {
@@ -908 +911 @@
-    var values = Array(length);
+    var values = new Array(length);
@@ -919 +922 @@
-    var pairs = Array(length);
+    var pairs = new Array(length);
@@ -1254 +1257 @@
-    var accum = Array(Math.max(0, n));
+    var accum = new Array(Math.max(0, n));
@@ -1309 +1312 @@
-    return _.isFunction(value) ? value.call(object) : value;
+    return (value instanceof Function) ? value.call(object) : value;
@@ -1473,0 +1477 @@
+  declare var define: any;
```
