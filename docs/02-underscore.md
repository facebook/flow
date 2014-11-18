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
underscore.js:140:25,47: function call
Callable signature not found in
  [LIB] core.js:235:1,236:1: statics of TypeError
```

A glimpse at the lines mentioned make it clear it is about the instantiation of classes without the `new` keyword. We simply update lines like:

{% highlight javascript %}
if (!length) throw TypeError(reduceError);
{% endhighlight %}

to

{% highlight javascript %}
if (!length) throw new TypeError(reduceError);
{% endhighlight %}

At the time of writing, Underscore contains seven `Array` instantations and a `TypeError` instantiation that Flow complains about. When updated, this brings our error count down considerably.

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

For the truly brave, here is another diff that shows where one may end up with 0 errors after turning off weak mode. You may notice a lot of Flow type system features in use in this diff, including type aliases, function types, object types, union types, tuple types, and generics.

```diff
@@ -1,7 +1,11 @@
+/* @flow */
 //     Underscore.js 1.7.0
 //     http://underscorejs.org
 //     (c) 2009-2014 Jeremy Ashkenas, DocumentCloud and Investigative Reporters &
 Editors
 //     Underscore may be freely distributed under the MIT license.
+declare var define: any;
+type FUNCTION = (...args:Array<any>)=>any;
+type PREDICATE = (_:any)=>boolean;

 (function() {

@@ -9,7 +13,7 @@
   // --------------

   // Establish the root object, `window` in the browser, or `exports` on the serv
er.
-  var root = this;
+  var root:any = this;

   // Save the previous value of the `_` variable.
   var previousUnderscore = root._;
@@ -33,10 +37,10 @@
     nativeBind         = FuncProto.bind;

   // Create a safe reference to the Underscore object for use below.
-  var _ = function(obj) {
+  var _ = function(obj:any):any {
     if (obj instanceof _) return obj;
     if (!(this instanceof _)) return new _(obj);
-    this._wrapped = obj;
+        var _this:any = this; _this._wrapped = obj;
   };

   // Export the Underscore object for **Node.js**, with
@@ -57,19 +61,19 @@
   // Internal function that returns an efficient (for current engines) version
   // of the passed-in callback, to be repeatedly applied in other Underscore
   // functions.
-  var createCallback = function(func, context, argCount) {
+  var createCallback = function(func:any, context, argCount?) {
     if (context === void 0) return func;
     switch (argCount == null ? 3 : argCount) {
       case 1: return function(value) {
         return func.call(context, value);
       };
-      case 2: return function(value, other) {
+      case 2: return function(value, other?) {
         return func.call(context, value, other);
       };
-      case 3: return function(value, index, collection) {
+      case 3: return function(value, index?, collection?) {
         return func.call(context, value, index, collection);
       };
-      case 4: return function(accumulator, value, index, collection) {
+      case 4: return function(accumulator, value?, index?, collection?) {
         return func.call(context, accumulator, value, index, collection);
       };
     }
@@ -81,7 +85,7 @@
   // A mostly-internal function to generate callbacks that can be applied
   // to each element in a collection, returning the desired result — either
   // identity, an arbitrary callback, a property matcher, or a property accessor.
-  _.iteratee = function(value, context, argCount) {
+  _.iteratee = function(value, context?, argCount?): any {
     if (value == null) return _.identity;
     if (_.isFunction(value)) return createCallback(value, context, argCount);
     if (_.isObject(value)) return _.matches(value);
@@ -94,7 +98,7 @@
   // The cornerstone, an `each` implementation, aka `forEach`.
   // Handles raw objects in addition to array-likes. Treats all
   // sparse array-likes as if they were dense.
-  _.each = _.forEach = function(obj, iteratee, context) {
+  _.each = _.forEach = function(obj:any, iteratee:any, context?):any {
     if (obj == null) return obj;
     iteratee = createCallback(iteratee, context);
     var i, length = obj.length;
@@ -112,12 +116,12 @@
   };

   // Return the results of applying the iteratee to each element.
-  _.map = _.collect = function(obj, iteratee, context) {
+  _.map = _.collect = function(obj:Array<any>, iteratee, context?):Array<any> {
     if (obj == null) return [];
     iteratee = _.iteratee(iteratee, context);
-    var keys = obj.length !== +obj.length && _.keys(obj),
+    var keys:any = obj.length !== +obj.length && _.keys(obj),
         length = (keys || obj).length,
-        results = Array(length),
+        results = new Array(length),
         currentKey;
     for (var index = 0; index < length; index++) {
       currentKey = keys ? keys[index] : index;
@@ -130,10 +134,10 @@

   // **Reduce** builds up a single result from a list of values, aka `inject`,
   // or `foldl`.
-  _.reduce = _.foldl = _.inject = function(obj, iteratee, memo, context) {
+  _.reduce = _.foldl = _.inject = function<T>(obj:?Array<T>, iteratee:(memo:T,v
alue:T,key:number,obj:Array<T>)=>T, memo:T, context:any):T {
     if (obj == null) obj = [];
     iteratee = createCallback(iteratee, context, 4);
-    var keys = obj.length !== +obj.length && _.keys(obj),
+    var keys:any = obj.length !== +obj.length && _.keys(obj),
         length = (keys || obj).length,
         index = 0, currentKey;
     if (arguments.length < 3) {
@@ -148,10 +152,10 @@
   };

   // The right-associative version of reduce, also known as `foldr`.
-  _.reduceRight = _.foldr = function(obj, iteratee, memo, context) {
+  _.reduceRight = _.foldr = function<T>(obj:?Array<T>, iteratee:(memo:T,value:T
,key:number,obj:Array<T>)=>T, memo:T, context:any):T {
     if (obj == null) obj = [];
     iteratee = createCallback(iteratee, context, 4);
-    var keys = obj.length !== + obj.length && _.keys(obj),
+    var keys:any = obj.length !== + obj.length && _.keys(obj),
         index = (keys || obj).length,
         currentKey;
     if (arguments.length < 3) {
@@ -166,10 +170,10 @@
   };

   // Return the first value which passes a truth test. Aliased as `detect`.
-  _.find = _.detect = function(obj, predicate, context) {
+  _.find = _.detect = function<T>(obj:?Array<T>, predicate:PREDICATE, context?)
:?T {
     var result;
     predicate = _.iteratee(predicate, context);
-    _.some(obj, function(value, index, list) {
+    _.some(obj, function(value, index?, list?) {
       if (predicate(value, index, list)) {
         result = value;
         return true;
@@ -180,7 +184,7 @@

   // Return all the elements that pass a truth test.
   // Aliased as `select`.
-  _.filter = _.select = function(obj, predicate, context) {
+  _.filter = _.select = function<T>(obj:?Array<T>, predicate:PREDICATE, context
?):Array<T> {
     var results = [];
     if (obj == null) return results;
     predicate = _.iteratee(predicate, context);
@@ -191,16 +195,16 @@
   };

   // Return all the elements for which a truth test fails.
-  _.reject = function(obj, predicate, context) {
+  _.reject = function<T>(obj:?Array<T>, predicate:PREDICATE, context?):Array<T>
 {
     return _.filter(obj, _.negate(_.iteratee(predicate)), context);
   };

   // Determine whether all of the elements match a truth test.
   // Aliased as `all`.
-  _.every = _.all = function(obj, predicate, context) {
+  _.every = _.all = function<T>(obj:?Array<T>, predicate:PREDICATE, context?):b
oolean {
     if (obj == null) return true;
     predicate = _.iteratee(predicate, context);
-    var keys = obj.length !== +obj.length && _.keys(obj),
+    var keys:any = obj.length !== +obj.length && _.keys(obj),
         length = (keys || obj).length,
         index, currentKey;
     for (index = 0; index < length; index++) {
@@ -212,10 +216,10 @@

   // Determine if at least one element in the object matches a truth test.
   // Aliased as `any`.
-  _.some = _.any = function(obj, predicate, context) {
+  _.some = _.any = function<T>(obj:?Array<T>, predicate:PREDICATE, context?):bo
olean {
     if (obj == null) return false;
     predicate = _.iteratee(predicate, context);
-    var keys = obj.length !== +obj.length && _.keys(obj),
+    var keys:any = obj.length !== +obj.length && _.keys(obj),
         length = (keys || obj).length,
         index, currentKey;
     for (index = 0; index < length; index++) {
@@ -227,14 +231,14 @@

   // Determine if the array or object contains a given value (using `===`).
   // Aliased as `include`.
-  _.contains = _.include = function(obj, target) {
+  _.contains = _.include = function(obj, target):boolean {
     if (obj == null) return false;
     if (obj.length !== +obj.length) obj = _.values(obj);
     return _.indexOf(obj, target) >= 0;
   };

   // Invoke a method (with arguments) on every item in a collection.
-  _.invoke = function(obj, method) {
+  _.invoke = function(obj:Array<any>, method):Array<any> {
     var args = slice.call(arguments, 2);
     var isFunc = _.isFunction(method);
     return _.map(obj, function(value) {
@@ -243,24 +247,24 @@
   };

   // Convenience version of a common use case of `map`: fetching a property.
-  _.pluck = function(obj, key) {
+  _.pluck = function(obj:Array<any>, key):Array<any> {
     return _.map(obj, _.property(key));
   };

   // Convenience version of a common use case of `filter`: selecting only objects
   // containing specific `key:value` pairs.
-  _.where = function(obj, attrs) {
+  _.where = function<T>(obj:Array<T>, attrs):Array<T> {
     return _.filter(obj, _.matches(attrs));
   };

   // Convenience version of a common use case of `find`: getting the first object
   // containing specific `key:value` pairs.
-  _.findWhere = function(obj, attrs) {
+  _.findWhere = function<T>(obj:Array<T>, attrs):?T {
     return _.find(obj, _.matches(attrs));
   };

   // Return the maximum element (or element-based computation).
-  _.max = function(obj, iteratee, context) {
+  _.max = function(obj, iteratee:any, context?):any {
     var result = -Infinity, lastComputed = -Infinity,
         value, computed;
     if (iteratee == null && obj != null) {
@@ -300,7 +304,7 @@
       iteratee = _.iteratee(iteratee, context);
       _.each(obj, function(value, index, list) {
         computed = iteratee(value, index, list);
-        if (computed < lastComputed || computed === Infinity && result === Infini
ty) {
+        if ((computed < lastComputed) || computed === Infinity && result === In
finity) {
           result = value;
           lastComputed = computed;
         }
@@ -311,10 +315,10 @@

   // Shuffle a collection, using the modern version of the
   // [Fisher-Yates shuffle](http://en.wikipedia.org/wiki/Fisher–Yates_shuffle).
-  _.shuffle = function(obj) {
+  _.shuffle = function(obj):Array<any> {
     var set = obj && obj.length === +obj.length ? obj : _.values(obj);
     var length = set.length;
-    var shuffled = Array(length);
+    var shuffled = new Array(length);
     for (var index = 0, rand; index < length; index++) {
       rand = _.random(0, index);
       if (rand !== index) shuffled[index] = shuffled[rand];
@@ -326,7 +330,7 @@
   // Sample **n** random values from a collection.
   // If **n** is not specified, returns a single random element.
   // The internal `guard` argument allows it to work with `map`.
-  _.sample = function(obj, n, guard) {
+  _.sample = function<T>(obj:Array<T>, n?:number, guard?:boolean):T|Array<T> {
     if (n == null || guard) {
       if (obj.length !== +obj.length) obj = _.values(obj);
       return obj[_.random(obj.length - 1)];
@@ -335,7 +339,7 @@
   };

   // Sort the object's values by a criterion produced by an iteratee.
-  _.sortBy = function(obj, iteratee, context) {
+  _.sortBy = function<T>(obj:Array<T>, iteratee:FUNCTION, context?:any):Array<T
> {
     iteratee = _.iteratee(iteratee, context);
     return _.pluck(_.map(obj, function(value, index, list) {
       return {
@@ -388,7 +392,7 @@

   // Use a comparator function to figure out the smallest index at which
   // an object should be inserted so as to maintain order. Uses binary search.
-  _.sortedIndex = function(array, obj, iteratee, context) {
+  _.sortedIndex = function(array:Array<any>, obj, iteratee?, context?):number {
     iteratee = _.iteratee(iteratee, context, 1);
     var value = iteratee(obj);
     var low = 0, high = array.length;
@@ -400,7 +404,7 @@
   };

   // Safely create a real, live array from anything iterable.
-  _.toArray = function(obj) {
+  _.toArray = function(obj:any):Array<any> {
     if (!obj) return [];
     if (_.isArray(obj)) return slice.call(obj);
     if (obj.length === +obj.length) return _.map(obj, _.identity);
@@ -408,14 +412,14 @@
   };

   // Return the number of elements in an object.
-  _.size = function(obj) {
+  _.size = function(obj):number {
     if (obj == null) return 0;
     return obj.length === +obj.length ? obj.length : _.keys(obj).length;
   };

   // Split a collection into two arrays: one whose elements all satisfy the given
   // predicate, and one whose elements all do not satisfy the predicate.
-  _.partition = function(obj, predicate, context) {
+  _.partition = function<T>(obj:Array<T>, predicate:PREDICATE, context?:any):[A
rray<T>,Array<T>] {
     predicate = _.iteratee(predicate, context);
     var pass = [], fail = [];
     _.each(obj, function(value, key, obj) {
@@ -430,7 +434,7 @@
   // Get the first element of an array. Passing **n** will return the first N
   // values in the array. Aliased as `head` and `take`. The **guard** check
   // allows it to work with `_.map`.
-  _.first = _.head = _.take = function(array, n, guard) {
+  _.first = _.head = _.take = function<T>(array:?Array<T>, n?:number, guard?:bo
olean):?T|Array<T> {
     if (array == null) return void 0;
     if (n == null || guard) return array[0];
     if (n < 0) return [];
@@ -441,13 +445,13 @@
   // the arguments object. Passing **n** will return all the values in
   // the array, excluding the last N. The **guard** check allows it to work with
   // `_.map`.
-  _.initial = function(array, n, guard) {
+  _.initial = function(array:Array<any>, n?:number, guard?:boolean):Array<any>
{
     return slice.call(array, 0, Math.max(0, array.length - (n == null || guard ?
1 : n)));
   };

   // Get the last element of an array. Passing **n** will return the last N
   // values in the array. The **guard** check allows it to work with `_.map`.
-  _.last = function(array, n, guard) {
+  _.last = function<T>(array:?Array<T>, n?:number, guard?:boolean):?T|Array<T>
{
     if (array == null) return void 0;
     if (n == null || guard) return array[array.length - 1];
     return slice.call(array, Math.max(array.length - n, 0));
@@ -457,17 +461,17 @@
   // Especially useful on the arguments object. Passing an **n** will return
   // the rest N values in the array. The **guard**
   // check allows it to work with `_.map`.
-  _.rest = _.tail = _.drop = function(array, n, guard) {
+  _.rest = _.tail = _.drop = function(array:Array<any>, n?:number, guard?:boole
an):Array<any> {
     return slice.call(array, n == null || guard ? 1 : n);
   };

   // Trim out all falsy values from an array.
-  _.compact = function(array) {
+  _.compact = function<T>(array:Array<T>):Array<T> {
     return _.filter(array, _.identity);
   };

   // Internal implementation of a recursive `flatten` function.
-  var flatten = function(input, shallow, strict, output) {
+  var flatten = function(input:Array<any>, shallow:boolean, strict:boolean, out
put:Array<any>):Array<any> {
     if (shallow && _.every(input, _.isArray)) {
       return concat.apply(output, input);
     }
@@ -485,19 +489,19 @@
   };

   // Flatten out an array, either recursively (by default), or just one level.
-  _.flatten = function(array, shallow) {
+  _.flatten = function(array:Array<any>, shallow:boolean):Array<any> {
     return flatten(array, shallow, false, []);
   };

   // Return a version of the array that does not contain the specified value(s).
-  _.without = function(array) {
+  _.without = function<T>(array:Array<T>):Array<T> {
     return _.difference(array, slice.call(arguments, 1));
   };

   // Produce a duplicate-free version of the array. If the array has already
   // been sorted, you have the option of using a faster algorithm.
   // Aliased as `unique`.
-  _.uniq = _.unique = function(array, isSorted, iteratee, context) {
+  _.uniq = _.unique = function(array:?Array<any>, isSorted?:boolean, iteratee?:
boolean, context?):Array<any> {
     if (array == null) return [];
     if (!_.isBoolean(isSorted)) {
       context = iteratee;
@@ -527,13 +531,13 @@

   // Produce an array that contains the union: each distinct element from all of
   // the passed-in arrays.
-  _.union = function() {
+  _.union = function():Array<any> {
     return _.uniq(flatten(arguments, true, true, []));
   };

   // Produce an array that contains every item shared between all the
   // passed-in arrays.
-  _.intersection = function(array) {
+  _.intersection = function(array:?Array<any>):Array<any> {
     if (array == null) return [];
     var result = [];
     var argsLength = arguments.length;
@@ -550,7 +554,7 @@

   // Take the difference between one array and a number of other arrays.
   // Only the elements present in just the first array will remain.
-  _.difference = function(array) {
+  _.difference = function<T>(array:Array<T>):Array<T> {
     var rest = flatten(slice.call(arguments, 1), true, true, []);
     return _.filter(array, function(value){
       return !_.contains(rest, value);
@@ -559,10 +563,10 @@

   // Zip together multiple lists into a single array -- elements that share
   // an index go together.
-  _.zip = function(array) {
+  _.zip = function(array:?Array<any>):Array<any> {
     if (array == null) return [];
     var length = _.max(arguments, 'length').length;
-    var results = Array(length);
+    var results = new Array(length);
     for (var i = 0; i < length; i++) {
       results[i] = _.pluck(arguments, i);
     }
@@ -589,7 +593,7 @@
   // or -1 if the item is not included in the array.
   // If the array is large and already in sort order, pass `true`
   // for **isSorted** to use binary search.
-  _.indexOf = function(array, item, isSorted) {
+  _.indexOf = function<T>(array:Array<T>, item:T, isSorted?:boolean):number {
     if (array == null) return -1;
     var i = 0, length = array.length;
     if (isSorted) {
@@ -604,7 +608,7 @@
     return -1;
   };

-  _.lastIndexOf = function(array, item, from) {
+  _.lastIndexOf = function<T>(array:Array<T>, item:T, from:number):number {
     if (array == null) return -1;
     var idx = array.length;
     if (typeof from == 'number') {
@@ -617,7 +621,7 @@
   // Generate an integer Array containing an arithmetic progression. A port of
   // the native Python `range()` function. See
   // [the Python documentation](http://docs.python.org/library/functions.html#ran
ge).
-  _.range = function(start, stop, step) {
+  _.range = function(start:number, stop:number, step:number):Array<number> {
     if (arguments.length <= 1) {
       stop = start || 0;
       start = 0;
@@ -625,7 +629,7 @@
     step = step || 1;

     var length = Math.max(Math.ceil((stop - start) / step), 0);
-    var range = Array(length);
+    var range = new Array(length);

     for (var idx = 0; idx < length; idx++, start += step) {
       range[idx] = start;
@@ -643,17 +647,17 @@
   // Create a function bound to a given object (assigning `this`, and arguments,
   // optionally). Delegates to **ECMAScript 5**'s native `Function.bind` if
   // available.
-  _.bind = function(func, context) {
+  _.bind = function(func, context) {
     var args, bound;
     if (nativeBind && func.bind === nativeBind) return nativeBind.apply(func, sli
ce.call(arguments, 1));
     if (!_.isFunction(func)) throw new TypeError('Bind must be called on a functi
on');
     args = slice.call(arguments, 2);
-    bound = function() {
-      if (!(this instanceof bound)) return func.apply(context, args.concat(slice.
call(arguments)));
+    bound = function() {
+      if (!(this instanceof bound)) return func.apply(context, args && args.conca
t(slice.call(arguments)));
       Ctor.prototype = func.prototype;
       var self = new Ctor;
-      Ctor.prototype = null;
-      var result = func.apply(self, args.concat(slice.call(arguments)));
+      Ctor.prototype = {};
+      var result = func.apply(self, args && args.concat(slice.call(arguments)));
       if (_.isObject(result)) return result;
       return self;
     };
@@ -703,16 +707,16 @@

   // Delays a function for the given number of milliseconds, and then calls
   // it with the arguments supplied.
-  _.delay = function(func, wait) {
+  _.delay = function(func?:any, wait?=0) {
     var args = slice.call(arguments, 2);
     return setTimeout(function(){
-      return func.apply(null, args);
+      return func && func.apply(null, args);
     }, wait);
   };

   // Defers a function, scheduling it to run after the current call stack has
   // cleared.
-  _.defer = function(func) {
+  _.defer = function(func:any) {
     return _.delay.apply(_, [func, 1].concat(slice.call(arguments, 1)));
   };

@@ -721,7 +725,7 @@
   // as much as it can, without ever going more than once per `wait` duration;
   // but if you'd like to disable the execution on the leading edge, pass
   // `{leading: false}`. To disable execution on the trailing edge, ditto.
-  _.throttle = function(func, wait, options) {
+  _.throttle = function(func, wait:number, options:{ leading: boolean; trailing
: boolean }) {
     var context, args, result;
     var timeout = null;
     var previous = 0;
@@ -755,11 +759,11 @@
   // be triggered. The function will be called after it stops being called for
   // N milliseconds. If `immediate` is passed, trigger the function on the
   // leading edge, instead of the trailing.
-  _.debounce = function(func, wait, immediate) {
-  var timeout, args, context, timestamp, result;
+  _.debounce = function(func, wait:number, immediate?) {
+  var timeout, args, context, timestamp=0, result;

     var later = function() {
-      var last = _.now() - timestamp;
+      var last:number = _.now() - timestamp;

       if (last < wait && last > 0) {
         timeout = setTimeout(later, wait - last);
@@ -790,12 +794,12 @@
   // Returns the first function passed as an argument to the second,
   // allowing you to adjust arguments, run code before and after, and
   // conditionally execute the original function.
-  _.wrap = function(func, wrapper) {
+  _.wrap = function(func:FUNCTION, wrapper:FUNCTION):FUNCTION {
     return _.partial(wrapper, func);
   };

   // Returns a negated version of the passed-in predicate.
-  _.negate = function(predicate) {
+  _.negate = function(predicate:PREDICATE):PREDICATE {
     return function() {
       return !predicate.apply(this, arguments);
     };
@@ -803,7 +807,7 @@

   // Returns a function that is the composition of a list of functions, each
   // consuming the return value of the function that follows.
-  _.compose = function() {
+  _.compose = function():(_:any)=>any {
     var args = arguments;
     var start = args.length - 1;
     return function() {
@@ -815,7 +819,7 @@
   };

   // Returns a function that will only be executed after being called N times.
-  _.after = function(times, func) {
+  _.after = function(times:number, func:FUNCTION):FUNCTION {
     return function() {
       if (--times < 1) {
         return func.apply(this, arguments);
@@ -824,11 +828,11 @@
   };

   // Returns a function that will only be executed before being called N times.
-  _.before = function(times, func) {
+  _.before = function(times?=0, func?:FUNCTION) {
     var memo;
     return function() {
       if (--times > 0) {
-        memo = func.apply(this, arguments);
+        if (func != null) memo = func.apply(this, arguments);
       } else {
         func = null;
       }
@@ -845,7 +849,7 @@

   // Retrieve the names of an object's properties.
   // Delegates to **ECMAScript 5**'s native `Object.keys`
-  _.keys = function(obj) {
+  _.keys = function(obj:any):Array<string> {
     if (!_.isObject(obj)) return [];
     if (nativeKeys) return nativeKeys(obj);
     var keys = [];
@@ -854,10 +858,10 @@
   };

   // Retrieve the values of an object's properties.
-  _.values = function(obj) {
+  _.values = function<T>(obj:any):Array<T> {
     var keys = _.keys(obj);
     var length = keys.length;
-    var values = Array(length);
+    var values = new Array(length);
     for (var i = 0; i < length; i++) {
       values[i] = obj[keys[i]];
     }
@@ -865,10 +869,10 @@
   };

   // Convert an object into a list of `[key, value]` pairs.
-  _.pairs = function(obj) {
+  _.pairs = function<T>(obj:any):Array<[string,T]> {
     var keys = _.keys(obj);
     var length = keys.length;
-    var pairs = Array(length);
+    var pairs = new Array(length);
     for (var i = 0; i < length; i++) {
       pairs[i] = [keys[i], obj[keys[i]]];
     }
@@ -876,7 +880,7 @@
   };

   // Invert the keys and values of an object. The values must be serializable.
-  _.invert = function(obj) {
+  _.invert = function(obj) {
     var result = {};
     var keys = _.keys(obj);
     for (var i = 0, length = keys.length; i < length; i++) {
@@ -887,7 +891,7 @@

   // Return a sorted list of the function names available on the object.
   // Aliased as `methods`
-  _.functions = _.methods = function(obj) {
+  _.functions = _.methods = function(obj:any):Array<string> {
     var names = [];
     for (var key in obj) {
       if (_.isFunction(obj[key])) names.push(key);
@@ -971,7 +975,7 @@
   };

   // Internal recursive comparison function for `isEqual`.
-  var eq = function(a, b, aStack, bStack) {
+  var eq = function(a:any, b:any, aStack:Array<any>, bStack:Array<any>):boolean
 {
     // Identical objects are equal. `0 === -0`, but they aren't identical.
     // See the [Harmony `egal` proposal](http://wiki.ecmascript.org/doku.php?id=h
armony:egal).
     if (a === b) return a !== 0 || 1 / a === 1 / b;
@@ -1061,13 +1065,13 @@
   };

   // Perform a deep comparison to check if two objects are equal.
-  _.isEqual = function(a, b) {
+  _.isEqual = function<T,S>(a:T, b:S):boolean {
     return eq(a, b, [], []);
   };

   // Is a given array, string, or object empty?
   // An "empty" object has no enumerable own-properties.
-  _.isEmpty = function(obj) {
+  _.isEmpty = function(obj):boolean {
     if (obj == null) return true;
     if (_.isArray(obj) || _.isString(obj) || _.isArguments(obj)) return obj.lengt
h === 0;
     for (var key in obj) if (_.has(obj, key)) return false;
@@ -1075,72 +1079,73 @@
   };

   // Is a given value a DOM element?
-  _.isElement = function(obj) {
+  _.isElement = function(obj):boolean {
     return !!(obj && obj.nodeType === 1);
   };

   // Is a given value an array?
   // Delegates to ECMA5's native Array.isArray
-  _.isArray = nativeIsArray || function(obj) {
+  _.isArray = nativeIsArray || function(obj):boolean {
     return toString.call(obj) === '[object Array]';
   };

   // Is a given variable an object?
-  _.isObject = function(obj) {
+  _.isObject = function(obj):boolean {
     var type = typeof obj;
     return type === 'function' || type === 'object' && !!obj;
   };

   // Add some isType methods: isArguments, isFunction, isString, isNumber, isDate
, isRegExp.
   _.each(['Arguments', 'Function', 'String', 'Number', 'Date', 'RegExp'], functio
n(name) {
-    _['is' + name] = function(obj) {
+    var __:any = _;
+    __['is' + name] = function(obj):boolean {
       return toString.call(obj) === '[object ' + name + ']';
     };
   });

   // Define a fallback version of the method in browsers (ahem, IE), where
   // there isn't any inspectable "Arguments" type.
-  if (!_.isArguments(arguments)) {
-    _.isArguments = function(obj) {
+  if (!_.isArguments(arguments)) {
+    _.isArguments = function(obj):boolean {
       return _.has(obj, 'callee');
     };
   }

   // Optimize `isFunction` if appropriate. Work around an IE 11 bug.
   if (typeof /./ !== 'function') {
-    _.isFunction = function(obj) {
+    _.isFunction = function(obj):boolean {
       return typeof obj == 'function' || false;
     };
   }

   // Is a given object a finite number?
-  _.isFinite = function(obj) {
+  _.isFinite = function(obj):boolean {
     return isFinite(obj) && !isNaN(parseFloat(obj));
   };

   // Is the given value `NaN`? (NaN is the only number which does not equal itsel
f).
-  _.isNaN = function(obj) {
+  _.isNaN = function(obj):boolean {
     return _.isNumber(obj) && obj !== +obj;
   };

   // Is a given value a boolean?
-  _.isBoolean = function(obj) {
+  _.isBoolean = function(obj):boolean {
     return obj === true || obj === false || toString.call(obj) === '[object Boole
an]';
   };

   // Is a given value equal to null?
-  _.isNull = function(obj) {
+  _.isNull = function(obj):boolean {
     return obj === null;
   };

   // Is a given variable undefined?
-  _.isUndefined = function(obj) {
+  _.isUndefined = function(obj):boolean {
     return obj === void 0;
   };

   // Shortcut function for checking if an object has a given property directly
   // on itself (in other words, not on a prototype).
-  _.has = function(obj, key) {
+  _.has = function(obj, key:string):boolean {
     return obj != null && hasOwnProperty.call(obj, key);
   };

@@ -1190,14 +1195,14 @@

   // Run a function **n** times.
   _.times = function(n, iteratee, context) {
-    var accum = Array(Math.max(0, n));
+    var accum = new Array(Math.max(0, n));
     iteratee = createCallback(iteratee, context, 1);
     for (var i = 0; i < n; i++) accum[i] = iteratee(i);
     return accum;
   };

   // Return a random integer between min and max (inclusive).
-  _.random = function(min, max) {
+  _.random = function(min, max?) {
     if (max == null) {
       max = min;
       min = 0;
@@ -1206,7 +1211,7 @@
   };

   // A (possibly faster) way to get the current timestamp as an integer.
-  _.now = Date.now || function() {
+  _.now = Date.now || function():number {
     return new Date().getTime();
   };

@@ -1228,8 +1233,8 @@
     };
     // Regexes for identifying a key that needs to be escaped
     var source = '(?:' + _.keys(map).join('|') + ')';
-    var testRegexp = RegExp(source);
-    var replaceRegexp = RegExp(source, 'g');
+    var testRegexp = new RegExp(source);
+    var replaceRegexp = new RegExp(source, 'g');
     return function(string) {
       string = string == null ? '' : '' + string;
       return testRegexp.test(string) ? string.replace(replaceRegexp, escaper) : s
tring;
@@ -1293,7 +1298,7 @@
     settings = _.defaults({}, settings, _.templateSettings);

     // Combine delimiters into one regular expression via alternation.
-    var matcher = RegExp([
+    var matcher = new RegExp([
       (settings.escape || noMatch).source,
       (settings.interpolate || noMatch).source,
       (settings.evaluate || noMatch).source
@@ -1363,9 +1368,10 @@
   };

   // Add your own custom functions to the Underscore object.
-  _.mixin = function(obj) {
-    _.each(_.functions(obj), function(name) {
-      var func = _[name] = obj[name];
+  _.mixin = function(obj:any) {
+    _.each(_.functions(obj), function(name) {
+      var __:any = _;
+      var func = __[name] = obj[name];
       _.prototype[name] = function() {
         var args = [this._wrapped];
         push.apply(args, arguments);
@@ -1379,7 +1385,8 @@

   // Add all mutator Array functions to the wrapper.
   _.each(['pop', 'push', 'reverse', 'shift', 'sort', 'splice', 'unshift'], functi
on(name) {
-    var method = ArrayProto[name];
+    var _ArrayProto:any = ArrayProto;
+    var method = _ArrayProto[name];
     _.prototype[name] = function() {
       var obj = this._wrapped;
       method.apply(obj, arguments);
@@ -1389,8 +1396,9 @@
   });

   // Add all accessor Array functions to the wrapper.
-  _.each(['concat', 'join', 'slice'], function(name) {
-    var method = ArrayProto[name];
+  _.each(['concat', 'join', 'slice'], function(name) {
+    var _ArrayProto:any = ArrayProto;
+    var method = _ArrayProto[name];
     _.prototype[name] = function() {
       return result.call(this, method.apply(this._wrapped, arguments));
     };
```
