---
id: nullable-types
title: Maybe Types
layout: docs
permalink: /docs/nullable-types.html
prev: functions.html
next: destructuring.html
---

In JavaScript, `null` implicitly converts to all the primitive types; it is
also a valid inhabitant of any object type.

In contrast, Flow considers `null` to be a distinct value that is not part of
any other type. For example, the following code does not typecheck:

{% highlight javascript linenos=table %}
/* @flow */
var o = null;
print(o.x);
{% endhighlight %}

```bbcode
file.js:3:7,9: property x
Property cannot be accessed on possibly null value
  file.js:2:9,12: null
```

## Type Annotating Null

Any type `T` can be made to include `null` (and the related value `undefined`) by writing `?T`: the latter type
is a maybe type that describes `null` (or `undefined`) or the set of values of `T`.

{% highlight javascript linenos=table %}
/* @flow */
var o: ?string = null;
print(o.length);
{% endhighlight %}

```bbcode
file.js:3:7,14: property length
Property cannot be accessed on possibly null or undefined value
  file.js:2:9,14: ?string
```

Relaxing a type into a maybe type makes it a valid annotation for any location that may
contain `null`, but it still does not allow useful operations to be performed
on it (as shown by the code above). To do so, we must perform a `null` check,
as follows:

{% highlight javascript linenos=table %}
/* @flow */
var o: ?string = null;
if (o == null) {
  o = 'hello';
}
print(o.length);
{% endhighlight %}

In this code, after the `if`-statement Flow infers that `o` is not `null` (it
either was `null` before the `if`-statement but is now an object, or was not
`null` before the `if`-statement). So the code typechecks.

This illustrates an interesting feature of Flow: it understands the effects of
some dynamic type tests and can adjust the types of local variables
accordingly (in technical terms, Flow's analysis is path-sensitive).

## Maybe and Objects

In addition to being able to adjust types of local variables, Flow can sometimes
also adjust types of object properties, especially when there are no intermediate
operations between a check and a use. In general, though, aliasing of objects 
limits the scope of this form of reasoning, since a check on an object property
may be invalidated by a write to that property through an alias, and 
it is difficult for a static analysis to track aliases precisely. 

In particular, don't expect a nullable field to be recognized as non-`null` in some method because a `null` check is
performed in some other method in your code, even when it is clear to you that
the `null` check is sufficient for safety at run time (say, because you know
that calls to the former method always follow calls to the latter method). On
the other hand, you can always propagate the result of a `null`-check by
explicitly passing around the non-`null` value in your code, and if you are
careful enough it should be possible to satisfy Flow without doing additional
`null` checks.

## Undefined Values

Undefined values, just like `null`, can cause issues too. Unfortunately,
undefined values are ubiquitous in JavaScript and it is hard to avoid them
without severely affecting the usability of the language. For example, arrays
can have holes for elements; object properties can be dynamically added and
removed. Flow makes a tradeoff in this case: it detects `undefined` local variables
and return values, but ignores the possibility of `undefined` resulting from object property and array element
accesses. Being stricter would force the programmer to do undefined checks (like `null` checks) on each dereference of
an array element or object property to get anything useful done.
