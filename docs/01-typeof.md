---
id: arrays
title: Typeof
layout: docs
permalink: /docs/typeof.html
prev: variables.html
next: classes.html
---

Flow supports the `typeof` operator, which returns a `string` specifying the 
data type of an expression. Here is a simple example:

{% highlight javascript linenos=table %}
/* @flow */
var index: number = 10;
var result: string = typeof index;
// result: 'number'
{% endhighlight %}

You can use the `typeof` operator on more complicated expressions:

{% highlight javascript linenos=table %}
/* @flow */
class _C { }
// Create a typed instance of C via typeof and assignment
var M: { C : typeof _C } = { C: _C };
// This works
new M.C();
{% endhighlight %}

Let's see an example where we use `typeof` and Flow will catch a typing error:

{% highlight javascript linenos=table %}
/* @flow */
class _C { }
class _D { }
// Create a typed instance of C via typeof and assignment
var M: { C : typeof _C } = { C: _C };
// This works
new M.D();
{% endhighlight %}

```bbcode
/tmp/flow/f.js:7:5,7: property D
Property not found in
  /tmp/flow/f.js:7:5,5: object type

Found 1 errors
```
