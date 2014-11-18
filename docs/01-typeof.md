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
class Foo { }
// b ends up being a Foo type, since f evaluates to Foo
var b: { f : typeof Foo } = { f : Foo };
// Since f is Foo, and b is of a Foo type, we can instantiate b as Foo 
new b.f();
{% endhighlight %}

Let's see an example where we use `typeof` and Flow will catch a typing error:

{% highlight javascript linenos=table %}
/* @flow */
class Foo { }
class Bar { }
var b: { f : typeof Foo } = { f : Foo };
var c: { g : typeof Bar } = { g : Bar };
// b is of type Foo, not of type Bar (g is a Bar )
new b.g();
{% endhighlight %}

```bbcode
/tmp/flow/f.js:6:5,7: property g
Property not found in
  /tmp/flow/f.js:6:5,5: object type

Found 1 error
```
