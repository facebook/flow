---
id: type-aliases
title: Type Aliases
layout: docs
permalink: /docs/type-aliases.html
prev: classes.html
next: nullable-types.html
---

TODO 

{% highlight javascript linenos=table %}
/* @flow */
type T = number
var x:T = 0;
  
type F<U,V> = (x:U) => V
function foo<X,Y>(f: F<X,Y>, x:X): Y { return f(); }
var result: number = foo (function(x) { return x }, 0)
{% endhighlight %}
