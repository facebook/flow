---
id: arrays
title: Typeof
layout: docs
permalink: /docs/typeof.html
prev: variables.html
next: classes.html
---

{% highlight javascript linenos=table %}
/* @flow */
class _C { }
var M: { C : typeof _C } = { C: _C };
new M.C();
{% endhighlight %}

