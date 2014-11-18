---
id: destructuring
title: Destructuring
layout: docs
permalink: /docs/destructuring.html
prev: arrays.html
next: objects.html
---

{% highlight javascript linenos=table %}
/* @flow */
var [a,b,] = [1,"",true]
// a: number, b: string
{% endhighlight %}

{% highlight javascript linenos=table %}
/* @flow */
var {x, y, ...o} = {x: string, y: number, z: boolean }
// x: string, y: number, o: { z: boolean }
{% endhighlight %}
