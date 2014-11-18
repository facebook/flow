---
id: union-intersection-types
title: Union and Intersection Types
layout: docs
permalink: /docs/union-intersection-types.html
prev: classes.html
next: nullable-types.html
---

{% highlight javascript linenos=table %}
/* @flow */
var x: number | string = 0;

declare var f: (x:number) => void & (x:string) => void;
f("");
{% endhighlight %}
