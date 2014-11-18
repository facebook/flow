---
id: declarations
title: Declarations
layout: docs
permalink: /docs/declarations.html
prev: classes.html
next: nullable-types.html
---

{% highlight javascript linenos=table %}
declare class C {
  x: string;
}
declare module M {
  declare function foo(c: C):void;
}
{% endhighlight %}

{% highlight javascript linenos=table %}
var M = require('M');
M.foo(new C());
{% endhighlight %}
