---
id: destructuring
title: Destructuring
layout: docs
permalink: /docs/destructuring.html
prev: arrays.html
next: objects.html
---

Flow supports the JavaScript construct of destructuring, which allows 
you to extract data from structured values. Here is a simple example:

{% highlight javascript linenos=table %}
/* @flow */
var arr = [1, '', true];
var [a, b, c] = arr;
// a: number (1), b: string (''), c : boolean (true)
{% endhighlight %}

The canonical example of destructuring is swapping:

{% highlight javascript linenos=table %}
var a = 1, b = 2;
[a, b] = [b, a];
// a = 2, b = 1
{% endhighlight %}

Flow can verify that any destructuring in your code is type-safe.

{% highlight javascript linenos=table %}
/* @flow */
var arr = [1, '', 'Hello', true];
// If you only care about some of the return values, you can skip some
// elements with , ,
var [a, b, ,c] = arr;
// a: number (1), b: string (''), c : boolean (true)
var z: number = a * c;
{% endhighlight %} 

```bbcode
/tmp/flow/f.js:2:28,31: boolean
This type is incompatible with
  /tmp/flow/f.js:5:17,21: number

Found 1 error
```

{% highlight javascript linenos=table %}
/* @flow */
var {x, y, o} = {x: '', y: 3, o: {z: false} }
// x: string, y: number, o: {z: boolean}
var z: number = o;
{% endhighlight %}

```bbcode
/tmp/flow/f.js:3:5,16: object pattern
This type is incompatible with
  /tmp/flow/f.js:5:8,13: number

Found 1 errors
```
