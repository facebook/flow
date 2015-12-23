---
id: destructuring
title: Destructuring
layout: docs
permalink: /docs/destructuring.html
prev: nullable-types.html
next: union-intersection-types.html
---

Flow supports the JavaScript construct of destructuring, which allows 
you to extract data from structured values. Here is a simple example:

{% highlight javascript linenos=table %}
/* @flow */
const arr = [1, '', true];
const [a, b, c] = arr;
// a: number (1), b: string (''), c : boolean (true)
{% endhighlight %}

The canonical example of destructuring is swapping:

{% highlight javascript linenos=table %}
let a = 1, b = 2;
[a, b] = [b, a];
// a = 2, b = 1
{% endhighlight %}

## Destructuring and Type Checks

Flow can verify that any destructuring in your code is type-safe.

{% highlight javascript linenos=table %}
/* @flow */
const arr = [1, '', 'Hello', true];
// If you only care about some of the return values, you can skip some
// elements with , ,
const [a, b, ,c] = arr;
// a: number (1), b: string (''), c : boolean (true)
const z: number = a * c;
{% endhighlight %} 

Above we have a four (4) element array `arr` (actually a 
[`tuple`](http://flowtype.org/docs/arrays.html#tuples))
. And then we destructure that array into three (3) variables, `a`, `b`,  `c`. 
However, we then try to multiply `a` (a `number`), and `c` (a `boolean`). Flow 
catches this.

```bbcode
/tmp/flow/f.js:2:28,31: boolean
This type is incompatible with
  /tmp/flow/f.js:5:17,21: number

Found 1 error
```

## Another Example 

{% highlight javascript linenos=table %}
/* @flow */
const {x, y, ...o} = {x: '', y: 3, o: {z: false} }
// x: string, y: number, o: {z: boolean}
const z: number = o;
{% endhighlight %}

```bbcode
/tmp/flow/f.js:3:5,16: object pattern
This type is incompatible with
  /tmp/flow/f.js:5:8,13: number

Found 1 errors
```

`o` has been destructed as an object that contains a boolean value. That 
cannot be assigned to a number.
