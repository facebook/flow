---
id: union-intersection-types
title: Union and Intersection Types
permalink: /docs/union-intersection-types.html
prev: destructuring.html
next: type-aliases.html
---

Flow adds support for both union and intersection types. A union type allows 
for a value to be one of the input types.

{% highlight javascript linenos=table %}
/* @flow */
var x: number | string = 0;
{% endhighlight %}

`x` can be either a `number` or a `string`. A default value can even be 
provided of one of those two types.

{% highlight javascript linenos=table %}
/* @flow */
declare var f: ((x: number) => void) & ((x: string) => void);
f('');
{% endhighlight %}

> NOTE
> 
> Parentheses are important. Flow will not type-check correctly if you leave 
> out the outer parenthesis on each of the function declarations on `f`.


We are intersecting `function` here. A call to `f` has to be with a `number` 
or `string`. Intersections are well-suited to mimic function overloading.

> NOTE
> 
> Not all intersection types make sense. For example, no value has type 
`number & string` since there is no value that can have both of those types.


## Syntax

- Union: `<type 1> | <type 2>  ... | <type n>`
- Intersection: `<type 1> & <type 2> ... & <type n>`

## Union Example

{% highlight javascript linenos=table %}
/* @flow */
class A {}
class B {}
class C {}

var x: A | B | number | C = new C();
x = 3;
x = new B();
x = true; // Flow will error here
{% endhighlight %}

`x` is the union of `A`, `B`, `number` and `C`. So `x` can be assigned to any 
of those types. It cannot, however, be assigned a `boolean`.

```bbcode
/tmp/flow/f.js:9:5,8: boolean
This type is incompatible with
  /tmp/flow/f.js:2:7,7: A

/tmp/flow/f.js:9:5,8: boolean
This type is incompatible with
  /tmp/flow/f.js:3:7,7: B

/tmp/flow/f.js:9:5,8: boolean
This type is incompatible with
  /tmp/flow/f.js:4:7,7: C

/tmp/flow/f.js:9:5,8: boolean
This type is incompatible with
  /tmp/flow/f.js:6:16,21: number
```

## Intersection Example

{% highlight javascript linenos=table %}
/* @flow */
class Foo {}
class Bar {}
declare var f: ((x: Foo) => void) & ((x: Bar) => void);
f(new Foo());
f(true); // Flow will error here.
{% endhighlight %}

`f` is intersected on `function` that take a `Foo` or `Bar`. Trying to pass in 
a `boolean` will cause a type error.

```bbcode
/tmp/flow/f.js:6:3,6: boolean
This type is incompatible with
  /tmp/flow/f.js:2:7,9: Foo

/tmp/flow/f.js:6:3,6: boolean
This type is incompatible with
  /tmp/flow/f.js:3:7,9: Bar
```
