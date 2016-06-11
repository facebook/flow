---
id: union-intersection-types
title: Union and Intersection Types
permalink: /docs/union-intersection-types.html
prev: type-aliases.html
next: typeof.html
---

Flow adds support for both union and intersection types. A union type requires
a value to be one of the input types:

{% highlight javascript linenos=table %}
/* @flow */
type U = number | string
var x: U = 1;
x = "two";
{% endhighlight %}

An intersection type requires a value to be all of the input types:

{% highlight javascript linenos=table %}
/* @flow */
type I = {a: number} & {b: number}
var x: I = {a: 1, b: 2};
x = {a: 1, b: 2, c: "three"};
{% endhighlight %}

The value `{a: 1, b: 2, c: "three"}` is admissible here because the
`Intersection` type only constrains properties `a` and `b`.

> NOTE
>
> Intersection typing interacts with function types to yield union types over
> the parameters, e.g. `((x: A) => T) & ((x: B) => U)` is equivalent to
> `(x: A | B) => (T & U)`.


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
