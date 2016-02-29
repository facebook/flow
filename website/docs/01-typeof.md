---
id: typeof
title: Typeof
permalink: /docs/typeof.html
prev: type-aliases.html
next: dynamic-type-tests.html
---

In vanilla JavaScript, the `typeof` operator returns a `string` specifying the 
data type of an expression. Here is a simple example:

{% highlight javascript linenos=table %}
/* @flow */
var index: number = 10;
var result: string = typeof index;
// result: 'number'
{% endhighlight %}

However, in Flow, `typeof` can also be used to as a mechanism to capture 
types in type annotating positions as well.

## Use of `typeof`

Take the following code example:

{% highlight javascript linenos=table %}
/* @flow */
class X {}
var a = X; // a infers its type from X
var b: typeof X; // b has the same type as X. It is the same as a
{% endhighlight %}

There is no real advantage of using `typeof` for variable typing in the above 
case.

However, imagine `X` has a `static` function:

{% highlight javascript linenos=table %}
/* @flow */
class X {
  static bar(): string {
    return 'Hi';
  }
}
var a: X = new X();
a.bar(); // Type error
var b: typeof X = X;
b.bar(); // Good
{% endhighlight %}

`typeof` allows the capturing of the class `X` itself, rather than just an 
instance of `X`. So constructs like `static` functions can be called using 
a variable that captured a class via `typeof`.

Here is the error that would occur if Flow was used to check the status of 
the code above:

```bbcode
/tmp/flow/f.js:8:1,7: call of method bar
Property not found in
  /tmp/flow/f.js:2:7,7: X
```

`typeof` is very useful for being able to work with the actual object itself, 
whether that is a class, module or some other construct.

## Another Example

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
