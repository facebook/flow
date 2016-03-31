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
class Bar { }
// b ends up being a Foo type, since f evaluates to Foo
var b: { f : typeof Foo } = { f : Foo };
// Since the type of b.f is typeof Foo (i.e. Class<Foo>), the following 
// assignment is valid because the type of the new instance is Foo:
var inst1: Foo = new b.f();
// However, this fails because the type of the new instance is not Bar:
var inst2: Bar = new b.f();
{% endhighlight %}


```bbcode
tmp/flow/f.js:10
 10: var inst2: Bar = new b.f();
                      ^^^^^^^^^ Foo. This type is incompatible with
 10: var inst2: Bar = new b.f();
                ^^^ Bar


Found 1 error
```
