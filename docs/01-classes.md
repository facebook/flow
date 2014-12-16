---
id: classes
title: Classes
layout: docs
permalink: /docs/classes.html
prev: arrays.html
next: objects.html
---

Classes were introduced in JavaScript (ES6) to formalize the common practice of simulating class-like inheritance hierarchies in JavaScript with functions and prototypes.

## Type Annotating Classes

A class may be defined using ES6 syntax extended with field type declarations.

{% highlight javascript linenos=table %}
/* @flow */
class C {
  x: string;
  y: number;
  constructor(x) { this.x = x; }
  foo(y) { this.y = y; }
  bar() { return this.x; }
}

class D extends C {
  bar() {
    this.x = super.bar() + 1;
    return this.x;
  }

  static qux() { return new D(); }
}
{% endhighlight %}

In the code above, `C` has two fields `x` typed string and `y` typed number, a constructor, and a few methods; `D` overrides one of those methods, and also has a static method.

Just like other languages with classes, Flow enforces that the type of an
overridden method in a superclass (e.g., `bar` in `C`) matches the type of an
overriding method (e.g., `bar` in `D`). This ensures that subclassing implies
subtyping, i.e., the following code type checks:

{% highlight javascript linenos=table %}
...
var c: C = new D();
{% endhighlight %}

## Propagation

Types are propagated through method calls and field accesses. For example,
based on the example above, the following does not typecheck:


{% highlight javascript linenos=table %}
...
var c: C = D.qux();
c.foo(0);
var x: string = c.bar();
{% endhighlight %}

```bbcode
file.js:3:6,11: number
This type is incompatible with
  file.js:21:8,13: string
```

`c.bar()` returns a `number`, which cannot be stored into a `string`.


## Exported Classes

Classes are seldom defined to be used locally. When a class is exported, the
user must also annotate its fields, along with the parameters and returns of
its methods with types.

Taking the above example:

{% highlight javascript linenos=table %}
/* @flow */
class C {
  x: number;
  foo(x: number): void { this.x = x; }
  bar(): number { return this.x; }
}

class D extends C {
  bar(): number {
    this.x = super.bar() + 1;
    return this.x;
  }

  static qux(): D { return new D(); }
}
module.exports.C = C;
module.exports.D = D;
{% endhighlight %}

## Polymorphic classes

Class definitions can be polymorphic, meaning that they can represent a family
of classes of the same "shape" but differing only in the instantiation of its
type parameters.

Consider a polymorphic version of the class above:

{% highlight javascript linenos=table %}
/* @flow */
class C<X> {
  x: X;
  foo(x: X) { this.x = x; }
  bar(): X { return this.x; }
}

class D extends C<number> {
  bar(): number {
    this.x = super.bar() + 1;
    return this.x;
  }
  static qux(): D { return new D(); }
}

module.exports.C = C;
module.exports.D = D;
{% endhighlight %}

The class `C` is polymorphic in the type parameter `X`. Flow checks that the
type signatures for its methods `foo()` and `bar()`, which refer to `X`, are
correct for any instantiation of `X`. Thus, when class `D` extends `C<number>`
, Flow can conclude that the latter has a method with signature `bar(): number`
, and (as usual) check that it matches the type of `bar` in `D`.

### Polymorphism and Static Properties

Polymorphism does not apply to static properties. This is because,
even though there may be multiple instances of a class whose types may differ
in their instantiations of the type parameter, there is only one class
definition, and thus only one set of static properties, at run time.
Polymorphic classes are invariant in their type parameters, which means that
an expression of type `C<T>` may flow to a location typed `C<U>` only when `T`
and `U` are subtypes of each other.
