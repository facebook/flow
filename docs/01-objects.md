---
id: objects
title: Objects
layout: docs
permalink: /docs/objects.html
prev: classes.html
next: functions.html
---

Objects can be created with object literals. The types of properties are fixed
based on their initializers.

{% highlight javascript linenos=table %}
/* @flow */
var o = {
  x: 42,
  foo(x) { this.x = x; }
};
o.foo('hello');
{% endhighlight %}

Flow infers the type of property `x` of the object to be number since it is
initialized with a `number`. The method call `foo()` on the object writes
`string` to that property. As expected, running Flow produces the following
error:

```bbcode
file.js:6:7,13: string
This type is incompatible with
  file.js:3:6,7: number
```

## Object Types

Object types are of the form:

{% highlight javascript linenos=table %}
{ x1: T1; x2: T2; ... x3: T3;}
{% endhighlight %}

Here is an example of declaring an object type:

{% highlight javascript linenos=table %}
/* @flow */
class Foo {}
var obj: {a: boolean; b: string; c: Foo} = {a: true, b: "Hi", c: new Foo()}
{% endhighlight %}

Here is an example of Flow catching a problem with your object type:

{% highlight javascript linenos=table %}
/* @flow */
class Foo {}
class Bar {}
var obj: {a: boolean; b: string; c: Foo} = {a: true, b: "Hi", c: new Bar()}
{% endhighlight %}

```bbcode
/tmp/flow/f.js:4:70,72: Bar
This type is incompatible with
  /tmp/flow/f.js:2:7,9: Foo
```

## Reusable Object Types

Object types can be made reusable through the use of
[type aliases](type-aliases.html):

{% highlight javascript linenos=table %}
/* @flow */
type MyType = {message: string; isAwesome: boolean};
function sayHello(data: MyType) {
  console.log(data.message);
}

var mySampleData: MyType = {message: 'Hello World', isAwesome: true};
sayHello(mySampleData);
sayHello({message: 'Hi', isAwesome: false});
{% endhighlight %}

## Constructor Functions and Prototype Objects

Another way of creating objects in JavaScript is by using `new` on
constructor functions. A constructor function is typically an open method
that "initializes" some properties of `this`; and a `new` operation on such a
function calls it on a freshly created object before returning it.

Additionally, a constructor function may set various properties on its
`prototype` object. These properties are typically methods, and are inherited
by all objects created from that constructor function by a process known as
prototype chaining.

{% highlight javascript linenos=table %}
/* @flow */
function Foo(x) { this.x = x; }
Foo.prototype.f = function() { return this.x; }

var o = new Foo(42);
var x: number = o.f();
{% endhighlight %}

In this code, a `new` object is created by `new Foo(42)`; this object has a
property `x` initialized by `Foo` with the `number` passed to it. The object
also responds to the `f` method defined in `Foo.prototype`, so `o.f()` reads
`o.x` and returns it. This fits with the expectation of a `number` as
expressed by the annotation at line 7, so this code typechecks.

Furthermore, Flow ensures that an object's type can always be viewed as a
subtype of its constructor's `prototype type`. (This is analogous to subtyping
based on class inheritance.) This means that the following code typechecks:

{% highlight javascript linenos=table %}
...
var o: Foo = new Foo(42);
{% endhighlight %}

## Adding properties

It is a common idiom in JavaScript to add properties to objects after they are
created. In fact, we have already seen this idiom on several occasions above:
when initializing properties of `this` properties in a constructor function;
when building a constructor function's `prototype` object; when building a
`module.exports` object; and so on.

Flow supports this idiom. As far as we know, this is a type system novelty:
supporting this idiom while balancing other constraints of the type system,
such as sound subtyping over objects and prototypes, can be quite tricky!

However, for a property that may be added to an object after its creation,
Flow cannot guarantee the existence of that property at a particular property
access operation; it can only check that its writes and reads are type-
consistent. Providing such guarantees for dynamic objects would significantly
complicate the analysis; this is a well-known fact (in technical terms, Flow's
analysis is heap-insensitive for strong updates).

For example, the following code typechecks:

{% highlight javascript linenos=table %}
/* @flow */
function foo(p) { p.x = 42; }
function bar(q) { q.f(); }

var o = { f() { return this.x; } };

bar(o);
foo(o);
{% endhighlight %}

In this code, when `bar(o)` is called, `o.x` is undefined; only later is it
initialized by `foo(o)`, but it is hard to track this fact statically.

Fortunately, though, the following code does not typecheck:

{% highlight javascript linenos=table %}
/* @flow */
function foo(p) { p.x = 42; }
function bar(q) { q.f(); }

var o = { f() { return this.x; } };

foo(o);
var x: string = bar(o);
{% endhighlight %}

```bbcode
file.js:3:16,16: undefined
This type is incompatible with
  file.js:8:7,12: string
```

In other words, Flow knows enough to infer that whenever the `x` property of
`o` does exist, it is a number, so a `string` should not be expected.

### Cautious Flexibility

Overall, the weaker guarantee for dynamically added properties is a small cost
to pay for the huge increase in flexibility it affords. Specifically, it
allows Flow to usefully type check lots of idiomatic JavaScript code, while
trusting the programmer to follow the discipline of fully initializing an
object before making it available, which effectively ensures that any
dynamically added properties during initialization are only accessed after
initialization is complete.

In any case, for most objects you can altogether avoid adding properties
dynamically, in which case you get stronger guarantees. Furthermore, as
described below, object type annotations are sealed, so you can always force
sealing by going through an annotation (and sealing is enforced at module
boundaries).

## Objects as Maps

An object can be viewed as a map from `string` to some value type by setting
and getting its properties via bracket notation (i.e. dynamic accessors),
instead of dot notation. Flow infers a precise value type for the map: in
other words, if you only write `number` values to a map, you will read `number`values back (rather than, say, `any`).

Such a map can be given a type of the form `{ [key:string]: number }` where `string` is the key type and `number` is the
value type of the map.

### Maps as Records

Viewing an object as a map does not preclude viewing it as a record. However,
for such an object, the value type of the map does not interfere with the
types of the properties of the record. This is a potentially unsound, but we
admit it because a sound design would necessarily lead to severe imprecision
in the types of properties.

