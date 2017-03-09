---
title: Property Variance and Other Upcoming Changes
short-title: Property Variance
author: samwgoldman
---

The next release of Flow, 0.34, will include a few important changes to object
types:

* property variance,
* invariant-by-default dictionary types,
* covariant-by-default method types,
* and more flexible getters and setters.

<!--truncate-->

## What is Variance?

Defining the subtype relationship between types is a core resposibility of Flow
as a type system. These relationships are determined either directly for
simple types or, for complex types, defined in terms of their parts.

Variance describes the subtyping relationship for complex types as it relates to
the subtyping relationships of their parts.

For example, Flow directly encodes the knowledge that `string` is a subtype of
`?string`. Intuitively, a `string` type contains string values while a `?string`
type contains `null`, `undefined`, and also string values, so membership in the
former naturally implies membership in the later.

The subtype relationships between two function types is not as direct. Rather,
it is derived from the subtype relationships between the functions' parameter
and return types.

Let's see how this works for two simple function types:

```js
type F1 = (x: P1) => R1;
type F2 = (x: P2) => R2;
```

Whether `F2` is a subtype of `F1` depends on the relationships between `P1` and
`P2` and `R1` and `R2`. Let's use the notation `B <: A` to mean `B` is a subtype
of `A`.

It turns out that `F2 <: F1` if `P1 <: P2` and `R2 <: R1`. Notice that the
relationship for parameters is reversed? In technical terms, we can say that
function types are "contravariant" with respect to their parameter types and
"covariant" with respect to their return types.

Let's look at an example:

```js
function f(callback: (x: string) => ?number): number {
  return callback("hi") || 0;
}
```

What kinds of functions can we pass to `f`? Based on the subtyping rule above,
then we can pass a function whose parameter type is a supertype of `string` and
whose return type is a subtype of `?number`.

```js
function g(x: ?string): number {
  return x ? x.length || 0;
}
f(g);
```

The body of `f` will only ever pass `string` values into `g`, which is safe
because `g` takes at least `string` by taking `?string`. Conversely, `g` will
only ever return `number` values to `f`, which is safe because `f` handles at
least `number` by handling `?number`.

### Input and Output

One convenient way to remember when something is covariant vs. contravariant is
to think about "input" and "output."

Parameters are in an *input* position, often called a "negative" position.
Complex types are contravariant in their input positions.

Return is an *output* position, often called a "positive" position. Complex
types are covariant in their output positions.

## Property Invariance

Just as function types are composed of parameter and return types, so too are
object types composed of property types. Thus, the subtyping relationship
between objects is derived from the subtyping relationships of their properties.

However, unlike functions which have input parameters and an output return,
object properties can be read and written. That is, properties are *both* input
and output.

Let's see how this works for two simple object types:

```js
type O1 = {p: T1};
type O2 = {p: T2};
```

As with function types, whether `O2` is a subtype of `O1` depends on the
relationship between its parts, `T1` and `T2`.

Here it turns out that `O2 <: O1` if `T2 <: T1` *and* `T1 <: T2`. In technical
terms, object types are "invariant" with respect to their property types.

Let's look at an example:

```js
function f(o: {p: ?string}): void {
  // We can read p from o
  let len: number;
  if (o.p) {
    len = o.p.length;
  } else {
    len = 0;
  }

  // We can also write into p
  o.p = null;
}
```

What kinds of objects can we pass into `f`, then? If we try to pass in an object
with a subtype property, we get an error:

```js
var o1: {p: string} = {p: ""};
f(o1);
```

```
function f(o: {p: ?string}) {}
                   ^ null. This type is incompatible with
var o1: {p: string} = {p: ""};
            ^ string
function f(o: {p: ?string}) {}
                   ^ undefined. This type is incompatible with
var o1: {p: string} = {p: ""};
            ^ string
```

Flow has correctly identified an error here. If the body of `f` writes `null`
into `o.p`, then `o1.p` would no longer have type `string`.

If we try to pass an object with a supertype property, we again get an error:

```js
var o2: {p: ?(string|number)} = {p: 0};
f(o2);
```

```
var o1: {p: ?(string|number)} = {p: ""};
                     ^ number. This type is incompatible with
function f(o: {p: ?string}) {}
                   ^ string
```

Again, Flow correctly identifies an error, because if `f` tried to read `p` from
`o`, it would find a number.

## Property Variance

So objects have to be invariant with respect to their property types because
properties can be read from and written to. But just because you *can* read and
write, doesn't mean you always do.

Consider a function that gets the length of an nullable string property:

```js
function f(o: {p: ?string}): number {
  return o.p ? o.p.length : 0;
}
```

We never write into `o.p`, so we should be able to pass in an object where the
type of property `p` is a subtype of `?string`. Until now, this wasn't possible
in Flow.

With property variance, you can explicitly annotate object properties as being
covariant and contravariant. For example, we can rewrite the above function:

```js
function f(o: {+p: ?string}): number {
  return o.p ? o.p.length : 0;
}

var o: {p: string} = {p: ""};
f(o); // no type error!
```

It's crucial that covariant properties only ever appear in output positions. It
is an error to write to a covariant property:

```js
function f(o: {+p: ?string}) {
  o.p = null;
}
```

```
o.p = null;
^ object type. Covariant property `p` incompatible with contravariant use in
o.p = null;
^ assignment of property `p`
```

Conversely, if a function only ever writes to a property, we can annotate the
property as contravariant. This might come up in a function that initializes an
object with default values, for example.

```js
function g(o: {-p: string}): void {
  o.p = "default";
}
var o: {p: ?string} = {p: null};
g(o);
```

Contravariant properties can only ever appear in input positions. It is an
error to read from a contravariant property:

```js
function f(o: {-p: string}) {
  o.p.length;
}
```

```
o.p.length;
^ object type. Contravariant property `p` incompatible with covariant use in
o.p.length;
^ property `p`
```

## Invariant-by-default Dictionary Types

The object type `{[key: string]: ?number}` describes an object that can be used
as a map. We can read any property and Flow will infer the result type as
`?number`. We can also write `null` or `undefined` or `number` into any
property.

In Flow 0.33 and earlier, these dictionary types were treated covariantly by the
type system. For example, Flow accepted the following code:

```js
function f(o: {[key: string]: ?number}) {
  o.p = null;
}
declare var o: {p: number};
f(o);
```

This is unsound because `f` can overwrite property `p` with `null`. In Flow
0.34, dictionaries are invariant, like named properties. The same code now
results in the following type error:

```
function f(o: {[key: string]: ?number}) {}
                               ^ null. This type is incompatible with
declare var o: {p: number};
                   ^ number
function f(o: {[key: string]: ?number}) {}
                               ^ undefined. This type is incompatible with
declare var o: {p: number};
                   ^ number
```

Covariant and contravariant dictionaries can be incredibly useful, though. To
support this, the same syntax used to support variance for named properties can
be used for dictionaries as well.

```
function f(o: {+[key: string]: ?number}) {}
declare var o: {p: number};
f(o); // no type error!
```

## Covariant-by-default Method Types

ES6 gave us a shorthand way to write object properties which are functions.

```js
var o = {
  m(x) {
    return x * 2
  }
}
```

Flow now interprets properties which use this shorthand method syntax as
covariant by default. This means it is an error to write to the property `m`.

If you don't want covariance, you can use the long form syntax:

```js
var o = {
  m: function(x) {
    return x * 2;
  }
}
```

## More Flexible Getters and Setters

In Flow 0.33 and earlier, getters and setters had to agree exactly on their
return type and parameter type, respectively. Flow 0.34 lifts that restriction.

This means you can write code like the following:

```js
/* @flow */

declare var x: string;

var o = {
  get x(): string {
    return x;
  },
  set x(value: ?string) {
    x = value || "default";
  }
}
```
