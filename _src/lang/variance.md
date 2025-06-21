---
title: Type Variance
slug: /lang/variance
---

Variance is a topic that comes up fairly often in type systems. It is used to determine
how type parameters behave with respect to subtyping.

First we'll setup a couple of classes that extend one another.

```js
class Noun {}
class City extends Noun {}
class SanFrancisco extends City {}
```

We saw in the section on [generic types](../../types/generics/#toc-variance-sigils)
that it is possible to
use variance sigils to describe when a type parameter is used in an output position,
when it is used in an input position, and when it is used in either one.

Here we'll dive deeper into each one of these cases.

## Covariance {#toc-covariance}

Consider for example the type
```js
type CovariantOf<X> = {
  +prop: X;
  getter(): X;
}
```
Here, `X` appears strictly in *output* positions: it is used to read out information
from objects `o` of type `CovariantOf<X>`, either through property accesses `o.prop`,
or through calls to `o.getter()`.

Notably, there is no way to input data through the reference to the object `o`,
given that `prop` is a readonly property.

When these conditions hold, we can use the sigil `+` to annotate `X` in the definition
of `CovariantOf`:
```js
type CovariantOf<+X> = {
  +prop: X;
  getter(): X;
}
```

These conditions have important implications on the way that we can treat an object
of type `CovariantOf<T>` with respect to subtyping. As a reminder, subtyping rules
help us answer the question: "given some context that expects values of type
`T`, is it safe to pass in values of type `S`?" If this is the case, then `S` is a
subtype of `T`.

Using our `CovariantOf` definition, and given that `City` is a subtype of `Noun`, it is
also the case that `CovariantOf<City>` is a subtype of `CovariantOf<Noun>`. Indeed
* it is safe to *read* a property `prop` of type `City` when a property
of type `Noun` is expected, and
* it is safe to *return* values of type `City` when calling `getter()`, when
values of type `Noun` are expected.

Combining these two, it will always be safe to use `CovariantOf<City>` whenever a
`CovariantOf<Noun>` is expected.

A commonly used example where covariance is used is [`$ReadOnlyArray<T>`](../../types/arrays/#toc-readonlyarray).
Just like with the `prop` property, one cannot use a `$ReadOnlyArray` reference to write data
to an array. This allows more flexible subtyping rules: Flow only needs to prove that
`S` is a subtype of `T` to determine that `$ReadOnlyArray<S>` is also a subtype
of `$ReadOnlyArray<T>`.


## Invariance {#toc-invariance}

Let's see what happens if we try to relax the restrictions on the use of `X` and make,
for example, `prop` be a read-write property. We arrive at the type definition
```js
type NonCovariantOf<X> = {
  prop: X;
  getter(): X;
};
```
Let's also declare a variable `nonCovariantCity` of type `NonCovariantOf<City>`
```js
declare const nonCovariantCity: NonCovariantOf<City>;
```
Now, it is not safe to consider `nonCovariantCity` as an object of type `NonCovariantOf<Noun>`.
Were we allowed to do this, we could have the following declaration:
```js
const nonCovariantNoun: NonCovariantOf<Noun> = nonCovariantCity;
```
This type permits the following assignment:
```js
nonCovariantNoun.prop = new Noun;
```
which would invalidate the original type for `nonCovariantCity` as it would now be storing
a `Noun` in its `prop` field.


What distinguishes `NonCovariantOf` from the `CovariantOf` definition is that type parameter `X` is used both
in input and output positions, as it is being used to both read and write to
property `prop`. Such a type parameter is called *invariant* and is the default case
of variance, thus requiring no prepending sigil:
```js
type InvariantOf<X> = {
  prop: X;
  getter(): X;
  setter(X): void;
};
```
Assuming a variable
```js
declare const invariantCity: InvariantOf<City>;
```
it is *not* safe to use `invariantCity` in a context where:
- an `InvariantOf<Noun>` is needed, because we should not be able to write a `Noun` to property
`prop`.
- an `InvariantOf<SanFrancisco>` is needed, because reading `prop` could return a `City` which
may not be `SanFrancisco`.

In orther words, `InvariantOf<City>` is neither a subtype of `InvariantOf<Noun>` nor
a subtype of `InvariantOf<SanFrancisco>`.


## Contravariance {#toc-contravariance}

When a type parameter is only used in *input* positions, we say that it is used in
a *contravariant* way. This means that it only appears in positions through which
we write data to the structure. We use the sigil `-` to describe this kind of type
parameters:

```js
type ContravariantOf<-X> = {
  -prop: X;
  setter(X): void;
};
```
Common contravariant positions are write-only properties and "setter" functions.

An object of type `ContravariantOf<City>` can be used whenever an object of type
`ContravariantOf<SanFrancisco>` is expected, but not when a `ContravariantOf<Noun>` is.
In other words, `ContravariantOf<City>` is a subtype of `ContravariantOf<SanFrancisco>`, but not
`ContravariantOf<Noun>`.
This is because it is fine to write `SanFrancisco` into a property that can have any `City` written
to, but it is not safe to write just any `Noun`.
