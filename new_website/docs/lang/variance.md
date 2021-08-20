---
title: Type Variance
slug: /lang/variance
---

Variance is a topic that comes up fairly often in type systems and can be a bit
confusing the first time you hear it. Let's walk through each form of variance.

First we'll setup a couple of classes that extend one another.

```js
class Noun {}
class City extends Noun {}
class SanFrancisco extends City {}
```

We'll use these classes to write a method that has each kind of variance.

> **Note:** The `*variantOf` types below are not a part of Flow, they are being
> used to explain variance.

---

## Invariance {#toc-invariance}

```js
function method(value: InvariantOf<City>) {...}

method(new Noun());         // error...
method(new City());         // okay
method(new SanFrancisco()); // error...
```

- Invariance _does not_ accept **supertypes**.
- Invariance _does not_ accept **subtypes**.

## Covariance {#toc-covariance}

```js
function method(value: CovariantOf<City>) {...}

method(new Noun());         // error...
method(new City());         // okay
method(new SanFrancisco()); // okay
```

- Covariance _does not_ accept **supertypes**.
- Covariance _does_ accept **subtypes**.

## Contravariance {#toc-contravariance}

```js
function method(value: ContravariantOf<City>) {...}

method(new Noun());         // okay
method(new City());         // okay
method(new SanFrancisco()); // error...
```

- Contravariance _does_ accept **supertypes**.
- Contravariance _does not_ accept **subtypes**.

## Bivariance {#toc-bivariance}

```js
function method(value: BivariantOf<City>) {...}
method(new Noun());         // okay
method(new City());         // okay
method(new SanFrancisco()); // okay
```

- Bivariance _does_ accept **supertypes**.
- Bivariance _does_ accept **subtypes**.

---

As a result of having weak dynamic typing, JavaScript doesn't have any of
these, you can use any type at any time.

## Variance in Classes {#toc-variance-in-classes}

To get a sense of when and why the different kinds of variance matters, let's
talk about methods of subclasses and how they get type checked.

We'll quickly set up our `BaseClass` which will define just one method that
accepts an input value with the type `City` and a returned output also with
the type `City`.

```js
class BaseClass {
  method(value: City): City { ... }
}
```

Now, let's walk through different definitions of `method()` in a couple
different _subclasses_.

---

### Equally specific inputs and outputs — Good {#toc-equally-specific-inputs-and-outputs-good}

To start, we can define a SubClass that extends our BaseClass. Here you can see
that the value and the return type are both City just like in BaseClass:

```js
class SubClass extends BaseClass {
  method(value: City): City { ... }
}
```

This is okay because if something else in your program is using `SubClass` as
if it were a `BaseClass`, it would still be using a `City` and wouldn't cause
any issues.

### More specific outputs — Good {#toc-more-specific-outputs-good}

Next, we'll have a different `SubClass` that returns a more specific type:

```js
class SubClass extends BaseClass {
  method(value: City): SanFrancisco { ... }
}
```

This is also okay because if something is using `SubClass` as if it were a
`BaseClass` they would still have access to the same interface as before
because `SanFrancisco` is just a `City` with a little more information.

### Less specific outputs — Bad {#toc-less-specific-outputs-bad}

Next, we'll have a different `SubClass` that returns a less specific type:

```js
class SubClass extends BaseClass {
  method(value: City): Noun { ... } // ERROR!!
}
```

In Flow this will cause an error because if you are expecting to get a return
value of a `City`, you may be using something that doesn't exist on `Noun`,
which could easily cause an error at runtime.

### Less specific inputs — Good {#toc-less-specific-inputs-good}

Next, we'll have another SubClass that accepts a value of a less specific type.

```js
class SubClass extends BaseClass {
  method(value: Noun): City { ... }
}
```

This is perfectly fine because if we pass in a more specific type we'll still
have all the information we need to be compatible with `Noun`.

### More specific inputs — Bad {#toc-more-specific-inputs-bad}

Finally, we'll have yet another `SubClass` that accepts a value of a more
specific type.

```js
class SubClass extends BaseClass {
  method(value: SanFrancisco): City { ... } // ERROR!!
}
```

This is an error in Flow because if you are expecting a `SanFrancisco` and you
get a `City` you could be using something that only exists on `SanFrancisco`
which would cause an error at runtime.

---

All of this is why Flow has contravariant inputs (accepts less specific types
to be passed in), and covariant outputs (allows more specific types to be
returned).
