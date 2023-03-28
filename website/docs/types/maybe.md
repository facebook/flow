---
title: Maybe Types
slug: /types/maybe
---

You can prefix a type with `?` to make it a [union](../unions) with `null` and `void`:
`?T` is equivalent to the union `T | null | void`.

For example, `?number` is equivalent to `number | null | void`, and allows for numbers, `null`, and `undefined` as values. It's "maybe" a number.

```js flow-check
function acceptsMaybeNumber(value: ?number) {
  // ...
}

acceptsMaybeNumber(42);        // Works!
acceptsMaybeNumber();          // Works! (implicitly undefined)
acceptsMaybeNumber(undefined); // Works!
acceptsMaybeNumber(null);      // Works!
acceptsMaybeNumber("42");      // Error!
```

In the case of objects, a **missing** property is not the same thing as an explicitly `undefined` property.

```js flow-check
function acceptsMaybeProp({value}: {value: ?number}) {
  // ...
}

acceptsMaybeProp({value: undefined}); // Works!
acceptsMaybeProp({});                 // Error!
```

If you want to allow missing properties, use [optional property](../objects/#toc-optional-object-type-properties) syntax, where the `?` is placed _before_ the colon.
It is also possible to combine both syntaxes for an optional maybe type, for example `{value?: ?number}`.


## Refining maybe types {#toc-refining-maybe-types}

Imagine we have the type `?number`, if we want to use that value as a `number`
we'll need to first check that it is not `null` or `undefined`.

```js flow-check
function acceptsMaybeNumber(value: ?number): number {
  if (value !== null && value !== undefined) {
    return value * 2;
  }
  return 0;
}
```

You can simplify the two checks against `null` and `undefined` using a single
`!= null` check which will do both.

```js flow-check
function acceptsMaybeNumber(value: ?number): number {
  if (value != null) {
    return value * 2;
  }
  return 0;
}
```
Most double equal checks are discouraged in JavaScript, but the above pattern is safe (it checks for exactly `null` and `undefined`).

You could also flip it around, and check to make sure that the value has a type
of `number` before using it.

```js flow-check
function acceptsMaybeNumber(value: ?number): number {
  if (typeof value === 'number') {
    return value * 2;
  }
  return 0;
}
```

However, type refinements can be lost. For instance, calling a function after refining the type of an object's property will invalidate this refinement.
Consult the [refinement invalidations](../../lang/refinements/#toc-refinement-invalidations) docs for more details, to understand why Flow works this way,
and how you can avoid this common pitfall.
