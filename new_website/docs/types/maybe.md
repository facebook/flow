---
title: Maybe Types
slug: /types/maybe
---

It's common for JavaScript code to introduce "optional" values so that you
have the option of leaving out the value or passing `null` instead.

Using Flow you can use Maybe types for these values. Maybe types work with any
other type by simply prefixing it with a question mark `?` such as `?number` as
a sort of modifier.

Maybe types accept the provided type as well as `null` or `undefined`. So
`?number` would mean `number`, `null`, or `undefined`.

```js flow-check
// @flow
function acceptsMaybeNumber(value: ?number) {
  // ...
}

acceptsMaybeNumber(42);        // Works!
acceptsMaybeNumber();          // Works!
acceptsMaybeNumber(undefined); // Works!
acceptsMaybeNumber(null);      // Works!
acceptsMaybeNumber("42");      // Error!
```

In the case of objects, a **missing** property is not the same thing as an explicitly `undefined` property.

```js flow-check
// @flow
function acceptsMaybeProp({ value }: { value: ?number }) {
  // ...
}

acceptsMaybeProp({ value: undefined }); // Works!
acceptsMaybeProp({});                   // Error!
```

If you want to allow missing properties, use [optional property](../objects/#toc-optional-object-type-properties) syntax, where the `?` is placed _before_ the colon. It is also possible to combine both syntaxes for an optional maybe type, for example `{ value?: ?number }`.


## Refining Maybe types {#toc-refining-maybe-types}

Imagine we have the type `?number`, if we want to use that value as a `number`
we'll need to first check that it is not `null` or `undefined`.

```js flow-check
// @flow
function acceptsMaybeNumber(value: ?number) {
  if (value !== null && value !== undefined) {
    return value * 2;
  }
}
```

You can simplify the two checks against `null` and `undefined` using a single
`!= null` check which will do both.

```js flow-check
// @flow
function acceptsMaybeNumber(value: ?number) {
  if (value != null) {
    return value * 2;
  }
}
```

You could also flip it around, and check to make sure that the value has a type
of `number` before using it.

```js flow-check
// @flow
function acceptsMaybeNumber(value: ?number) {
  if (typeof value === 'number') {
    return value * 2;
  }
}
```

However, type refinements can be lost. For instance, calling a function after refining the type of an object's property will invalidate this refinement. Consult the [Refinement Invalidations](../../lang/refinements/#toc-refinement-invalidations) docs for more details, to understand why Flow works this way, and how you can avoid this common pitfall.
