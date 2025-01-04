---
title: Type Refinements
slug: /lang/refinements
---

Refinements allow us to narrow the type of a value based on conditional tests.

For example, in the function below `value` is a [union](../../types/unions) of `"A"` or `"B"`.

```js flow-check
function func(value: "A" | "B") {
  if (value === "A") {
    value as "A";
  }
}
```

Inside of the `if` block we know that value must be `"A"` because that's the only
time the if-statement will be true.

The ability for a static type checker to be able to tell that the value inside
the if statement must be `"A"` is known as a refinement.

Next we'll add an `else` block to our if statement.

```js flow-check
function func(value: "A" | "B") {
  if (value === "A") {
    value as "A";
  } else {
    value as "B";
  }
}
```

Inside of the `else` block we know that value must be `"B"` because it can only
be `"A"` or `"B"` and we've removed `"A"` from the possibilities.

## Ways to refine in Flow

### `typeof` checks
You can use a `typeof value === "<type>"` check to refine a value to one of the categories supported by the [`typeof`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof) operator.

The `typeof` operator can output `"undefined"`,`"boolean"`, `"number"`, `"bigint"`, `"string"`, `"symbol"`, `"function"`, or `"object"`.

Keep in mind that the `typeof` operator will return `"object"` for objects, but also `null` and arrays as well.

```js flow-check
function func(value: mixed) {
  if (typeof value === "string") {
    value as string;
  } else if (typeof value === "boolean") {
    value as boolean;
  } else if (typeof value === "object") {
    // `value` could be null, an array, or an object
    value as null | interface {} | $ReadOnlyArray<mixed>;
  }
}
```

To check for `null`, use a `value === null` [equality](#equality-checks) check.

```js flow-check
function func(value: mixed) {
  if (value === null) {
    value as null; // `value` is null
  }
}
```

To check for [arrays](../../types/arrays), use `Array.isArray`:

```js flow-check
function func(value: mixed) {
  if (Array.isArray(value)) {
    value as $ReadOnlyArray<mixed>; // `value` is an array
  }
}
```

### Equality checks

As shown in the introductory example, you can use an equality check to narrow a value to a specific type.
This also applies to equality checks made in `switch` statements.

```js flow-check
function func(value: "A" | "B" | "C") {
  if (value === "A") {
    value as "A";
  } else {
    value as "B" | "C";
  }

  switch (value) {
    case "A":
      value as "A";
      break;
    case "B":
      value as "B";
      break;
    case "C":
      value as "C";
      break;
  }
}
```

While in general it is not recommended to use `==` in JavaScript, due to the coercions it performs,
doing `value == null` (or `value != null`) checks `value` exactly for `null` and `void`.
This works well with Flow's [maybe](../../types/maybe) types, which create a union with `null` and `void`.

```js flow-check
function func(value: ?string) {
  if (value != null) {
    value as string;
  } else {
    value as null | void;
  }
}
```

You can refine a union of object types based on a common tag, which we call [disjoint object unions](../../types/unions/#toc-disjoint-object-unions):

```js flow-check
type A = {type: "A", s: string};
type B = {type: "B", n: number};

function func(value: A | B) {
  if (value.type === "A") {
    // `value` is A
    value.s as string; // Works
  } else {
    // `value` is B
    value.n as number; // Works
  }
}
```

### Truthiness checks

You can use non-booleans in JavaScript conditionals.
`0`, `NaN`, `""`, `null`, and `undefined` will all coerce to `false` (and so are considered "falsey").
Other values will coerce to `true` (and so are considered "truthy").

```js flow-check
function func(value: ?string) {
  if (value) {
    value as string; // Works
  } else {
    value as null | void; // Error! Could still be the empty string ""
  }
}
```

You can see in the above example why doing a truthy check when your value can be a string or number is not suggested:
it is possible to unintentionally check against the `""` or `0`.
We created a [Flow lint](../../linting) called [sketchy-null](../../linting/rule-reference/#toc-sketchy-null) to guard against this scenario:

```js flow-check
// flowlint sketchy-null:error
function func(value: ?string) {
  if (value) { // Error!
  }
}
```

### `in` checks

You can use the [in](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in)
operator to check if a property exists on an object (either in its own properties, or up the prototype chain).
This can be used to refine a union of objects:

```js flow-check
function func(obj: {foo: string, value: boolean} | {bar: string, value: number}) {
  if ('foo' in obj) {
    obj.value as boolean; // Works!
  } else {
    obj.value as number; // Works!
  }
}
```

This works best on unions of [exact objects](../../types/objects/#exact-and-inexact-object-types), since in the negation we know the property does not exist.
We cannot say the same for [inexact objects](../../types/objects/#exact-and-inexact-object-types), [interfaces](../../types/interfaces/), and [instance types](../../types/classes/),
since they may have other unknown properties, including the one we are checking.
Additionally, [optional properties](../../types/objects/#toc-optional-object-type-properties) may or may not exist, so are not particularly useful to check against.

If you want to refine a union of [tuple types](../../types/tuples/) based on whether an element exists,
check the [length](../../types/tuples/#length-refinement) property instead of attempting to use `in`.

### `instanceof` checks

You can use the [instanceof](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof) operator to narrow a value as well.
It checks if the supplied constructor's prototype is anywhere in a value's prototype chain.

```js flow-check
class A {
  amaze(): void {}
}
class B extends A {
  build(): void {}
}

function func(value: mixed) {
  if (value instanceof B) {
    value.amaze(); // Works
    value.build(); // Works
  }

  if (value instanceof A) {
    value.amaze(); // Works
    value.build(); // Error
  }

  if (value instanceof Object) {
    value.toString(); // Works
  }
}
```

### Assignments

Flow follows your control flow and narrows the type of a variable after you have assigned to it.

```js flow-check
declare const b: boolean;

let x: ?string = b ? "str" : null;

x as ?string;

x = "hi";

// We know `x` must now be a string after the assignment
x as string; // Works
```

### Type Guards

You can create a reusable refinement by defining a function which is a [type guard](../../types/type-guards/).

```js flow-check
function nonMaybe<T>(x: ?T): x is T {
  return x != null;
}

function func(value: ?string) {
  if (nonMaybe(value)) {
    value as string; // Works!
  }
}
```

## Refinement Invalidations {#toc-refinement-invalidations}

It is also possible to invalidate refinements, for example:

```js flow-check
function otherFunc() { /* ... */ }

function func(value: {prop?: string}) {
  if (value.prop) {
    otherFunc();
    value.prop.charAt(0); // Error!
  }
}
```

The reason for this is that we don't know that `otherFunc()` hasn't done
something to our value. Imagine the following scenario:

```js flow-check
const obj: {prop?: string} = {prop: "test"};

function otherFunc() {
  if (Math.random() > 0.5) {
    delete obj.prop;
  }
}

function func(value: {prop?: string}) {
  if (value.prop) {
    otherFunc();
    value.prop.charAt(0); // Error!
  }
}

func(obj);
```

Inside of `otherFunc()` we sometimes remove `prop`. Flow doesn't know if the
`if (value.prop)` check is still true, so it invalidates the refinement.

There's a straightforward way to get around this. Store the value before
calling another function and use the stored value instead. This way you can
prevent the refinement from invalidating.

```js flow-check
function otherFunc() { /* ... */ }

function func(value: {prop?: string}) {
  if (value.prop) {
    const prop = value.prop;
    otherFunc();
    prop.charAt(0);
  }
}
```
