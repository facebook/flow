---
title: Type Refinements
slug: /lang/refinements
---

Refinements are a frequently used aspect of many type systems. They are so
ingrained in the way that we program and even the way that we think you might
not even notice them.

In the code below, value can either be `"A"` or `"B"`.

```js flow-check
// @flow
function method(value: "A" | "B") {
  if (value === "A") {
    // value is "A"
  }
}
```

Inside of the if block we know that value must be `"A"` because that's the only
time the if-statement will be truthy.

The ability for a static type checker to be able to tell that the value inside
the if statement must be `"A"` is known as a refinement.

Next we'll add an else block to our if statement.

```js flow-check
// @flow
function method(value: "A" | "B") {
  if (value === "A") {
    // value is "A"
  } else {
    // value is "B"
  }
}
```

Inside of the else block we know that value must be `"B"` because it can only
be `"A"` or `"B"` and we've removed `"A"` from the possibilities.

You can expand this even further and keep refining possibilities away:

```js flow-check
// @flow
function method(value: "A" | "B" | "C" | "D") {
  if (value === "A") {
    // value is "A"
  } else if (value === "B") {
    // value is "B"
  } else if (value === "C") {
    // value is "C"
  } else {
    // value is "D"
  }
}
```

Refinements can also come in other forms other than testing for equality:

```js flow-check
// @flow
function method(value: boolean | Array<string> | Event) {
  if (typeof value === "boolean") {
    // value is a boolean
  } else if (Array.isArray(value)) {
    // value is an Array
  } else if (value instanceof Event) {
    // value is an Event
  }
}
```

Or you could refine on the shape of objects.

```js flow-check
// @flow
type A = { type: "A" };
type B = { type: "B" };

function method(value: A | B) {
  if (value.type === "A") {
    // value is A
  } else {
    // value is B
  }
}
```

Which also applies to nested types within objects.

```js flow-check
// @flow
function method(value: { prop?: string }) {
  if (value.prop) {
    value.prop.charAt(0);
  }
}
```

### Refinement Invalidations {#toc-refinement-invalidations}

It is also possible to invalidate refinements, for example:

```js flow-check
// @flow
function otherMethod() { /* ... */ }

function method(value: { prop?: string }) {
  if (value.prop) {
    otherMethod();
    // $ExpectError
    value.prop.charAt(0);
  }
}
```

The reason for this is that we don't know that `otherMethod()` hasn't done
something to our value. Imagine the following scenario:

```js flow-check
// @flow
var obj = { prop: "test" };

function otherMethod() {
  if (Math.random() > 0.5) {
    delete obj.prop;
  }
}

function method(value: { prop?: string }) {
  if (value.prop) {
    otherMethod();
    // $ExpectError
    value.prop.charAt(0);
  }
}

method(obj);
```

Inside of `otherMethod()` we sometimes remove `prop`. Flow doesn't know if the
`if (value.prop)` check is still true, so it invalidates the refinement.

There's a straightforward way to get around this. Store the value before
calling another method and use the stored value instead. This way you can
prevent the refinement from invalidating.

```js
// @flow
function otherMethod() { /* ... */ }

function method(value: { prop?: string }) {
  if (value.prop) {
    var prop = value.prop;
    otherMethod();
    prop.charAt(0);
  }
}
```
