---
title: Width Subtyping
slug: /lang/width-subtyping
---

It's safe to use an object with "extra" properties in a position that is
annotated with a specific set of properties.

```js flow-check
// @flow
function method(obj: { foo: string }) {
  // ...
}

method({
  foo: "test", // Works!
  bar: 42      // Works!
});
```

Within `method`, we know that `obj` has at least a property `foo` and the
property access expression `obj.foo` will have type `string`.

This is a kind of subtyping commonly referred to as "width subtyping" because
a type that is "wider" (i.e., has more properties) is a subtype of a
narrower type.

So in the following example, `obj2` is a _subtype_ of `obj1`.

```js flow-check
let obj1 = { foo: 'test' };
let obj2 = { foo: 'test', bar: 42 };
```

However, it's often useful to know that a property is definitely absent.

```js flow-check
// @flow
function method(obj: { foo: string } | { bar: number }) {
  if (obj.foo) {
    (obj.foo: string); // Error!
  }
}
```

The above code has a type error because Flow would also allow the call
expression `method({ foo: 1, bar: 2 })`, because `{ foo: number, bar: number }`
is a subtype of `{ bar: number }`, one of the members of the parameter's union
type.

For cases like this where it's useful to assert the absence of a property,
Flow provides a special syntax for
["exact" object types](../../types/objects/#toc-exact-object-types).

```js flow-check
// @flow
function method(obj: {| foo: string |} | {| bar: number |}) {
  if (obj.foo) {
    (obj.foo: string); // Works!
  }
}
```

[Exact object types](../../types/objects/#toc-exact-object-types) disable width
subtyping, and do not allow additional properties to exist.

Using exact object types lets Flow know that no extra properties will exist at
runtime, which allows [refinements](../refinements/) to get more specific.

```js flow-check
// @flow
function method(obj: {| foo: string |} | {| bar: number |}) {
  if (obj.foo) {
    (obj.foo: string); // Works!
  }
}
```
