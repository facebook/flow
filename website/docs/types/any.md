---
title: Any
slug: /types/any
---

> **Warning:** Do not mistake `any` with [`mixed`](../mixed). It's also not the same as [`empty`](../empty).

If you want a way to opt-out of using the type checker, `any` is the way to do
it. **Using `any` is completely unsafe, and should be avoided whenever
possible.**

For example, the following code will not report any errors:

```js flow-check
function add(one: any, two: any): number {
  return one + two;
}

add(1, 2);     // Works.
add("1", "2"); // Works.
add({}, []);   // Works.
```

Even code that will cause runtime errors will not be caught by Flow:

```js flow-check
function getNestedProperty(obj: any) {
  return obj.foo.bar.baz;
}

getNestedProperty({});
```

There are only a couple of scenarios where you might consider using `any`:

1. When you are in the process of converting existing code to using Flow
  types and you are currently blocked on having the code type checked (maybe
  other code needs to be converted first).
2. When you are certain your code works and for some reason Flow is unable to
  type check it correctly. There are a (decreasing) number of idioms in
  JavaScript that Flow is unable to statically type.

You can ban `any` by enabling the [`unclear-type`](../../linting/rule-reference/#toc-unclear-type) lint rule.

You can use the [coverage](../../cli/coverage/) command to identify code typed as `any`.

## Avoid leaking `any` {#toc-avoid-leaking-any}

When you have a value with the type `any`, you can cause Flow to infer `any`
for the results of all of the operations you perform.

For example, if you get a property on an object typed `any`, the resulting
value will also have the type `any`.

```js flow-check
function fn(obj: any) {
  let foo = obj.foo; // Results in `any` type
}
```

You could then use the resulting value in another operation, such as adding it
as if it were a number and the result will also be `any`.

```js flow-check
function fn(obj: any) {
  let foo = obj.foo; // Results in `any` type
  let bar = foo * 2; // Results in `any` type
}
```

You could continue this process until `any` has leaked all over your code.

```js flow-check
function fn(obj: any) {
  let foo = obj.foo;
  let bar = foo * 2;
  return bar; // Results in `any` type
}

let bar = fn({ foo: 2 }); // Results in `any` type
let baz = "baz:" + bar; // Results in `any` type
```

Prevent this from happening by cutting `any` off as soon as possible by casting
it to another type.

```js flow-check
function fn(obj: any) {
  let foo: number = obj.foo;
}
```

Now your code will not leak `any`.
