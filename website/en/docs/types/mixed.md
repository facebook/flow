---
layout: guide
---

In general, programs have several different categories of types:

**A single type:**

Here the input value can only be a `number`.

```js
function square(n: number) {
  return n * n;
}
```

**A group of different possible types:**

Here the input value could be either a `string` or a `number`.

```js
function stringifyBasicValue(value: string | number) {
  return '' + value;
}
```

**A type based on another type:**

Here the return type will be the same as the type of whatever value is passed
into the function.

```js
function identity<T>(value: T): T {
  return value;
}
```

These three are the most common categories of types. They will make up the
majority of the types you'll be writing.

However, there is also a fourth category.

**An arbitrary type that could be anything:**

Here the passed in value is an unknown type, it could be any type and the
function would still work.

```js
function getTypeOf(value: mixed): string {
  return typeof value;
}
```

These unknown types are less common, but are still useful at times.

You should represent these values with `mixed`.

## Anything goes in, Nothing comes out <a class="toc" id="toc-anything-goes-in-nothing-comes-out" href="#toc-anything-goes-in-nothing-comes-out"></a>

`mixed` will accept any type of value. Strings, numbers, objects, functions–
anything will work.

```js
// @flow
function stringify(value: mixed) {
  // ...
}

stringify("foo");
stringify(3.14);
stringify(null);
stringify({});
```

When you try to use a value of a `mixed` type you must first figure out what
the actual type is or you'll end up with an error.

```js
// @flow
function stringify(value: mixed) {
  // $ExpectError
  return "" + value; // Error!
}

stringify("foo");
```

Instead you must ensure the value is a certain type by refining it.

```js
// @flow
function stringify(value: mixed) {
  if (typeof value === 'string') {
    return "" + value; // Works!
  } else {
    return "";
  }
}

stringify("foo");
```

Because of the `typeof value === 'string'` check, Flow knows the `value` can
only be a `string` inside of the `if` statement. This is known as a
[refinement](../../lang/refinements/).

## Mixed vs Any <a class="toc" id="toc-mixed-vs-any" href="#toc-mixed-vs-any"></a>
`mixed` in contrast to  `any` are very similar. They both represent an undetermined value, but with `any` you can manipulate or call any function without prior type refinement. While `mixed` requires type refinement before usage.
