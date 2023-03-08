---
title: Type Aliases
slug: /types/aliases
---

When you have complicated types that you want to reuse in multiple places, you
can alias them in Flow using a **type alias**.

```js flow-check
type MyObject = {
  foo: number,
  bar: boolean,
  baz: string,
};
```

These type aliases can be used anywhere a type can be used.

```js flow-check
type MyObject = {
  // ...
};

const val: MyObject = { /* ... */ };
function method(val: MyObject) { /* ... */ }
class Foo { constructor(val: MyObject) { /* ... */ } }
```

Type aliases are just that: aliases. They don't create a _different_ type, just another name for one.
This means that a type alias is completely interchangeable with the type it is equal to.

```js flow-check
type MyNumber = number;
declare const x: MyNumber;
declare function foo(x: number): void;
foo(x); // ok, because MyNumber = number
```

[Opaque type aliases](../opaque-types) offer an alternative for when you don't want to treat the
types as the same.

## Type Alias Syntax {#toc-type-alias-syntax}

Type aliases are created using the keyword `type` followed by its name, an
equals sign `=`, and a type definition.

```js
type Alias = Type;
```

Any type can appear inside a type alias.

```js flow-check
type NumberAlias = number;
type ObjectAlias = {
  property: string,
  method(): number,
};
type UnionAlias = 1 | 2 | 3;
type AliasAlias = ObjectAlias;
```

#### Type Alias Generics {#toc-type-alias-generics}

Type aliases can also have their own [generics](../generics).

```js flow-check
type MyObject<A, B, C> = {
  property: A,
  method(val: B): C,
};
```

Type alias generics are [parameterized](../generics#toc-parameterized-generics).
When you use a type alias you need to pass parameters for each of its generics.

```js flow-check
type MyObject<A, B, C> = {
  foo: A,
  bar: B,
  baz: C,
};

var val: MyObject<number, boolean, string> = {
  foo: 1,
  bar: true,
  baz: 'three',
};
```
