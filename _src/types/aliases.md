---
title: Type Aliases
slug: /types/aliases
description: "How to create reusable type aliases in Flow to name complex types and use them throughout your code."
---

Type aliases give a reusable name to any type. They are fully transparent — interchangeable with the type they refer to.

```js flow-check
type ID = number;
const userId: ID = 42;
```

## When to use this {#toc-when-to-use}

Use type aliases to give descriptive names to complex or frequently used types. Since aliases are transparent, any code that accepts the underlying type also accepts the alias and vice versa. When you need to hide the underlying type from consumers of a module, use [opaque type aliases](./opaque-types.md) instead. [Interfaces](./interfaces.md), [classes](./classes.md), and [enums](../enums/index.md) also introduce named types with additional capabilities beyond simple naming.

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

Type aliases can also have their own [generics](./generics.md).

```js flow-check
type MyObject<A, B, C> = {
  property: A,
  method(val: B): C,
};
```

Type alias generics are [parameterized](./generics.md#toc-parameterized-generics).
When you use a type alias you need to pass parameters for each of its generics.

```js flow-check
type MyObject<A, B, C> = {
  foo: A,
  bar: B,
  baz: C,
};

const val: MyObject<number, boolean, string> = {
  foo: 1,
  bar: true,
  baz: 'three',
};
```

## See Also {#toc-see-also}

- [Opaque Type Aliases](./opaque-types.md) — type aliases that hide their underlying type outside of the defining file
- [Generics](./generics.md) — parameterized types used with type aliases and functions
- [Interfaces](./interfaces.md) — another way to define reusable types, with structural (shape-based) checking for class instances
