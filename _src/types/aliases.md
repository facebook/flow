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

## Recursive Type Aliases {#toc-recursive-types}

A type alias can refer to itself, as long as the cycle passes through a constructor like an array, object, or function type. This is the canonical way to model tree- and list-like shapes:

```js flow-check
type Tree = {
  value: number,
  children: Array<Tree>,
};

const t: Tree = {
  value: 1,
  children: [
    {value: 2, children: []},
    {value: 3, children: [{value: 4, children: []}]},
  ],
};
```

Mutual recursion between aliases works as well, provided every cycle is broken by a constructor:

```js flow-check
type Doc  = {nodes: Array<Node>};
type Node =
  | {kind: "text",  text: string}
  | {kind: "group", doc: Doc};
```

## Type aliases are erased {#toc-aliases-erased}

A `type` declaration introduces no runtime binding — it only exists at compile time. You cannot use a type alias name in a value position, such as an argument, a `typeof` expression, or an `instanceof` check:

```js flow-check
type Status = "ok" | "error";

const s: Status = "ok"; // Works!
const v = Status;       // Error
```

This is unlike [classes](./classes.md) and [Flow Enums](../enums/index.md), which introduce a runtime value alongside a type of the same name.

## See Also {#toc-see-also}

- [Opaque Type Aliases](./opaque-types.md) — type aliases that hide their underlying type outside of the defining file
- [Generics](./generics.md) — parameterized types used with type aliases and functions
- [Interfaces](./interfaces.md) — another way to define reusable types, with structural (shape-based) checking for class instances
- [Const Expressions](./const-expression.md) — `as const` for producing runtime values that double as types
