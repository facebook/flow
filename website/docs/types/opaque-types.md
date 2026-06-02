---
title: Opaque Type Aliases
slug: /types/opaque-types
description: "How to use opaque type aliases in Flow to hide the underlying type representation outside of the defining module."
---

Opaque type aliases are type aliases that hide their underlying type outside of the file in which they are defined.

```js flow-check
opaque type ID = string;
```

:::info TypeScript comparison
Opaque types are Flow-only — TypeScript has no native equivalent. The common TS idiom is "branded types" (intersection with a private symbol-typed property), which is a userland pattern rather than a language feature. The boundary it enforces is weaker than Flow's file-scoped abstraction: TS brands are userland and forgeable with an `as` cast, and are structurally constructible when the brand key is exposed or string-keyed rather than a private `unique symbol`. See [Flow's opaque types](../flow-vs-typescript.md#toc-opaque-types) for the full comparison.
:::

## When to use this {#toc-when-to-use}

Use opaque types over regular [type aliases](./aliases.md) when you need to enforce abstraction boundaries across module boundaries — for example, preventing callers from treating an `ID` as a plain `string`. A regular `type ID = string` is transparent at every file boundary, so any consumer can treat an `ID` exactly like a `string`; `opaque type ID = string` keeps that transparency inside the defining file but seals the underlying type at the module boundary.

**`exports.js`**

```js
export type TransparentID = string;

export opaque type OpaqueID = string;
export function makeOpaqueID(s: string): OpaqueID { return s; }
```

**`imports.js`**

```js flow-check
// The importer's view: `TransparentID` is still `string`; `OpaqueID` is nominal.
declare type TransparentID = string;
declare opaque type OpaqueID;
declare function makeOpaqueID(s: string): OpaqueID;

const a: TransparentID = "abc"; // Works — transparent alias is interchangeable with string
const b: string = a; // Works — round-trips with no boundary

const oid: OpaqueID = makeOpaqueID("abc"); // Works — the only way to obtain an OpaqueID
const c: OpaqueID = "abc"; // Error — outside the defining file, string is not an OpaqueID
const d: string = oid; // Error — outside the defining file, OpaqueID is not a string
```

Reach for `opaque type` when callers must obtain a value only through a constructor function the defining module controls — for example IDs, sanitized strings, or units of measure. Keep `type` when the alias is documentation only and you're fine with consumers treating it as its underlying representation. Use the optional [supertype constraint](#toc-subtyping-constraints) when consumers need partial access (e.g. reading an `ID` as a `string` but not creating one from a `string`).

## Opaque Type Alias Syntax {#toc-opaque-type-alias-syntax}

Opaque type aliases are created using the words `opaque type` followed by its
name, an equals sign `=`, and a type definition.

```js
opaque type Alias = Type;
```

You can optionally add a subtyping constraint to an opaque type alias by adding
a colon `:` and a type after the name.

```js
opaque type Alias: SuperType = Type;
```

Any type can appear as the super type or type of an opaque type alias.

```js flow-check
opaque type StringAlias = string;
opaque type ObjectAlias = {
  property: string,
  method(): number,
};
opaque type UnionAlias = 1 | 2 | 3;
opaque type AliasAlias: ObjectAlias = ObjectAlias;
opaque type VeryOpaque: AliasAlias = ObjectAlias;
```

## Opaque Type Alias Type Checking {#toc-opaque-type-alias-type-checking}

### Within the Defining File {#toc-within-the-defining-file}

When in the same file the alias is defined, opaque type aliases behave exactly
as regular [type aliases](./aliases.md) do.

```js flow-check
opaque type NumberAlias = number;

0 as NumberAlias;

function add(x: NumberAlias, y: NumberAlias): NumberAlias {
    return x + y;
}
function toNumberAlias(x: number): NumberAlias { return x; }
function toNumber(x: NumberAlias): number { return x; }
```

### Outside the Defining File {#toc-outside-the-defining-file}

When importing an opaque type alias, it behaves like a
[nominal type](../lang/nominal-structural.md#toc-nominal-typing), hiding its
underlying type.

**`exports.js`**

```js
export opaque type NumberAlias = number;
```

**`imports.js`**

```js flow-check
// In imports.js, NumberAlias is opaque — its underlying type (number) is hidden
declare opaque type NumberAlias;

0 as NumberAlias; // Error: 0 is not a NumberAlias!

function convert(x: NumberAlias): number {
  return x; // Error: x is not a number!
}
```

### Subtyping Constraints {#toc-subtyping-constraints}

When you add a subtyping constraint to an opaque type alias, we allow the opaque
type to be used as the super type when outside of the defining file.

**`exports.js`**

```js flow-check
export opaque type ID: string = string;
```

**`imports.js`**

```js flow-check
// In imports.js, ID is opaque with a supertype constraint of string
declare opaque type ID: string;

function formatID(x: ID): string {
    return "ID: " + x; // OK! IDs are strings.
}

function toID(x: string): ID {
    return x; // Error: strings are not IDs.
}
```

When you create an opaque type alias with a subtyping constraint, the type in
the type position must be a subtype of the type in the super type position.

```js flow-check
opaque type Bad: string = number; // Error: number is not a subtype of string
opaque type Good: {x: string, ...} = {x: string, y: number};
```

### Generics {#toc-generics}

Opaque type aliases can also have their own [generics](./generics.md),
and they work exactly as generics do in regular [type aliases](./aliases.md#toc-type-alias-generics)

```js flow-check
opaque type MyObject<A, B, C>: {foo: A, bar: B, ...} = {
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

### Library Definitions {#toc-library-definitions}

You can also declare opaque type aliases in
[libdefs](../libdefs/index.md). There, you omit the underlying
type, but may still optionally include a super type.

```js flow-check
declare opaque type Foo;
declare opaque type PositiveNumber: number;
```

## See Also {#toc-see-also}

- [Type Aliases](./aliases.md) — regular (transparent) type aliases
- [Nominal & Structural Typing](../lang/nominal-structural.md) — opaque types are nominally typed, unlike regular type aliases
- [Generics](./generics.md) — opaque type aliases can be parameterized with generics
- [Library Definitions](../libdefs/index.md) — declaring opaque types in library definition files
