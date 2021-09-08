---
title: Opaque Type Aliases
slug: /types/opaque-types
---

Opaque type aliases are type aliases that do not allow access to their
underlying type outside of the file in which they are defined.

```js
opaque type ID = string;
```

Opaque type aliases, like regular type aliases, may be used anywhere a type can
be used.


```js
// @flow
opaque type ID = string;

function identity(x: ID): ID {
  return x;
}
export type {ID};
```

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

```js
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
as regular [type aliases](../aliases/) do.

```js
//@flow
opaque type NumberAlias = number;

(0: NumberAlias);

function add(x: NumberAlias, y: NumberAlias): NumberAlias {
    return x + y;
}
function toNumberAlias(x: number): NumberAlias { return x; }
function toNumber(x: NumberAlias): number { return x; }
```

### Outside the Defining File {#toc-outside-the-defining-file}

When importing an opaque type alias, it behaves like a
[nominal type](../../lang/nominal-structural/#toc-nominal-typing), hiding its
underlying type.

**`exports.js`**

```js
export opaque type NumberAlias = number;
```

**`imports.js`**

```js
import type {NumberAlias} from './exports';

(0: NumberAlias) // Error: 0 is not a NumberAlias!

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

```js
import type {ID} from './exports';

function formatID(x: ID): string {
    return "ID: " + x; // Ok! IDs are strings.
}

function toID(x: string): ID {
    return x; // Error: strings are not IDs.
}
```

When you create an opaque type alias with a subtyping constraint, the type in
the type position must be a subtype of the type in the super type position.

```js
//@flow
opaque type Bad: string = number; // Error: number is not a subtype of string
opaque type Good: {x: string} = {x: string, y: number};
```

### Generics {#toc-generics}

Opaque type aliases can also have their own [generics](../generics/),
and they work exactly as generics do in regular [type aliases](../aliases#toc-type-alias-generics)

```js
// @flow
opaque type MyObject<A, B, C>: { foo: A, bar: B } = {
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

### Library Definitions {#toc-library-definitions}

You can also declare opaque type aliases in
[libdefs](../../libdefs). There, you omit the underlying
type, but may still optionally include a super type.

```js
declare opaque type Foo;
declare opaque type PositiveNumber: number;
```
