---
title: Mapped Types
slug: /types/mapped-types
description: "How to use mapped types in Flow to transform object types or tuple types by iterating over their keys."
---

Mapped types transform object types by iterating over their keys and producing new property types.

```js flow-check
type O = {foo: number, bar: string};
type ReadOnly = {+[key in keyof O]: O[key]};
```

## When to use this {#toc-when-to-use}

Use mapped types when you need to systematically transform every property of an existing object type — for example, making all properties optional, read-only, or wrapping their values. If you only need to change a few specific properties, [object type spread](./objects.md#object-type-spread) is simpler.

## Basic Usage {#toc-basic-usage}

Mapped types have syntax similar to indexed object types but use the `in` keyword:
```js flow-check
type O = {foo: number, bar: string};

type Methodify<T> = () => T;

type MappedType = {[key in keyof O]: Methodify<O[key]>};
```

In this example, `MappedType` has all of the keys from `O` with all of the value types transformed by
`Methoditfy<O[key]>`. The `key` variable is substituted for each key in `O` when creating the property, so
this type evaluates to:
```
{
  foo: Methodify<O['foo']>,
  bar: Methodify<O['bar']>,
}
= {
  foo: () => number,
  bar: () => string,
}
```

## Mapped Type Sources {#toc-mapped-type-sources}

We call the type that comes after the `in` keyword the *source* of the mapped type. The source of
a mapped type must be a subtype of `string | number | symbol`:
```js flow-check
type MappedType = {[key in boolean]: number}; // ERROR!
```

Typically, you'll want to create a mapped type based on another object type. In this case, you
should write your mapped type using an inline `keyof`:
```js flow-check
type GetterOf<T> = () => T;
type Obj = {foo: number, bar: string};
type MappedObj = {[key in keyof Obj]: GetterOf<Obj[key]>};
```

> NOTE: `keyof` only works inline in mapped types for now. Full support for `keyof` is not yet available.

But you do not need to use an object to generate a mapped type. You can also use a union of string
literal types to represent the keys of an object type:
```js flow-check
type Union = 'foo' | 'bar' | 'baz';
type MappedType = {[key in Union]: number};
// = {foo: number, bar: number, baz: number};
```

Importantly, when using string literals the union is collapsed into a *single object type*:
```js flow-check
type MappedTypeFromKeys<Keys extends string> = {[key in Keys]: number};
type MappedFooAndBar = MappedTypeFromKeys<'foo' | 'bar'>;
// = {foo: number, bar: number}, not {foo: number} | {bar: number}
```

If you use a type like `number` or `string` in the source of your mapped type then Flow will create
an indexer:
```js flow-check
type MappedTypeFromKeys<Keys extends string> = {[key in Keys]: number};
type MappedFooAndBarWithIndexer = MappedTypeFromKeys<'foo' | 'bar' | string>;
// = {foo: number, bar: number, [string]: number}
```

## Distributive Mapped Types {#toc-distributive-mapped-types}

When the mapped type uses an inline `keyof` or a type parameter bound by `keyof`
Flow will distribute the mapped type over unions of object types.

For example:
```js flow-check
// This mapped type uses keyof inline
type MakeAllValuesNumber<O extends {...}> = {[key in keyof O]: number};
type ObjWithFoo = {foo: string};
type ObjWithBar = {bar: string};

type DistributedMappedType = MakeAllValuesNumber<
  | ObjWithFoo
  | ObjWithBar
>; // = {foo: number} | {bar: number};

// This mapped type uses a type parameter bound by keyof
type Pick<O extends {...}, Keys extends keyof O> = {[key in Keys]: O[key]};
type O1 = {foo: number, bar: number};
type O2 = {bar: string, baz: number};
type PickBar = Pick<O1 | O2, 'bar'>; // = {bar: number} | {bar: string};
```

Distributive mapped types will also distribute over `null` and `undefined`:
```js flow-check
type Distributive<O extends ?{...}> = {[key in keyof O]: O[key]};
type Obj = {foo: number};
type MaybeMapped = Distributive<?Obj>;// = ?{foo: number}
null as MaybeMapped; // OK
undefined as MaybeMapped; // OK
({foo: 3}) as MaybeMapped; // OK
```

## Property Modifiers {#toc-property-modifiers}

You can also add `+` or `-` variance modifiers and the optionality modifier `?` in mapped types:
```js flow-check
type O = {foo: number, bar: string}
type ReadOnlyPartialO = {+[key in keyof O]?: O[key]}; // = {+foo?: number, +bar?: string};
```

When no variance nor optionality modifiers are provided and the mapped type is distributive,
the variance and optionality are determined by the input object:
```js flow-check
type O = {+foo: number, bar?: string};
type Mapped = {[key in keyof O]: O[key]}; // = {+foo: number, bar?: string}
```

Otherwise, the properties are read-write and required when no property modifiers are present:
```js flow-check
type Union = 'foo' | 'bar' | 'baz';
type MappedType = {[key in Union]: number};
// = {foo: number, bar: number, baz: number};
```

> NOTE: Flow does not yet support removing variance or optionality modifiers.

## Mapped Type on Arrays {#toc-mapped-type-on-arrays}

Mapped type also works on array or tuple inputs. If the mapped type is in the form of

```
{[K in keyof <type_1>]: <type_2>}
```

then `type_1` is allowed to be an array or tuple type.

This feature will be especially useful if you want to map over elements of a tuple:

```js flow-check
type Tuple = [+a: number, b?: string];
type MappedTuple = {[K in keyof Tuple]: Tuple[K] extends number ? boolean : string};
const a: MappedTuple[0] = true;
const b: MappedTuple[1] = '';
'' as MappedTuple[0] // error
false as MappedTuple[1] // error
declare const mapped: MappedTuple;
mapped[0] = true; // error: cannot-write
```

For now, the only supported property modifier on array input is the optionality modifier `?`.

```js flow-check
type Tuple = [+a: number, b?: string];
type Supported = {[K in keyof Tuple]?: string};
type Unsupported1 = {+[K in keyof Tuple]: string}; // Error
type Unsupported2 = {-[K in keyof Tuple]: string}; // Error
```

## Adoption {#toc-adoption}

To use mapped types, you need to upgrade your infrastructure so that it supports the syntax:

- `flow` and `flow-parser`: 0.210.0. Between v0.210.0 to v0.211.1, you need to explicitly enable it in your .flowconfig, under the `[options]` heading, add `mapped_type=true`.
- `prettier`: 3
- `babel` with `babel-plugin-syntax-hermes-parser`. See [our Babel guide](../tools/babel.md) for setup instructions.
- `eslint` with `hermes-eslint`. See [our ESLint guide](../tools/eslint.md) for setup instructions.

## See Also {#toc-see-also}

- [Indexed Access Types](./indexed-access.md) — extracting property types, commonly used in mapped type bodies
- [Conditional Types](./conditional.md) — another advanced type feature for type-level logic
- [Generics](./generics.md) — mapped types rely on generic type parameters
- [Utility Types](./utilities.md) — built-in types like `Partial`, `Readonly`, and `Pick` (which mapped types generalize)
