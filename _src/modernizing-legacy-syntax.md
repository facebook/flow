---
title: "Modernizing Legacy Flow Syntax"
slug: /modernizing-legacy-syntax
description: "Reference for migrating Flow's $-prefixed utility types and other legacy syntax forms to their modern equivalents — what's been removed, what's been deprecated, and what's discouraged."
---

Flow's syntax has evolved substantially over time, with many forms converging on TypeScript-compatible spellings. Many `$`-prefixed utility types have been renamed, some have been removed entirely, and some legacy syntactic forms have modern keyword equivalents. This page is the canonical reference for what each legacy form should become.

The forms fall into three groups:

- [**Removed**](#toc-removed) — Flow no longer recognizes these and they must be rewritten.
- [**Deprecated**](#toc-deprecated) — a modern equivalent exists, and the Flow team intends to remove the legacy form in the future. Migrate now — it is cheaper than migrating later.
- [**Discouraged**](#toc-discouraged) — still recognized by the type checker, but new code should avoid the form.

## Removed {#toc-removed}

Flow no longer recognizes these.

| Legacy Flow | Modern rewrite | Removed in |
|---|---|---|
| `$Diff<A, B>` | Typically [`Omit<A, keyof B>`](./types/utilities.md#toc-omit) or restructuring. Not always semantically identical — handle case by case. | 0.268.0 |
| `$Rest<A, B>` | Same status as `$Diff`. Usually replaceable with [`Omit`](./types/utilities.md#toc-omit) + [spread](./types/objects.md#object-type-spread). | 0.267.0 |
| `$PropertyType<T, K>` | [`T[K]` (indexed access)](./types/indexed-access.md). | 0.266.0 |
| `$ElementType<T, K>` | [`T[K]` (indexed access)](./types/indexed-access.md). | 0.266.0 |
| `$TupleMap<T, F>` | [Mapped type](./types/mapped-types.md) with the body of `F` inlined — e.g. `$TupleMap<T, <V>(V) => Promise<V>>` becomes `{[K in keyof T]: Promise<T[K]>}`. | 0.248.0 |
| `$TupleMapi<T, F>` | [Mapped type](./types/mapped-types.md) with key access and `F` inlined — e.g. `{[K in keyof T]: [K, T[K]]}`. | 0.248.0 |
| `%checks` predicate functions | [Type guards](./types/type-guards.md) — e.g. `function isString(x: unknown): boolean %checks` becomes `function isString(x: unknown): x is string`. | 0.248.0 |
| `$Call<F, ...Args>` | [`ReturnType<F>`](./types/utilities.md#toc-return-type) plus [indexed access](./types/indexed-access.md), or a [conditional type](./types/conditional.md) with `infer`. | 0.247.0 |
| `$ObjMap<O, F>` | [Mapped type](./types/mapped-types.md) with `F` inlined — e.g. `$ObjMap<O, <V>(V) => Promise<V>>` becomes `{[K in keyof O]: Promise<O[K]>}`. | 0.246.0 |
| `$ObjMapi<O, F>` | [Mapped type](./types/mapped-types.md) with key access and `F` inlined — e.g. `{[K in keyof O]: [K, O[K]]}`. | 0.246.0 |
| `React.Element<typeof Foo>` | Prefer [render types](./react/render-types.md) when expressing composition constraints ("a `Menu` only renders `MenuItem`s") — that is what most uses of `React.Element<typeof Foo>` were really expressing. For generic "any element / any node" positions, use `React.MixedElement` / `React.Node`. Reach for `ExactReactElement_DEPRECATED<typeof Foo>` only as an escape hatch when exact element identity is genuinely required. | 0.245.0 |
| `$Shape<T>` | [`Partial<T>`](./types/utilities.md#toc-partial). **Not always semantically identical** — `$Shape` was permissive about depth in ways `Partial` is not, so migrate per call site rather than blindly renaming. | 0.206.0 |
| `$Partial<T>` | [`Partial<T>`](./types/utilities.md#toc-partial). | 0.203.0 |

## Deprecated {#toc-deprecated}

A modern equivalent exists. Some forms already error today; others still parse but are on a deprecation path.

### Utility types {#toc-deprecated-utilities}

| Legacy Flow | Modern Flow | Enabled by default since |
|---|---|---|
| `$Keys<T>` | [`keyof T`](./types/utilities.md#toc-keys) | 0.290 |
| `$ReadOnly<T>` | [`Readonly<T>`](./types/utilities.md#toc-readonly) | 0.290 |
| `$NonMaybeType<T>` | [`NonNullable<T>`](./types/utilities.md#toc-nonmaybe) | 0.290 |
| `$ReadOnlyArray<T>` | `ReadonlyArray<T>` | 0.290 |
| `$ReadOnlyMap<K, V>` / `$ReadOnlySet<T>` / `$ReadOnlyWeakMap<K, V>` / `$ReadOnlyWeakSet<T>` | `ReadonlyMap` / `ReadonlySet` / `ReadonlyWeakMap` / `ReadonlyWeakSet` | 0.290 |
| `$Values<T>` | [`Values<T>`](./types/utilities.md#toc-values) (renamed within Flow's own utilities) | 0.290 |
| `mixed` | [`unknown`](./types/unknown.md) | 0.290 |

### Non-utility syntax forms {#toc-deprecated-syntax}

| Legacy Flow | Modern Flow | Enabled by default since |
|---|---|---|
| `(x: T)` cast | [`x as T`](./types/casting.md) | 0.229 |
| `<T: Bound>` | [`<T extends Bound>`](./types/generics.md#toc-generic-types-act-as-bounds) | 0.302 |
| `{\| a: number \|}` exact | [`{a: number}` (exact is the default)](./types/objects.md#exact-and-inexact-object-types) | 0.202 |
| `+foo` / `-foo` property variance sigils | [`readonly foo`](./types/objects.md#read-only-object-properties) / `writeonly foo` keywords (`writeonly` is Flow-specific) | 0.315 |
| `+T` / `-T` type parameter variance sigils | [`out T` / `in T` keywords](./types/generics.md#toc-variance-sigils) | 0.315 |

## Discouraged {#toc-discouraged}

The form is still recognized by the type checker and does not error, but new code should avoid it.

| Type | Recommended approach |
|---|---|
| `$Exact<T>` | Define the exact type first and derive inexact variants from it via [object type spread](./types/objects.md#object-type-spread) (`{...Exact, ...}`). See [`$Exact<T>` (Discouraged)](./types/utilities.md#toc-exact). |

## `$`-prefixed utilities that are not deprecated {#toc-not-deprecated}

A few `$`-prefixed types remain first-class Flow utilities — the `$` is just part of the name, not a marker of being deprecated. They are esoteric and rarely encountered, but if you do hit one, keep using it. Examples include `$KeyMirror<O>` and `$Exports<...>`. See [Utility Types](./types/utilities.md) for the current list.

## See also {#toc-see-also}

- [Flow for TypeScript Users](./flow-vs-typescript.md) — broader comparison of Flow and TypeScript, including the TS-aligned subset of the renames covered here.
- [Utility Types](./types/utilities.md) — reference for Flow's current utility types.
- [Mapped Types](./types/mapped-types.md) — the modern replacement for `$ObjMap` / `$TupleMap` and friends.
- [Type Casting](./types/casting.md) — the `as` cast syntax that replaces `(x: T)`.
- [Indexed Access Types](./types/indexed-access.md) — the `T[K]` form that replaces `$PropertyType` / `$ElementType`.
