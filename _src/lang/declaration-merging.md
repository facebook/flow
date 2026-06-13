---
title: Declaration Merging
slug: /lang/declaration-merging
description: "How Flow merges multiple declarations of the same name — interfaces, classes, and namespaces."
---

Flow lets several declarations of the same name combine into one. Two interfaces with the same name union their members; a `declare class` and an `interface` fold together; a function or class can carry static members declared in a sibling namespace. Merging primarily shows up in [library definitions](../libdefs/creation.md), though some forms also work in regular source files.

:::info TypeScript comparison
Flow uses the same split-namespace model as TypeScript and supports merging in the cases needed to type declarations. Flow does *not* support TypeScript's runtime-merging (a `namespace` contributing runtime members to a sibling function), user-side `declare module 'name' { ... }` augmentation, or multi-block merging of `declare module` libdef definitions. See [Declaration merging is partially supported](../flow-vs-typescript.md#toc-declaration-merging) for the per-case comparison.
:::

## The split-namespace model {#toc-split-namespace}

Each name independently inhabits a *value* namespace and a *type* namespace, so a single identifier can be both a value and a type without colliding. Value-side uses resolve through the value namespace; type-side uses resolve through the type namespace. Constructs usable in both namespaces (classes, enums) register once on the value side and the type side falls back to it.

```js flow-check
const A = 1;
interface A {} // OK — `A` lives in both namespaces independently

const x: number = A; // `A` resolves as a value (the const)
declare const y: A; // `A` resolves as a type (the interface)
```

## Supported merges {#toc-supported-merges}

- **`interface` + `interface`** — members union. Compatible duplicates are allowed; conflicting members error. In **library definition files**, `extends` lists also concatenate, call signatures overload as intersections, and a type-parameter arity mismatch errors. In **regular source files**, the merge is limited to members — `extends` lists and call signatures don't combine across declarations, and same-named interfaces can't both be exported from a file.
- **`declare class` + `interface`** — interface members fold into the class (either order).
- **`function` / `declare function` + `declare namespace`** — the namespace's type members fold into the function and are accessible as `fn.T` (either order).
- **`class` / `declare class` + `declare namespace`** — the namespace's type members fold into the class and are accessible as `Cls.T` (either order).

## Not supported {#toc-not-supported}

- **Runtime merging.** Only the *type* members of a `declare namespace` reliably propagate to its sibling function or class. Value members (e.g., `declare const helper: number` inside `declare namespace fn`) are not treated as runtime properties on the host.
- **Multiple `declare module 'name' { ... }` blocks for the same module.** A second block is treated as an override of the first, not a union. A libdef for a given module should live in one place.
- **User-side `declare module 'name' { ... }` from a source file.** The form is only valid at the top level of a library file (see [Declaring a module in the global namespace](../libdefs/creation.md#toc-declaring-a-module-globally) for the libdef-only pattern).

## See Also {#toc-see-also}

- [Library Definitions](../libdefs/creation.md) — where `declare class`, `declare namespace`, and module declarations are most often written
- [Declaration Files](../declarations/index.md) — `.flow` files that describe colocated source, another place ambient declarations appear
- [Interfaces](../types/interfaces.md) — interface members and `extends` mechanics
- [Flow for TypeScript Users — Declaration merging](../flow-vs-typescript.md#toc-declaration-merging) — per-case comparison with TypeScript
