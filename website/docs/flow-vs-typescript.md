---
title: "Flow for TypeScript Users"
slug: /flow-vs-typescript
description: "Flow vs. TypeScript: how object exactness, variance defaults, `as` casts, type guards, and Flow's `component`/`hook`/`renders` syntax differ from TypeScript."
---

Flow and TypeScript share most of the same syntax, much of the same vocabulary, and a large set of overlapping concepts (conditional types, mapped types, `keyof`, `as const`, `unknown`, `Readonly`, generics, type guards). The convergence is largely intentional — Flow's syntax has shifted to align with TypeScript's over the past several years. If you know TypeScript, your intuition will get you most of the way through a Flow program.

Where the two diverge, the divergence is usually a deliberate Flow choice in favor of stronger static guarantees. Flow rejects a number of patterns that TypeScript accepts but that can throw at runtime or leave the program with inaccurate static types.

React is one notable area where Flow does not mirror TypeScript: Flow ships its own first-class [`component`](./react/component-syntax.md), [`hook`](./react/hook-syntax.md), and [`renders`](./react/render-types.md) syntax instead of modeling components through function types, `forwardRef`, and framework/library patterns — covered in [Flow-only concepts](#toc-flow-only) below.

This page is organized in four buckets:

- [Concepts that transfer cleanly from TypeScript](#toc-transfer-cleanly) — start here for syntax and concepts you can reuse directly.
- [Shared concepts, different rules](#toc-shared-concepts-different-rules) — if TypeScript-shaped code is rejected by Flow, these are the most likely culprits.
- [Flow-only concepts with no built-in TypeScript analogue](#toc-flow-only).
- [TypeScript-only features that do not exist in Flow](#toc-ts-only).

After those comparison buckets, the page closes with cross-cutting reference sections on [upcoming TS-aligned work](#toc-coming-soon), [legacy Flow syntax convergence](#toc-convergence), [shared config options](#toc-shared-options), and [external declaration mechanisms](#toc-declaration-mechanisms).

> **Scope note:** Flow is a typechecker only — the `flow` binary doesn't emit JavaScript. TypeScript is both a typechecker *and* a compiler. See [Getting Started](./getting-started.md#toc-installation) for setting up Flow. *TypeScript claims below verified against version 6.0.3 with `strict` enabled*.

## Concepts that transfer cleanly from TypeScript {#toc-transfer-cleanly}

The features below are close enough in syntax and semantics that you can reuse your TypeScript intuition more or less directly.

- [Conditional types](./types/conditional.md) with `infer`.
- [Mapped types](./types/mapped-types.md).
- [Type guards](./types/type-guards.md) of the form `param is T` (with additional validations), including inferred type predicates.
- [`T[K]`](./types/indexed-access.md) indexed access types.
- [`keyof T`](./types/utilities.md) operator.
- [`as const`](./types/const-expression.md) assertions.
- [`const` type parameters](./types/const-expression.md#const-type-parameters) — `function f<const T>(x: T): T`.
- [`unknown`](./types/unknown.md) top type.
- [Generic bounds](./types/generics.md#toc-adding-types-to-generics) with `<T extends Bound>`.
- [Array](./types/arrays.md) shorthand `T[]` (in addition to `Array<T>`).
- [Utility types](./types/utilities.md): `Readonly`, `ReadonlyArray`, `ReadonlyMap`, `ReadonlySet`, `Pick`, `Omit`, `Record`, `Partial`, `Required`, `Exclude`, `Extract`, `NonNullable`, `Parameters`, `ReturnType`, `Awaited`, `ThisParameterType`, `OmitThisParameter`, `NoInfer`.
- [Type-only imports and exports](./types/modules.md#toc-importing-and-exporting-types) — `import type` and `export type`.
- [Function-overload encoding via intersection](./types/intersections.md#toc-intersection-of-function-types) — `((x: number) => string) & ((x: string) => number)` resolves the per-call return type by argument type in both languages.
- [Ambient declaration forms](./libdefs/creation.md) like `declare const`, `declare let`, `declare class`, and `declare function`.

## Shared concepts, different rules {#toc-shared-concepts-different-rules}

This is the bucket where Flow most often surprises a reader coming from TypeScript. Both languages have these concepts — objects, classes, variance, refinement, generics, module exports, suppressions — but Flow's rules diverge in ways that have little or no surface signal.

Most subsections are same-syntax-different-semantics: code that compiles in TypeScript but is wrong, unsound, or rejected in Flow. The rest are renamed spellings, validations Flow adds at module boundaries, or a different suppression form. Each is a small, mechanical adjustment from the TS form, and the subsection headers below tell you which case applies.

- [Objects, classes, and interfaces](#toc-shape-rules) - exact objects, nominal classes, asymmetric class/object/interface subtyping
- [Type spellings](#toc-type-spellings) - `void` vs `undefined`, `?T`, `empty`, `unknown`
- [Variance](#toc-variance) - `readonly`, `writeonly`, `in`, and `out`
- [Refinement and module-level validation](#toc-validation) - validated type-guard bodies, refinement invalidation, module-boundary annotations
- [Explicit type controls](#toc-explicit-controls) - `as` casts, error suppressions, and type argument omission

:::info Nominal vs. structural typing.
TypeScript is primarily structural — two types with the same public shape are interchangeable, with narrow nominal carve-outs (`#private` fields, `private`/`protected` modifiers, and `unique symbol`). Flow is structural for plain objects and functions, but deliberately *nominal* for [classes](#toc-classes-nominal), [opaque types](#toc-opaque-types), and [Flow Enums](#toc-flow-enums).

The reason is that identity carries real information at those boundaries — a `UserId` is not a `PostId`, a `Celsius` is not a `Fahrenheit`, two distinct classes with the same fields are different concepts. Treating identity nominally rather than structurally lets the type system catch entire categories of logic bugs (the "right shape, wrong meaning" class of mistakes) and lets users model their domain at the level of *what something is*, not just *what it looks like*. The [Classes are nominal](#toc-classes-nominal) subsection below is the concrete instance of this stance; opaque types and Flow Enums (covered later in [Flow-only concepts](#toc-flow-only)) are the others.
:::

### Objects, classes, and interfaces {#toc-shape-rules}

How Flow types objects, classes, and interfaces — the exact-by-default object rule, nominal class identity, the asymmetric subtyping rules, primitive-to-interface assignability, optional re-introduction, type-level spread, tuple spread, and class-method `this` binding.

| Surface | TypeScript | Flow | Details |
|---|---|---|---|
| Object types | `{x: number}` allows extra properties except for fresh object-literal excess-property checks. | `{x: number}` is exact by default; use `{x: number, ...}` when extra properties are allowed. | [Object exactness](#toc-object-exactness) |
| Class / interface / object subtyping | Classes, interfaces, and object types are mostly interchangeable by structure. | Classes are nominal; object types accept only themselves; interfaces can accept all three | [Classes are nominal](#toc-classes-nominal) |
| `implements` / `extends` arg | Can target object-shaped utility types like `Pick<T, K>` or `Omit<T, K>`. | Must name an interface or class, not an object type alias. | [`implements` and `extends` clauses](#toc-implements-extends-rhs) |
| Primitives vs interfaces | Primitives satisfy object/interface shapes that exist on the boxed prototype. | Primitives are not subtypes of object types or interfaces. | [Primitives are not subtypes of interfaces or object types](#toc-primitives-interfaces) |
| Object combination | Intersections are the standard way to combine object types. | Use object type spread (`{...A, ...B}`); intersections, while supported for inexact objects, don't work for exact objects. | [Object type spread is type-level in Flow](#toc-type-spread) |

#### Object exactness {#toc-object-exactness}

In Flow, object types are *exact by default*: `{x: number}` admits exactly the property `x` and no others. To allow additional properties, write the inexact form with a trailing `...`: `{x: number, ...}`. In TypeScript, object types are open at the type-system level — `{x: number}` allows additional properties, and the rule that catches extras is the *excess-property check*, which fires only on direct object-literal assignment.

That distinction matters because it explains why TypeScript code can look like it agrees with Flow's exactness when it doesn't: `const v: T = {x: 1, y: 2}` errors in TS too, but only because the literal is inlined. Indirect cases — assigning the literal to a variable first, passing it through a function, or other paths where the literal's "fresh" status is lost — type-check in TS and would not be exactness violations there. Flow's exactness applies uniformly regardless of binding shape.

```js flow-check
type T = {x: number};
const extra = {x: 1, y: 2};
const v: T = extra; // ERROR
```

```ts
// TypeScript accepts this — `extra` is inferred as `{x: number, y: number}`,
// which is structurally a subtype of `{x: number}` because TS object types
// are open. The excess-property check does not fire on indirect assignment.
type T = {x: number};
const extra = {x: 1, y: 2};
const v: T = extra;
```

When the extra properties are intentional, the fix is the inexact form — write `{x: number, ...}` so the type explicitly admits unknown additional properties:

```js flow-check
type T = {x: number, ...};
const extra = {x: 1, y: 2};
const v: T = extra; // OK
```

#### Classes are nominal; the object/interface/class subtyping is asymmetric {#toc-classes-nominal}

TypeScript types classes structurally — `interface`, `type`, and class instances are largely interchangeable as long as the shapes match. In Flow, an *object type* describes a plain object — specifically the shape produced by an object literal `{...}` — while an [*interface*](./types/interfaces.md) describes a contract that any value can satisfy, plain object or class instance. From those definitions, combined with Flow's [nominal typing](./lang/nominal-structural.md) for classes (two distinct classes with the same members are not interchangeable), the one-way subtyping triangle follows directly:

- An object literal flows into an object type or an interface — its shape and kind are both fully known at the point of construction.
- A class instance flows into an interface (the contract makes no claim about what backs it) but **not** into an object type (object types describe object literals, not instances).
- An interface-typed value flows into another interface but **not** into an object type — it could be backed by a class instance, and the object type wouldn't accept that backing value.

Inexactness (`{a: number, ...}`) widens the set of additional *plain-object* properties allowed; it does not widen the set of *kinds of values* accepted, so the class-instance and interface cases still fail against an inexact object type — just with a different diagnostic:

- Against an *exact* object type (the default), the error is `[incompatible-exact]`.
- Against an *inexact* object type (`{a: number, ...}`), the error is `[class-object-subtyping]` with text "Class instances are not subtypes of object types; consider rewriting object type as an interface."

```js flow-check
class Foo {
  a: number = 1;
}
interface I { a: number }
type Obj = {a: number, ...};

declare function acceptsInterface(x: I): void;
declare function acceptsObj(x: Obj): void;
declare const someI: I;

acceptsInterface(new Foo()); // OK
acceptsInterface({a: 1});    // OK
acceptsObj({a: 1});          // OK
acceptsObj(new Foo());       // ERROR
acceptsObj(someI);           // ERROR — same code as above
```

The canonical fix when you hit this in Flow is to switch the parameter type from object type to interface.

One more direction-of-travel note: TypeScript's class-structurality is *almost* total — `const c: C = {x: 1}` type-checks in TS even though `c` is annotated as a class instance. TS preserves a handful of nominal channels on top of the structural default — ECMAScript `#private` fields, the `private` / `protected` access modifiers (both of which block assignability across distinct declarations), and `unique symbol` — but they're carve-outs from an otherwise structural model. Flow's class nominalism is total: no nominal opt-in is *required* because the class identity itself is the nominal channel. This is why Flow's class/object error fires far more often than the inverse experience would suggest.

#### `implements` and `extends` clauses must name an interface or class {#toc-implements-extends-rhs}

The right-hand side of an `implements` or `extends` clause has stricter shape rules in Flow than in TypeScript. TypeScript lets you write `class C implements Omit<HTMLAttrs, "k">` or `interface I extends Pick<Y, K>` — any object type works. Flow rejects object types in those clauses with their own diagnostics:

- `class C implements ObjType` errors with `[cannot-implement]` "Cannot implement `ObjType` because it is not an interface."
- `interface I extends ObjType` errors with `[incompatible-use]` "Cannot extend `ObjType` ... because `ObjType` is not inheritable."

The canonical Flow rewrite is to introduce an interface (`interface I { a: number; b: string }` then `class C implements I`) or to inline the members directly. Mapped/utility types applied to interfaces work in Flow but the result is an object type, which is exactly what these clauses won't accept — so the rewrite needs to land at an interface, not at an object-typed alias.

```js flow-check
type ObjType = {a: number, b: string};
class C implements ObjType { // ERROR — [cannot-implement]
  a: number = 1;
  b: string = "hi";
}
interface I extends ObjType { // ERROR — [incompatible-use]
  c: boolean;
}
```

#### Primitives are not subtypes of interfaces or object types {#toc-primitives-interfaces}

TypeScript treats `string` / `number` / `boolean` as structurally assignable to any interface or object type they satisfy — the primitive is checked against the members of its corresponding boxed prototype (`String.prototype` / `Number.prototype` / `Boolean.prototype`), so a `string` satisfies any interface whose members exist on `String.prototype` (e.g. `{length: number}`, `{charAt(i: number): string}`). No runtime boxing is implied — the compatibility is purely at the type level. Flow does not perform that check in `.js` files: a primitive flowing into an interface errors with `[incompatible-type]` ("Cannot use string as a subtype of interface"), and a primitive flowing into an object type errors with the generic `[incompatible-type]` code.

```js flow-check
interface HasLength { length: number }
const s: string = "abc";
const i: HasLength = s;             // ERROR
const o: {length: number, ...} = s; // ERROR
```

```ts
// TypeScript accepts both via wrapper-promotion.
interface HasLength { length: number }
const s: string = "abc";
const i: HasLength = s;
const o: {length: number} = s;
```

The Flow rewrite is to construct the interface-shaped value explicitly (`const i: HasLength = {length: s.length}`) or to read the property directly off the primitive (`s.length`) rather than asserting structural compatibility.

A useful corollary: TypeScript's `object` type (any non-primitive value) maps to Flow's `interface {}` — the empty interface accepts any object, array, or class instance and rejects primitives for the same reason described above.

#### Optional properties cannot be silently re-introduced {#toc-optional-reintroduction}

TypeScript allows a property to be forgotten via inexact subtyping and then re-introduced at a different (optional) type — leaving `y` typed as `number | undefined` while it actually holds the string `"Uh oh"`:

```ts
// TypeScript:
const a: {x: number, y: string} = {x: 1, y: "Uh oh"};
const b: {x: number} = a;                  // y "forgotten"
const c: {x: number, y?: number} = b;      // y re-introduced at a new type
// c.y has static type `number | undefined` but holds "Uh oh" at runtime.
```

Flow blocks this path in two places. A literal Flow translation already fails at `const b: {x: number} = a`, because `{x: number}` is exact by default. If you intentionally model the TypeScript "forget `y`" step with an inexact target (`{x: number, ...}`), Flow still rejects the next assignment, where `y` would be re-introduced at a different optional type. Exactness gates both directions: a property can only be forgotten when the target is inexact, and re-introduced only when the source is exact. This is the same underlying mechanism as [object exactness](#toc-object-exactness) showing up in a different shape — typical when modeling a Flow function on a TypeScript signature that takes a "looser" type and adds optional fields.

#### Object type spread is type-level in Flow, value-level only in TS {#toc-type-spread}

Flow lifts `{...A, b: T}` to the type level — `type C = {...A, b: T}` is a real type annotation that combines `A`'s properties with `b: T`. TypeScript has no type-level spread; it uses intersection (`type C = A & {b: T}`) instead.

This isn't a stylistic choice — it falls out of [exact object types](./types/objects.md#exact-and-inexact-object-types). Because Flow's exact object types forbid unlisted properties, intersecting two exact object types produces an *impossible* type: a value would have to be exactly `A` *and* exactly `B` simultaneously, which is uninhabitable as soon as `A` and `B` differ at all (see [impossible intersection types](./types/intersections.md#toc-impossible-intersection-types)). So Flow needs a different operation to combine exact object types. Type-level spread (`{...A, ...B}`) is that operation, and it mirrors the runtime semantics of value-level spread directly: own properties only (so [interfaces can't be spread](./types/objects.md#object-type-spread), since they don't track own-vs-prototype), later keys overwrite earlier ones, and exactness propagates — spreading an inexact type forces the result inexact, since the source could carry unknown properties.

The intersection form `A & {b: T}` is the natural reach if you're thinking in TypeScript, but it's the wrong tool for the job in Flow: `&` keeps its TypeScript intersection semantics, so writing `A & {b: T}` when `A` already declares `b` silently produces an uninhabitable type rather than the merged shape you wanted. The Flow idiom is `{...A, b: T}` — same shape as runtime spread, accurate semantics, no accidental impossibility. The [objects docs](./types/objects.md#object-type-spread) cover the full spread rules.

```js flow-check
type A = {x: number, y: string};
type C = {...A, z: boolean};
const c: C = {x: 1, y: "hi", z: true};
```

#### Tuple spread after an optional element is banned {#toc-tuple-spread-optional}

Spreading a tuple type with optional elements into another tuple is allowed in TypeScript but produces an inaccurate tuple type: `const x: [a?: 1] = []; const y: [0, 1 | undefined, 2] = [0, ...x, 2];` compiles in TS but `y[2]` is `undefined` at runtime. Flow rejects the spread with `[invalid-tuple-arity]` ("array literal has an unknown number of elements").

```js flow-check
const x: [a?: 1] = [];
const y: [0, 1 | void, 2] = [0, ...x, 2]; // ERROR
```

The TS type is statically known — `y` is annotated as `[0, 1 | undefined, 2]` and that annotation is accepted — but it is unsound: when `x` is empty at runtime, the value at position `1` is `2` (shifted) and position `2` is absent, so the tuple TS computed does not match the runtime layout. Flow rejects the spread because the source tuple's arity is not statically fixed, so no sound static tuple shape can be produced for the result. The Flow rewrite is to branch explicitly on whether the optional element is present and assemble each shape on its own arm.

#### Class methods cannot be unbound from their `this` {#toc-method-unbinding}

Method-shorthand properties on a *class* track their `this` binding in the type system; extracting one (`const f = c.m`) would lose that binding and is rejected with `[method-unbinding]` "Cannot get `c.m` because property `m` cannot be unbound from the context where it was defined." TypeScript treats methods as plain function values and lets the same extraction through silently — the resulting call then has the wrong `this` at runtime.

```js flow-check
class C {
  x: number = 0;
  m(): number { return this.x; }
}
const c = new C();
const f: () => number = c.m; // ERROR — [method-unbinding]
```

The Flow rewrites are either keep the call bound (`c.m()` directly), wrap with an arrow that captures `this` (`const f = () => c.m()`), or call `.bind` (`const f = c.m.bind(c)`). Note this is a class-instance rule — method-shorthand on plain object types (`{m(x: number): number}`) doesn't carry a `this` context to lose (usage of `this` in object literals is banned), so extraction is allowed there.

### Type spellings {#toc-type-spellings}

How Flow spells absent or nullable types and the top and bottom of the type hierarchy. These are name-only divergences — same concepts, different spellings.

| Concept | TypeScript | Flow | Note |
|---|---|---|---|
| Type inhabited only by `undefined` | `undefined` | `void` | Flow has no separate `undefined` type. Using `undefined` as an annotation errors with `[unsupported-syntax]`. ([details](#toc-void-vs-undefined)) |
| "No useful value" return marker | `void` | `void` | Same name; Flow has *only* `void` (see above). |
| Nullable value (`T \| null \| undefined`) | `T \| null \| undefined` | [`?T`](./types/maybe.md) (shorthand for `T \| null \| void`) | `T \| void` alone lacks `null`. |
| Bottom type | `never` | [`empty`](./types/empty.md) | `never` is the natural TS reach when Flow expects `empty`. |
| Top type | `unknown` | [`unknown`](./types/unknown.md) | Same name. |

See [type hierarchy](./lang/type-hierarchy.md) for where these sit relative to the rest of Flow's types.

#### `void` vs `undefined` {#toc-void-vs-undefined}

`undefined` as an annotation is a hard error in Flow:

```js flow-check
function f(): undefined { // ERROR — [unsupported-syntax]
  return undefined;
}
```

```js flow-check
function f(): void {
  return undefined; // OK — `undefined` is the value inhabiting `void`
}
```

This comes up most often when a TS-shaped function signature gets typed in Flow verbatim, and on TS utility-typed code (`Exclude<T, undefined>`, `T extends undefined ? ...`) where the `undefined` literal type appears inside a generic. Canonical Flow forms: `undefined` → `void` for annotations; `T | undefined` → `?T` if `null` is also intended (most JS APIs) or `T | void` if only the absent case is intended.

A related TS quirk worth flagging: TypeScript's `() => undefined` and `() => void` are assignably asymmetric (`undefined` returns satisfy `void` slots but not vice versa). Flow has no analogue since there's only `void`.

A parameter type that includes `void` — whether spelled `T | void`, `?T`, or `T | null | void` — makes the argument implicitly optional, so callers can omit it entirely. This differs from TypeScript, where `(x: T | undefined)` still requires the call site to pass `undefined`.

```js flow-check
function f(x: ?number) {}
f(null);      // OK
f(undefined); // OK
f();          // OK — `?T` includes `void`, which makes the arg optional
```

### Variance {#toc-variance}

Flow's variance defaults are stricter than TypeScript's. The subsections below cover the keyword syntax for opting in or out and the positions where the defaults diverge.

| Surface | TypeScript | Flow | Details |
|---|---|---|---|
| Variance keywords | Uses `readonly` properties and `in` / `out` type parameters; can also spell explicit invariance as `<in out T>`. | Uses `readonly` / `writeonly` properties and `in` / `out` type parameters; default type-parameter variance is invariant. | [Variance keywords](#toc-variance-keywords) |
| Mutable object properties | Covariant. | Invariant. | [Mutable object properties](#toc-variance-mutable-props) |
| `readonly` property assignability | `readonly` and mutable properties are assignable in ways that can drop the read-only constraint. | `readonly` cannot be dropped by assigning to a mutable-property type. | [`readonly` properties](#toc-variance-readonly-props) |
| Mutable arrays | Covariant. | Invariant. | [Mutable arrays](#toc-variance-arrays) |
| Generic type arguments | Variance is inferred from usage with compatibility-oriented exceptions. | Invariant by default unless declared `out` or `in`. | [Generic type arguments](#toc-variance-generics) |
| Method parameters | Bivariant for method syntax; function-typed fields are contravariant. | Contravariant. | [Method parameters](#toc-variance-methods) |

:::info Variance — a quick overview.
*Variance* describes how subtyping flows through a position where a type `T` appears — for example, the property type in `{x: T}`, a function parameter or return type, or a generic argument like `Container<T>`. Given that `Sub` is a subtype of `Super`, that position is:

- **Covariant** — preserves direction. A `{readonly x: Sub}` is a subtype of `{readonly x: Super}`. The right choice for read-only positions and function return types.
- **Contravariant** — reverses direction. A function `(x: Super) => void` is a subtype of `(x: Sub) => void` — a callee that accepts wider inputs satisfies a caller passing narrower ones.
- **Invariant** — neither direction; the position can't soundly widen or narrow. The required default whenever a slot is both read *and* written (e.g., a mutable `{x: T}`), since covariance breaks writes and contravariance breaks reads.
- **Bivariant** — both directions accepted. Usually unsound; TypeScript permits it in a few places (notably method parameters). Flow never uses bivariance.

Flow defaults each position to the strictest sound choice; TypeScript defaults to looser ones at several positions, which is the entire reason this section exists.
:::

#### Variance keywords (`readonly` / `writeonly`, `in` / `out`) {#toc-variance-keywords}

Flow's standard syntax for variance uses the TS-aligned keyword forms: `readonly` / `writeonly` on properties and `in` / `out` on type parameters. Note that `writeonly` is Flow-specific — TypeScript has no write-only equivalent.

In the other direction, TypeScript's combined `<in out T>` (explicit invariance) has no Flow counterpart. TypeScript infers variance from usage and preserves several compatibility-oriented exceptions, so users sometimes need to opt *back into* invariance to recover the stricter guarantee they wanted; Flow's default is invariance, so the stricter choice is what you get when you write nothing. See [Generic type arguments](#toc-variance-generics) below for the defaults contrast in detail.

Beyond the spelling, Flow validates that a type parameter declared `out T` (or `in T`) is only used in body positions that match the declared variance — `out T` in an input position errors `[incompatible-variance]` "Cannot use `T` in an input position because `T` is expected to occur only in output positions." TypeScript also validates `in` / `out` against the body in many positions (e.g. `interface Box<out T> { set: (t: T) => void }` errors in TS too, since the function-typed *field* puts `T` contravariantly — function inputs flip variance). The narrower gap is that TS keeps *method shorthand* bivariant even under an `out`/`in` annotation, so the Flow form below — written with method shorthand — errors in Flow but compiles in TS.

```js flow-check
type Box<out T> = {
  set(t: T): void; // ERROR — [incompatible-variance]
};
```

This subsection is about the *syntax*; for the much more important *semantic* divergence in how variance is enforced at each position, see the next subsection. See the [variance docs](./lang/variance.md) for full mechanics.

Each subsection below is a place where Flow picks the stricter sound default and TypeScript picks the looser one. Together they are the largest single cluster of TypeScript code that type-checks but relies on weaker static guarantees — every example accepts a program that can throw at runtime or leave inaccurate static types.

#### Mutable object properties are invariant in Flow, covariant in TS {#toc-variance-mutable-props}

Assigning `{x: number}` to `{x: number | string}` widens the slot's read type but also widens what can be written into it, so a downstream `obj.x = "oh no"` would corrupt the original.

```js flow-check
function f(obj: {x: number | string}) {}
const o: {x: number} = {x: 1};
f(o); // ERROR — property `x` is invariantly typed
```

```ts
// TypeScript allows this — the property is covariant, so `{x: number}`
// is treated as a subtype of `{x: number | string}`.
function f(obj: {x: number | string}) {}
const o: {x: number} = {x: 1};
f(o);
```

The fix is to make the target read-only — either with the [`Readonly<T>`](./types/utilities.md#toc-readonly) utility or the `readonly` property modifier. Removing the possibility of mutation through `obj` is what makes the widening safe. TypeScript supports the same `readonly` property modifier, but see the next sub-bullet for how the two languages diverge on enforcing it.

```js flow-check
function f(obj: Readonly<{x: number | string}>) {} // or {readonly x: number | string}
const o: {x: number} = {x: 1};
f(o); // OK
```

#### `readonly` properties are interchangeable with mutable in TS, but not in Flow {#toc-variance-readonly-props}

Assigning a `{readonly value: T}` to `{value: T}` would let a caller drop the read-only constraint and mutate the underlying object.

```js flow-check
function f(obj: {value: number}) {
  obj.value = 99;
}
const o: {readonly value: number} = {value: 1};
f(o); // ERROR — [incompatible-variance]
```

```ts
// TypeScript allows it; the mutation through `f` succeeds at runtime.
function f(obj: {value: number}) { obj.value = 99; }
const o: {readonly value: number} = {value: 1};
f(o);
```

Flow treats `readonly` / `writeonly` as load-bearing for static safety; TypeScript enforces `readonly` at direct write sites, but assignability can drop the readonly constraint.

The fix is to also mark the target read-only (`{readonly value: number}`) — once `f` declares it won't mutate, dropping the constraint is no longer at issue and the call succeeds. If `f` genuinely needs to mutate, the caller has to provide a mutable source instead.

```js flow-check
function f(obj: {readonly value: number}) {}
const o: {readonly value: number} = {value: 1};
f(o); // OK
```

#### Mutable arrays are invariant in Flow, covariant in TS {#toc-variance-arrays}

`Array<number>` is not assignable to `Array<number | string>` in Flow — it would let an `[0] = "oh no"` corrupt the source.

```js flow-check
function f(a: Array<number | string>) {}
const xs: Array<number> = [1, 2, 3];
f(xs); // ERROR
```

```ts
// TypeScript allows it.
function f(a: Array<number | string>) {}
const xs: number[] = [1, 2, 3];
f(xs);
```

The fix is to make the target a `ReadonlyArray<T>` — removing the possibility of mutation through `a` is what makes the widening safe. `ReadonlyArray` exists in Flow precisely *because* the mutable form is invariant — a fact often missed when reaching for the covariant TS pattern.

```js flow-check
function f(a: ReadonlyArray<number | string>) {}
const xs: Array<number> = [1, 2, 3];
f(xs); // OK
```

#### Generic type arguments are invariant by default {#toc-variance-generics}

Flow defaults generic parameters to invariance and asks the user to opt into co/contravariance with `out T` / `in T`. TypeScript infers variance from usage and preserves compatibility-oriented exceptions, which can leave read-write fields with weaker static guarantees than the Flow default.

```js flow-check
class C<T> { x: T; constructor(x: T) { this.x = x; } }
function f(c: C<number | string>) {}
const c: C<number> = new C(1);
f(c); // ERROR
```

```ts
// TypeScript allows this.
class C<T> { x: T; constructor(x: T) { this.x = x; } }
function f(c: C<number | string>) {}
const c: C<number> = new C(1);
f(c);
```

When writing a Flow generic in this shape: either the field is genuinely read-only (mark it `readonly` and the parameter `out`) or it is not, in which case Flow's invariance is correct.

#### Method parameters are contravariant in Flow but bivariant in TS {#toc-variance-methods}

A `{compare(x: number, y: number): number}` is not a subtype of `{compare(x: number | string, y: number | string): number}` in Flow; TypeScript treats it as one. In TypeScript, function-typed *fields* are contravariant but methods stay bivariant — this asymmetry is itself a TS-only wrinkle.

In Flow, both forms reject the widening, but for different reasons and with different error messages. Method shorthand fails *contravariance* (`[incompatible-type]` "the first parameter: number is incompatible with string") — function inputs flip variance, so widening them is unsound.

Switching from method shorthand to a mutable function field makes the check stricter rather than looser: the property itself is now mutable, so the error becomes *invariance* (the property is invariantly typed), which blocks the *opposite* (safe) direction too. Adding `readonly compare` restores that safe direction (a `Wider`-typed value flowing into a `NumNum` slot) by making the property covariant, but it does not fix the example above — function-input contravariance is still what blocks widening the inputs, and the only way to accept wider inputs is to declare `compare` with those wider inputs to begin with.

```js flow-check
type NumNum = {compare(x: number, y: number): number};
type Wider = {compare(x: number | string, y: number | string): number};
function f(w: Wider) {}
const nn: NumNum = {compare(x, y) { return x - y; }};
f(nn); // ERROR
```

```ts
// TypeScript accepts this under bivariance — `nn.compare("oh", "no")`
// then attempts string subtraction at runtime.
type NumNum = {compare(x: number, y: number): number};
type Wider = {compare(x: number | string, y: number | string): number};
function f(w: Wider) {}
const nn: NumNum = {compare(x, y) { return x - y; }};
f(nn);
```

TypeScript's bivariance hole (accepts the unsound direction too) is invisible at the call site, which can make Flow look "stricter for no reason" if you are coming from TypeScript — except the strictness is exactly what stops `nn.compare("oh", "no")` from doing string subtraction at runtime.

See the [variance docs](./lang/variance.md) and the [subtyping docs](./lang/depth-subtyping.md) for the full mechanics.

#### `this` type is restricted to output positions {#toc-this-variance}

The `this` type — used for fluent APIs and polymorphic method receivers — is constrained more tightly in Flow than in TypeScript. Output positions (return types) work in both languages: a method declared `m(): this` preserves the subclass type through fluent chains in Flow just as in TS — `new SubBuilder().add(1).extra()` keeps its `SubBuilder` type. Where Flow diverges is input and invariant positions. Using `this` as a parameter type or as a mutable field type errors with `[incompatible-variance]` "Cannot use `this` in an input position because `this` is expected to occur only in output positions." TypeScript accepts both freely. The rule falls out of the same variance model that makes [mutable object properties](#toc-variance-mutable-props) and [mutable arrays](#toc-variance-arrays) invariant — a writable slot typed `this` would let a caller stash a `Builder` into a `SubBuilder`-shaped field.

```js flow-check
class Builder {
  add(x: number): this { return this; } // OK — output position
  takesSelf(other: this): void {}       // ERROR — input position
  parent: this | null = null;           // ERROR — invariant field
}
```

The rewrite when you hit this is to name the class explicitly in the input/field position (`other: Builder`, `parent: Builder | null`) and accept the loss of subclass-preservation at that slot, or to make the field `readonly` so the position becomes covariant.

### Refinement and module-level validation {#toc-validation}

How Flow validates the body of type guards, when refinements are invalidated by intervening code, and the validation Flow performs at module boundaries (annotation requirements and the value/type seam).

#### User-defined type guard bodies are validated {#toc-type-guard-validation}

TypeScript checks the *signature* of an `x is T` predicate — it requires the predicate type to be assignable to the parameter type, so `function f(x: string): x is number` is rejected at declaration. But TypeScript does **not** check that the function body actually implements the claimed refinement. The body is trusted, so the following type-checks in TypeScript even though the body has nothing to do with `number`, and any caller relying on this guard will be lied to:

```ts
// TypeScript accepts this.
function isNumber(x: unknown): x is number {
  return typeof x === "boolean";
}
```

Flow validates the body of a type-guard function in **both directions**, and adds a separate rule about parameter writes. Each direction surfaces with its own diagnostic.

**Positive direction (`[incompatible-type-guard]`).** At every `return` expression, the type of the refined parameter must be a subtype of the guard type. So the equivalent of the TypeScript example above is rejected:

```js flow-check
function isNumber(x: unknown): x is number {
  return typeof x === "boolean"; // ERROR
}
```

**Negative direction (`[incompatible-type-guard]`).** When the predicate returns `false`, the negation must completely refine away the guard type from the parameter — otherwise a caller could see a value that *should* have been excluded. A predicate typed as `x is A` that actually checks `x instanceof B` (a strict subtype) is rejected for this reason:

```js flow-check
class A {}
class B extends A {}
function isA(x: unknown): x is A {
  return x instanceof B; // ERROR — negation does not refine `A` away
}
```

The diagnostic explicitly suggests the escape hatch: "Consider using a one-sided type-guard (`implies x is T`)." [One-sided guards](./types/type-guards.md) (`implies x is T`) skip exactly this negation check — they refine the parameter to `T` when the function returns `true` and leave it unchanged when it returns `false`, which is the right shape when only the positive direction holds.

**Parameter writes (`[function-predicate]`).** The refined parameter cannot be reassigned along the path to a `return`. A direct write triggers "at this return point it is written to":

```js flow-check
function isNumber(x: unknown): x is number {
  x = 1;
  return typeof x === "number"; // ERROR
}
```

A write via a captured closure triggers "`x` is reassigned" — but only if the closure is actually called between the original parameter and the `return`. Defining the closure without calling it is fine:

```js flow-check
function isNumber(x: unknown): x is number { // ERROR — `x` is reassigned via the `reset()` call below
  const reset = () => { x = 1; };
  reset();
  return typeof x === "number";
}
```

```js flow-check
function isNumber(x: unknown): x is number {
  const reset = () => { x = 1; }; // OK — never invoked
  return typeof x === "number";
}
```

See the [type guards docs](./types/type-guards.md#toc-consistency-checks-of-type-guard-functions) for the full consistency rules. When only the positive direction of the predicate holds — so the negation check would (correctly) reject the guard — the Flow-only [one-sided type guard](#toc-one-sided-guards) form `implies x is T` is the intended escape hatch.

#### Refinement invalidation rules differ {#toc-refinement-invalidation}

Both Flow and TypeScript narrow types via `typeof`, `instanceof`, equality, type guards, etc. — but the rules for when a refinement is dropped diverge in ways that have no syntactic signal. Flow invalidates a refinement when intervening code could have changed the underlying value at that storage location:

- A write to the refined binding or property (`x = ...`, `obj.k = ...`).
- A refinement on an object property where the property is reachable through aliasing or could be mutated by a callee.
- A refinement on a binding captured by a closure that an intervening call could invoke.

A bare call to a function that does not visibly touch the refined location does **not** by itself drop a refinement on a local — that is the most common over-correction. TypeScript's narrowing has its own (also non-trivial) invalidation model that does not agree with Flow's in detail; the same code may type-check in TS and not in Flow, or vice versa. See [refinement invalidations](./lang/refinements.md#toc-refinement-invalidations) for the full rule set.

```js flow-check
declare function sideEffect(): void;

function localCase(x: ?number) {
  if (x != null) {
    sideEffect();        // bare call does NOT drop the refinement on a local
    const a: number = x; // OK
  }
}

function propertyCase(obj: {x: ?number}) {
  if (obj.x != null) {
    sideEffect();            // bare call DROPS the refinement on a property
    const a: number = obj.x; // ERROR — callee could have mutated `obj.x`
  }
}

function writeCase(x: ?number) {
  if (x != null) {
    x = null;
    const a: number = x; // ERROR — direct write invalidates the refinement
  }
}
```

The standard fix for the property case is to extract the refined value to a local before any intervening code — once it's a local, the bare-call exemption above applies and the refinement survives. The write case is fixed by not reassigning the refined binding; use a separate local for the new value instead.

```js flow-check
declare function sideEffect(): void;

function propertyCaseFixed(obj: {x: ?number}) {
  const {x} = obj;
  if (x != null) {
    sideEffect();
    const a: number = x; // OK — local refinement survives the call
  }
}
```

#### Annotations are required at module boundaries {#toc-annotations-boundaries}

Flow [requires annotations](./lang/annotation-requirement.md) on function parameters, exports, and other key boundaries, and reports `[signature-verification-failure]` if a module's exports cannot be typed from annotations alone. If you're used to leaving exports unannotated and letting the typechecker infer them across modules, that will not work in Flow — the annotations have to be there.

This is a deliberate design choice that enables Flow to scale to repositories with millions of files. Because each module's exports are fully described by its annotations, Flow can extract a "typed interface" for the module without analyzing the module body, then typecheck every other module against that interface in parallel.

See the [annotation requirement docs](./lang/annotation-requirement.md) and the [Module Exports](./lang/annotation-requirement.md#toc-module-exports) subsection for full mechanics.

```js
// ERROR — return type inferred, not annotated.
export function getUser(id: string) { // [signature-verification-failure]
  return {id, name: 'Alice', age: 30};
}

// OK — annotate the return so the module's typed interface is self-contained.
export function getUser(id: string): {id: string, name: string, age: number} {
  return {id, name: 'Alice', age: 30};
}
```

#### Type-only bindings cannot cross the value/type seam {#toc-type-export-validation}

TypeScript's `import type` and `export type` are, by default, erasable annotations — the underlying classification happens at use site, so a value-position `import {Foo}` of a type-only export silently resolves as a type. (Under `--verbatimModuleSyntax`, TS does require `import type` at the import site and errors otherwise — but that's an opt-in mode, not the default.) Flow validates the value/type kind at the import and export site unconditionally, with two distinct diagnostics:

- A value-position `import {Foo}` from a module that only `export type`d `Foo` errors `[import-type-as-value]` "Cannot import the type `Foo` as a value. Use `import type` instead."
- A value-position `export {Foo}` where `Foo` is a type-only binding in the current module errors `[type-as-value]` "Cannot use type `Foo` as a value. Types are erased and don't exist at runtime."

The fix in both cases is the explicit type form: `import type {Foo}` or `export type {Foo}`. This is load-bearing for Flow's signature-extraction model — the typed interface a module exposes has to be unambiguous about which exports are types and which are values, since dependents are checked against that interface in parallel without analyzing the module body.

```js
// mod.js
export type Foo = {x: number};
```

```js
// consumer.js
import {Foo} from './mod'; // ERROR — [import-type-as-value]
import type {Foo as FooType} from './mod'; // OK
```

### Explicit type controls {#toc-explicit-controls}

Three places where TypeScript accepts a looser surface spelling and Flow requires the explicit form: `as` casts, error suppressions, and generic-argument lists.

#### `as` casts are stricter in Flow {#toc-as-casts}

Flow's `as` only widens or asserts (e.g. `42 as number`, `42 as 42`), and rejects unsafe downcasts at the type level — `{foo: 1} as {foo: number, bar: string}` is a Flow error, not a cast. TypeScript's `as` accepts any cast where the two types are assignable in *either* direction, which lets it silently approve unsafe downcasts. The same TypeScript line type-checks even though the cast invents a `bar: string` property that doesn't exist at runtime. This permissiveness is the single biggest source of "TS code that looks like it should work in Flow but doesn't."

```js flow-check
const v = {foo: 1} as {foo: number, bar: string}; // ERROR
```

```ts
// TypeScript accepts the same line because `as` permits assignability
// in either direction.
const v = {foo: 1} as {foo: number, bar: string};
```

Flow's escape hatch for a forced cast is the explicit `value as any as T` two-step; TypeScript's idiom is `value as unknown as T`.

#### Error suppressions are coded and scoped {#toc-suppressions}

Flow's `$FlowFixMe[code]` (and `$FlowExpectedError[code]`) suppresses **only** the named error code at that location — any other error on the same line still surfaces, and the suppression itself errors as unused if the targeted code doesn't fire. TypeScript's `// @ts-ignore` silences every error on the next line indiscriminately, and `// @ts-expect-error` similarly silences everything but errors when nothing was suppressed. The Flow form is strictly more granular and keeps suppression debt auditable. See the [errors docs](./errors/index.md).

```js flow-check
declare function takesNumber(n: number): void;
// $FlowFixMe[incompatible-type] - intentional for demo
takesNumber("not a number");
```

#### Generic type arguments cannot be omitted {#toc-generic-default-omission}

TypeScript lets you write a generic type unparameterized when every type parameter has a default — `Foo<T = string>` followed by `type A = Foo` resolves `A` to `Foo<string>`. Flow rejects the bare form and requires an explicit type-argument list (or an empty `<>` to fall back on defaults), reporting `[missing-type-arg]` "Cannot use `Foo` without 0-1 type arguments."

```js flow-check
type Foo<T = string> = {x: T};
type A = Foo;   // ERROR — [missing-type-arg]
type B = Foo<>; // OK    — uses the default `T = string`
```

The rewrite is mechanical: `Foo` → `Foo<>` for all-defaulted generics, or supply the args explicitly. The rationale for the explicit form is that Flow reserves the bare name `Foo` for the *type constructor itself* (so that operations on the type — type-level functions and the like — can take the unapplied form as input), rather than overloading it as shorthand for an applied instantiation.

## Flow-only concepts with no built-in TypeScript analogue {#toc-flow-only}

These are the constructs Flow has built that TypeScript hasn't built into the language or typechecker — most of them because the underlying problem (React component shapes, hook rules, render constraints, exhaustive pattern matching, nominal abstraction across module boundaries, runtime-and-type-level enums) is one TypeScript leaves to framework/library patterns, lint rules, or user code. There is no built-in TypeScript analogue to translate from, only a Flow concept to learn fresh.

### `component` syntax {#toc-component-syntax}

Flow ships [first-class `component` syntax](./react/component-syntax.md) for declaring React components with named props, optional ref, and render-type support. The compiler enforces rules that TypeScript's function-type component model does not encode as syntax — return type fixed to `React.Node`, no `this`, no nested components, ref parameters in their dedicated position. There is no built-in TypeScript equivalent; TypeScript models the equivalent shape with function types and `forwardRef`.

```js flow-check
import * as React from 'react';

component Greeting(name: string, age?: number) {
  return <div>Hello, {name}{age != null ? `, age ${age}` : ''}</div>;
}

const _ = <Greeting name="Alice" age={30} />;
```

### `hook` syntax {#toc-hook-syntax}

[`hook` syntax](./react/hook-syntax.md) is a first-class Flow keyword for declaring React hooks. Flow uses the keyword to enforce the [Rules of React](https://react.dev/reference/rules) at the type level on hook call sites. TypeScript has no equivalent — hook rules in TS are enforced by ESLint (`eslint-plugin-react-hooks`), which operates on AST patterns without type information or whole-program analysis.

```js flow-check
import {useState} from 'react';

hook useToggle(initial: boolean): [boolean, () => void] {
  const [value, setValue] = useState(initial);
  return [value, () => setValue(v => !v)];
}
```

### `renders` types {#toc-renders-types}

[Render types](./react/render-types.md) (`renders`, `renders?`, `renders*`) constrain what a component is allowed to render — for example, "a `Menu` only renders `MenuItem`s." There is no built-in TypeScript analogue.

```js flow-check
import * as React from 'react';

component MenuItem() {
  return <li />;
}

component Menu(children: renders MenuItem) {
  return <ul>{children}</ul>;
}

const _ = <Menu><MenuItem /></Menu>;
```

### `match` expressions and statements {#toc-match}

Flow has [`match` expressions and statements](./match/index.md) for pattern matching with structural patterns, guards, and exhaustiveness checking. TypeScript has no `match`; the closest analogue for the statement form is a hand-coded discriminated-union `switch` with an `assertNever` fallthrough. The **expression** form has no direct TS analogue at all, because `switch` is statement-only in JavaScript — TS users typically reach for nested ternaries or an IIFE wrapping a `switch`, both of which lose the structural patterns, guards, and exhaustiveness checks `match` provides.

```js flow-check
type Shape =
  | {kind: 'circle', radius: number}
  | {kind: 'square', side: number};

declare const s: Shape;

const area: number = match (s) {
  {kind: 'circle', const radius} => Math.PI * radius * radius,
  {kind: 'square', const side} => side * side,
};
```

### Opaque types {#toc-opaque-types}

[Opaque type aliases](./types/opaque-types.md) hide their underlying type outside the file in which they are defined, enforcing nominal abstraction across module boundaries. TypeScript has no native equivalent; the common idiom there is "branded types" using intersection with a (typically `unique symbol`-keyed) marker property, which is a userland pattern rather than a language feature.

The boundary the brand idiom enforces is weaker than Flow's file-scoped abstraction along two axes. First, a single `as` cast is enough to forge a branded value — `"abc" as UserId` type-checks in TypeScript because the source (`string`) and the target (`string & {readonly [brand]: true}`) overlap on `string`, and TS only rejects an `as` cast when the two sides are disjoint. (The `as unknown as T` double-cast is the universal escape hatch.) Second, when the brand key is exposed (re-exported `unique symbol`, or a plain string key like `__brand: "UserId"`), any consumer can structurally construct a branded value directly, no cast required. Even with an unexported `unique symbol`, the `as` route remains open. Flow's opaque types, by contrast, are sealed by the module boundary itself: outside the defining file, the underlying type is not visible at all, so neither structural construction nor `as` widening can produce the opaque type from its underlying representation.

```js flow-check
opaque type UserId = number;

declare function makeUserId(n: number): UserId;
declare function lookupUser(id: UserId): string;

const id: UserId = makeUserId(42);
lookupUser(id); // OK
// In another file, `42` is not a `UserId` and a `UserId` is not a `number`.
// Inside this file (where the underlying type is visible) the conversion is allowed:
const n: number = id;
```

### Flow Enums {#toc-flow-enums}

[Flow Enums](./enums/index.md) and TypeScript `enum` look superficially similar but are very different in detail.

| Aspect | TypeScript | Flow |
|---|---|---|
| Exhaustive `switch` | No built-in diagnostic; encoded with `never` or lint. | Built-in: `[invalid-exhaustive-check]` if a member is forgotten. |
| Implicit coercion to/from underlying primitive | Permits number → number-enum slots (except non-matching literals) and freely coerces enums to numbers. | Blocked both directions; use `.cast()` to convert in, and `value as <representation type>` (for example `as string` / `as number`) to convert out. |
| Default member values | Number enums auto-number from `0`. | Number-enum members must be explicitly initialized (`[invalid-enum]`); string enums default to mirroring member names. |
| Re-declaration | Allowed; can collide with default values silently. | `[name-already-bound]`. |
| Reverse mapping | Number enums get a runtime reverse-map; string enums error on the same access. | `.getName(value)` works for both number and string enums. |
| Iterating members | `for...in` over a number enum produces both numeric keys *and* member names. | `Status.members()` returns just the values. |
| Symbol enums | None. | Supported (`enum X of symbol { ... }`). |
| Definition restrictions | Permits heterogeneous initializers, non-literal initializers, and lowercase-leading member names. | All three error. |

A few of these have rationales worth knowing:

- The default-value rule exists because adding or removing a member from the middle of an auto-numbered enum silently renumbers everything after it, which is a serialization/logging hazard.
- The TS string-enum reverse-mapping error is structural: `StatusStr.Off` has literal type `"off"` (the value), not `"Off"` (the key), so `StatusStr[StatusStr.Off]` resolves to a non-existent `StatusStr["off"]`.
- TS `for...in` over a 3-member number enum produces `[ '0', '1', '2', 'Active', 'Paused', 'Off' ]` — both halves of the runtime reverse-map are enumerable.
- Lowercase-leading member names are reserved because Flow Enums expose lowercase methods like `.cast` and `.members`.

```js flow-check
enum Status {
  Active,
  Paused,
  Off,
}

declare const st: Status;

let label: string;
switch (st) {
  case Status.Active: label = 'on'; break;
  case Status.Paused: label = 'wait'; break;
  case Status.Off: label = 'off'; break;
  // Exhaustive — removing a case here errors.
}
```

See the [Flow Enums docs](./enums/index.md) for full mechanics.

### One-sided type guards (`implies`) {#toc-one-sided-guards}

A predicate function whose return type is `implies param is T` refines the parameter to `T` only when the function returns `true`, and leaves it unchanged when the function returns `false`. This is the escape hatch for the [body-validation rule](#toc-type-guard-validation) covered above when only the positive direction holds. TypeScript has no equivalent.

```js flow-check
declare function looksLikeFoo(x: unknown): implies x is {foo: string, ...};

declare const v: unknown;
if (looksLikeFoo(v)) {
  v.foo as string; // refined to {foo: string, ...}
}
// In the else branch, `v` stays `unknown` — that's the "one-sided" property.
```

### `import typeof` {#toc-import-typeof}

`import typeof` is the Flow-only form. `import type Foo from './m'` (which Flow shares with TypeScript — both languages support it) brings in the type of a *type* export; `import typeof Foo from './m'` is Flow-specific and brings in the type *of a value* export so it can be used as a type annotation.

TypeScript's nearest analogue for `import typeof` is `typeof import('./m')`, but the binding shape differs: TypeScript produces the namespace shape and is usually combined with indexed access (`typeof import('./m')['Foo']`), while `import typeof Foo from './m'` binds a single value's type as a top-level type binding.

```js flow-check
// `import type` imports a *type* declaration — `Node` is a type export, so
// it can be used directly as a type annotation:
import type {Node as ReactNode} from 'react';
const node: ReactNode = "hello, world";

// `import typeof` imports the *type of a value*. `useState` is a value, but
// after `import typeof` the name `useState` is usable as a type. Generic
// values stay generic — `useState<number>` instantiates the underlying
// function type to its `number` specialization, so the parameter below is
// callable as a `number`-typed `useState`:
import typeof {useState} from 'react';
hook useCounter(useStateNum: useState<number>): number {
  const [count, setCount] = useStateNum(0);
  setCount(c => c + 1);
  return count;
}
```

### Flow-only utility types {#toc-flow-only-utilities}

A handful of [utility types](./types/utilities.md) have no TypeScript counterpart. The closest TS spellings — where one exists — are noted below; the rest have no native TS form and are typically encoded with userland patterns.

- [`Class<T>`](./types/utilities.md#toc-class) — the type of the class constructor for an instance type `T`. No TS native form; the usual TS encoding is `new (...args: any[]) => T` or `typeof T` for a specific class.
- [`Values<T>`](./types/utilities.md#toc-values) — the union of value types of `T`'s properties. TS spelling is the indexed access `T[keyof T]`.
- [`$KeyMirror<O>`](./types/utilities.md#toc-keymirror) — an object type whose property values are string-literal types mirroring their keys. No TS native form.
- [`$Exports<'mod'>`](./types/utilities.md#toc-exports) — the type of a module's exports given a path string. TS's nearest analogue is `typeof import('mod')`, with a different shape.
- [`StringPrefix<P>` / `StringSuffix<S>`](./types/utilities.md#stringprefix-and-stringsuffix) — strings constrained to a literal prefix or suffix. The TS analogue is template literal types (`` `${P}${string}` `` / `` `${string}${S}` ``), which Flow does not yet have — see [Coming soon](#toc-coming-soon).
- [`$Exact<T>`](./types/utilities.md#toc-exact) — promotes an inexact object type to exact. Discouraged in new code; object types are exact by default, so this is only useful when wrapping an inexact alias.

### Flow-only syntactic forms {#toc-flow-only-syntax}

A handful of Flow type-annotation forms have no TypeScript spelling — `tsc` rejects them at parse time. They are alternate syntax for existing Flow concepts.

- **Inline `interface` type annotation** — `type T = interface { foo: number }`. Lets an interface appear inside a type expression instead of as a top-level declaration. TypeScript requires a separate `interface I { ... }` statement.
- **Optional indexed access type** — `Obj?.['prop']` mirrors the runtime `?.` operator at the type level: if `Obj` is nullish, the result is `void`; otherwise it is `Obj['prop']`. TypeScript has no type-level `?.`.
- **Anonymous function-type parameters** — `type F = string => void`. Flow lets you omit the parameter name when it carries no information; TypeScript requires `(x: string) => void`.
- **Anonymous indexer parameters** — `type O = {[string]: number}`. Same shape: Flow omits the index-key name when it isn't referenced; TypeScript requires `{[k: string]: number}`.

```js flow-check
type Inline = interface { foo: number };
type Opt = ?{foo: number};
type Pulled = Opt?.['foo']; // number | void
type Fn = string => void;
type Dict = {[string]: number};
```

### Relay / GraphQL integration {#toc-relay-integration}

Setting `relay_integration=true` in `[options]` makes Flow natively understand `graphql` tagged template literals and infer their types from the Relay compiler's emitted artifacts, so users can omit explicit type parameters on `useFragment`, `usePreloadedQuery`, etc. Companion options: `relay_integration.esmodules` (resolve artifacts as ES module default exports rather than CommonJS) and `relay_integration.excludes` (per-directory opt-out). See the [docs for this option](./config/options.md#toc-relay-integration).

TypeScript has no typechecker-level equivalent. TypeScript users either pass the generated type explicitly (`useFragment<MyFragment$key>(...)`), use a TypeScript *language service plugin* for editor hints (not typechecking), or use document-node patterns like `graphql-typed-document-node` / `gql.tada` that require explicit imports of generated types.

## TypeScript-only features that do not exist in Flow {#toc-ts-only}

These are TypeScript features that have no Flow equivalent today. Some Flow has deliberately not adopted, either because they overlap a Flow feature with different (usually more conservative) defaults or because they introduce footguns Flow's design avoids. Others are simply not implemented yet — see the separate [Coming soon](#toc-coming-soon) section for features that are in-flight. Reaching for any of the items below in Flow code won't work, and in some cases the TypeScript syntax will parse, so the failure shows up later than expected.

### TS-only syntactic forms {#toc-ts-only-syntax}

A handful of TS surface-syntax forms have no Flow spelling, but the *concept* is available in Flow under a different name. Flow rejects the TS form at parse/type-check time with a `[unsupported-syntax]` diagnostic that points at the Flow rewrite directly.

- **Angle-bracket type assertion** — TS `<T>x` → Flow `x as T`.
- **Optional unlabeled tuple elements** — TS `[number, string?]` → Flow `[a: number, b?: string]`. Flow requires the labeled variant for optional elements.
- **`readonly` type operator on tuples** — TS `readonly [T, S]` → Flow `Readonly<[T, S]>`.
- **`readonly` type operator on array shorthand** — TS `readonly T[]` → Flow `ReadonlyArray<T>`.

Note that `readonly` as a *property modifier* (`{readonly x: T}`) and on type parameters (`out T`) works the same way in both languages — see [Variance keywords](#toc-variance-keywords). The two `readonly` forms above are uses of `readonly` as a *type operator* (a prefix on a structural type), which is a TS-only spelling — Flow uses the wrapper utility instead.

```js flow-check
type A = readonly [number, string]; // ERROR — use Readonly<[number, string]>
type B = readonly number[];         // ERROR — use ReadonlyArray<number>
type C = [number, string?];         // ERROR — use the labeled form below
type OkA = Readonly<[number, string]>;
type OkB = ReadonlyArray<number>;
type OkC = [a: number, b?: string];
```

### Decorators {#toc-decorators}

Flow has no decorator support in any mode. TypeScript supports two incompatible modes: **stage-3 decorators** (the default, with a context-object parameter) and **legacy decorators** (under `--experimentalDecorators`, with the old `(target, key)` signature).

### TypeScript class syntax extensions {#toc-class-extensions}

TypeScript has several class-syntax extensions Flow has deliberately not adopted, asking users to write the equivalent JS instead.

**Parameter properties** (`constructor(public x: number)`) — a TS-only shorthand that emits runtime code: it auto-declares the field and assigns it from the constructor argument. Flow's diagnostic: "Flow does not support TypeScript parameter properties. To fix, declare the property in the class body and assign it in the constructor."

```js flow-check
class C {
  constructor(public x: number) {} // ERROR — [unsupported-syntax]
}
```

**`public` / `protected` / `private` access modifiers** — TS-checked access control. These are type-checker-only in TypeScript (the field is still publicly accessible at runtime), so dropping them is safe.

```js flow-check
class C {
  public a: number = 1;    // ERROR — drop the modifier
  protected b: number = 2; // ERROR — drop the modifier
  private c: number = 3;   // ERROR — use `#c` instead
}
```

The `private` rewrite lands at a different runtime shape from the TS form: ECMAScript `#private` fields are nominally private at runtime, while TS `private` is erased. Flow tracks `#private` nominally as part of the class identity.

**`accessor` auto-accessors** (`class C { accessor x: T = init }`) — a stage-3 proposal that desugars to a paired getter/setter backed by a private field. Flow does not parse the form. Write the getter and setter explicitly with a `#private` backing field, or use a plain field if no accessor wrapping is needed.

### Runtime `namespace` blocks {#toc-namespace-blocks}

No source-level `namespace { ... }` blocks. Flow has [`declare namespace`](./libdefs/creation.md) for ambient declarations inside libdefs, but not source-level namespace blocks that produce runtime values.

### `const enum` {#toc-const-enum}

No equivalent. Flow Enums are a runtime construct by design and do not have an inlined-at-compile-time mode. However, their restrictions (literal-only values, no redeclaration, no default number values) make it easier for your build system to do inlining.

### `infer extends` {#toc-infer-extends}

No equivalent. Flow's `infer` exists in conditional types but does not support TypeScript's `infer T extends Bound` constraint form. Restructure the conditional or move the bound check elsewhere.

### Assertion functions {#toc-assertion-functions}

TypeScript's `asserts x is T` return type declares a function that throws when the assertion fails and refines the parameter to `T` unconditionally after the call returns — a different shape from a [type guard](./types/type-guards.md), which returns a boolean and refines only inside an `if`/`else`. Flow has type guards (`x is T`) but no `asserts x is T` form — the assertion-function syntax errors with `[unsupported-syntax]` "Type guard assertions are not yet supported."

```js flow-check
function specificAssert(arg: unknown): asserts arg is string { // ERROR — [unsupported-syntax]
  if (typeof arg !== 'string') {
    throw new Error();
  }
}
```

The closest Flow equivalent is a type guard combined with an explicit `throw` at the call site: `function isStr(x: unknown): x is string { ... }` then `if (!isStr(x)) throw new Error();`.

### Expressions with type arguments {#toc-expression-type-args}

TypeScript accepts type arguments on a *value expression* — `Foo<string>` as a standalone expression specializes the generic and can be bound to a name. Flow does not parse the form and errors at the closing `>` with a `ParseError`.

```ts
// TypeScript:
declare class Foo<T> {
  value: T;
}
const StringFoo = Foo<string>;
```

The Flow rewrite is to supply the type arguments at the instantiation or call site (`new Foo<string>()`, `f<string>(x)`) rather than naming a pre-specialized binding.

### Sentinel refinement through destructured values {#toc-sentinel-destructure}

TypeScript narrows destructured properties of a discriminated union together: refining the sentinel binding also refines the other bindings extracted in the same destructuring.

```ts
// TypeScript:
type Shape =
  | {kind: 'circle', value: number}
  | {kind: 'square', value: string};

declare const s: Shape;
const {kind, value} = s;
if (kind === 'circle') {
  const r: number = value; // OK — TS narrows `value` based on `kind`
} else {
  const sd: string = value;
}
```

Flow refines sentinel-tagged unions through the *original* value (`if (s.kind === 'circle') { ... s.value ... }`), but each destructured binding carries its full union type independent of the others, so the same code fails:

```js flow-check
type Shape =
  | {kind: 'circle', value: number}
  | {kind: 'square', value: string};

declare const s: Shape;
const {kind, value} = s;
if (kind === 'circle') {
  const r: number = value; // ERROR — `value` keeps its full `number | string` type
}
```

The Flow rewrite is to refine through the original value rather than destructure.

### User-side module augmentation {#toc-module-augmentation}

No equivalent at the source level. TypeScript users routinely re-open third-party modules from source code via `declare module 'name' { ... }` to add types. Flow's `declare module` is only used inside *libdefs* under `flow-typed/`, not from arbitrary source files.

## Coming soon {#toc-coming-soon}

TypeScript features that are in-progress on the Flow side and will be released in the future. Each of these is TS-only *for now*; the entries in [TypeScript-only features](#toc-ts-only) above are the ones Flow has no in-flight plans to add.

- `satisfies` expression — validates an expression against a type without widening the inferred type.
- Mapped type modifiers — optionality removal `-?`, variance removal `-readonly`, and `as` key remapping.
- Template literal types — e.g. `` `${'a' | 'b'}-${'x' | 'y'}` ``.
- Additional TS utility types — `ConstructorParameters`, `InstanceType`, `ThisType`, and the intrinsic string-manipulation types `Uppercase`, `Lowercase`, `Capitalize`, `Uncapitalize`.
- `override` keyword on class members.
- Abstract classes and methods.
- Constructor types — `type Ctor = new (x: number) => R`.
- Symbol-keyed properties and `unique symbol`.
- Inline `import()` type expression — `type A = import('./m').A`.
- `import X = require('foo')` and `export = X` — CommonJS-style import and export bindings.

## Syntax convergence with TypeScript {#toc-convergence}

This table maps legacy Flow forms to their modern Flow replacements. Some rows are syntax renames; others are older utilities or features with TS-aligned equivalents. New code should use the right-hand form.

| Legacy Flow | Modern Flow (TS-aligned) |
|---|---|
| `mixed` | `unknown` |
| `$Keys<T>` | `keyof T` |
| `$ReadOnly<T>` | `Readonly<T>` |
| `$NonMaybeType<T>` | `NonNullable<T>` |
| `$ReadOnlyArray<T>` | `ReadonlyArray<T>` |
| `<T: Bound>` | `<T extends Bound>` |
| `(x: T)` cast | `x as T` |
| `{\| a: number \|}` exact | `{a: number}` (exact is the default) |
| `+foo` / `-foo` property variance | `readonly foo` / `writeonly foo` (`writeonly` is Flow-specific) |
| `+T` / `-T` type parameter variance | `out T` / `in T` |
| `%checks` predicate functions | user-defined [type guards](./types/type-guards.md) (`function isString(x: unknown): x is string`) |
| `$ObjMap<O, F>` / `$ObjMapi<O, F>` / `$TupleMap<T, F>` / `$TupleMapi<T, F>` | [mapped types](./types/mapped-types.md) (`{[K in keyof O]: ...}`) with the function body inlined |
| `$PropertyType<T, K>` / `$ElementType<T, K>` | [indexed access](./types/indexed-access.md) (`T[K]`) |
| `$Call<F, ...Args>` | [`ReturnType<F>`](./types/utilities.md) plus indexed access, or a [conditional type](./types/conditional.md) with `infer` |
| `$Diff<A, B>` / `$Rest<A, B>` | typically [`Omit<A, keyof B>`](./types/utilities.md), case by case (not always semantically identical) |

For the full picture see [Modernizing Legacy Flow Syntax](./modernizing-legacy-syntax.md).

## Config options aligned with TypeScript {#toc-shared-options}

A few Flow [`.flowconfig`](./config/index.md) `[options]` toggles correspond directly to TypeScript `compilerOptions` strictness flags — same semantics, but different defaults. In the TypeScript `strict` baseline used on this page, `useUnknownInCatchVariables` is enabled through `strict`, while `noUncheckedIndexedAccess` is not part of `strict` and stays opt-in. Flow has no `strict` options umbrella — both flags are opt-in individually and default to `false`, so porting from a TS project with `strict` enabled means turning `use_unknown_in_catch_variables` on to match.

| TypeScript option | Flow option | Description |
| --- | --- | --- |
| `noUncheckedIndexedAccess` | `no_unchecked_indexed_access` | Indexed access through an array or dictionary widens the result type with `undefined` (Flow: `void`), so reading `arr[i]` or `dict[k]` returns `T \| void` instead of `T` and forces the caller to refine before use. Tuple access with a *literal* index is unaffected in both languages. See [docs](./config/options.md#toc-no-unchecked-indexed-access) for more. |
| `useUnknownInCatchVariables` | `use_unknown_in_catch_variables` | Changes the default type of an un-annotated `catch` binding from `any` to `unknown`. The caller has to narrow the value (`instanceof Error`, `typeof e === 'string'`, …) before using it. See [docs](./config/options.md#toc-use-unknown-in-catch-variables) for more. |

## Typing external code {#toc-declaration-mechanisms}

TypeScript's `.d.ts` files cover two distinct concerns: typing third-party npm packages and typing first-party (or vendored) code that must remain plain JavaScript. Flow splits these into two mechanisms.

| TypeScript use case | Flow mechanism | Placement and resolution |
|---|---|---|
| Third-party package declarations, including `@types/*` packages and package-level `.d.ts` files. | [**Library definitions (libdefs)**](./libdefs/index.md). | Plain `.js` files in `flow-typed/` that usually name packages with `declare module 'pkg' { ... }`. |
| A sibling declaration file next to a JavaScript implementation. | [**Declaration files**](./declarations/index.md). | Colocated `.js.flow` or `.json.flow` files (for example, `Misc.js.flow` next to `Misc.js`) that shadow the implementation file. |
| Source-level module augmentation with `declare module 'pkg' { ... }` from arbitrary project files. | No source-level equivalent. | Flow's `declare module 'pkg' { ... }` form is for libdefs under `flow-typed/`, not for reopening modules from ordinary source files or colocated `.js.flow` declaration files. |

The two Flow mechanisms share much of TypeScript's ambient declaration syntax, but placement is load-bearing. `declare class`, `declare function`, `declare const`, and related forms can describe ambient values in libdefs, declaration files, or inline declarations; `declare module 'name' { ... }` is the named-package form used by libdefs. Declaration files usually describe the colocated module's exports directly, for example with `declare export ...` or `declare module.exports`.

See [User-side module augmentation](#toc-module-augmentation) for the declaration-file pattern TypeScript supports that Flow does not.

### Declaration merging is partially supported {#toc-declaration-merging}

Underpinning the merging cases below: Flow uses TypeScript's split-namespace model. Each name independently inhabits a *value* namespace and a *type* namespace, so a single identifier can be both a value and a type without colliding — `const A = 1; interface A {}` is accepted, value-side uses of `A` resolve to the const, type-side uses resolve to the interface. Constructs usable in both namespaces — classes, enums, opaque types — register once in the value namespace and the type side falls back to it.

On top of that namespace model, Flow supports the merging cases that matter most. Specifically, Flow merges:

- `interface` + `interface` — members union (first-wins on conflicts), `extends` lists concatenate, call signatures overload as intersections, type-param arities must match. Supported in both type-sig and local checking.
- `declare module` + `declare module` — exports union (first-wins on name collisions), incompatible export styles (CJS vs. ES) error, star re-exports concatenate.
- `declare class` + `interface` — interface members fold into the class (either order).
- `function` / `declare function` + `declare namespace` — namespace's type members fold into the function (either order).
- `class` / `declare class` + `declare namespace` — namespace's type members fold into the class (either order).

What Flow does *not* do is the *runtime-merging* cases — for example, arbitrary `function` + `namespace` value-side merging where the namespace contributes runtime members, and the [user-side `declare module 'name' { ... }` source-level augmentation pattern](#toc-module-augmentation).

### Generating declaration files {#toc-generating-declarations}

The mechanisms above are about declarations as *input* — typing code the typechecker can't otherwise see. The reverse direction is emitting declaration files *from* source. TypeScript handles this in the compiler itself: `tsc --declaration` emits a `.d.ts` alongside each compiled `.ts`, and `--emitDeclarationOnly` produces declarations without the corresponding `.js`. Flow has no equivalent built into the `flow` binary; the separate [`flow-api-translator`](https://www.npmjs.com/package/flow-api-translator) NPM package fills this gap, producing `.js.flow` or `.d.ts` files from a Flow source file.

## See also {#toc-see-also}

- [Glossary](./glossary.md) — carries a one-line TypeScript note on concepts that have one, and serves as a quick index when you only need to look up a single term.
- [Modernizing Legacy Flow Syntax](./modernizing-legacy-syntax.md) — the full reference for migrating Flow's legacy `$`-prefixed utilities and other older syntactic forms to their modern (often TS-aligned) equivalents.
