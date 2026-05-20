---
title: "Flow for TypeScript Users"
slug: /flow-and-typescript
description: "What transfers from TypeScript to Flow, what looks the same but means something different, what only Flow has, what only TypeScript has, and how to migrate Flow's legacy syntax."
---

Flow and TypeScript share most of the same surface syntax, much of the same vocabulary, and a large set of overlapping concepts (generics, conditional and mapped types, `keyof`, `unknown`, `Readonly`, type guards). The convergence is largely intentional — Flow's syntax has shifted to align with TypeScript's over the last several releases — so if you know TypeScript, your intuition will get you most of the way through a Flow program. React is one notable area where Flow does *not* mirror TypeScript: Flow ships its own first-class [`component`](./react/component-syntax.md), [`hook`](./react/hook-syntax.md), and [`renders`](./react/render-types.md) syntax instead of modeling components through function types, `forwardRef`, and framework/library patterns — covered in [Flow-only concepts](#toc-flow-only) below.

Where the two diverge, the divergence is usually a deliberate Flow choice in favor of stronger static guarantees. Flow rejects a number of patterns that TypeScript accepts but that can throw at runtime or leave the program with inaccurate static types; that strictness is intentional, not an oversight. The buckets below organize the divergences by failure mode so you can match a TypeScript pattern you know to its Flow equivalent (or its absence).

This page is organized in four buckets:

- [Concepts that transfer cleanly from TypeScript](#toc-transfer-cleanly).
- [Same syntax, different semantics](#toc-same-syntax) — code that compiles in TypeScript but is wrong, unsound, or rejected in Flow.
- [Flow-only concepts with no built-in TypeScript analogue](#toc-flow-only).
- [TypeScript-only features that do not exist in Flow](#toc-ts-only).

It then summarizes the syntactic convergence with TypeScript and points to the related comparison surfaces in the rest of the docs.

> **Scope note.** Flow is a typechecker only — it does not emit JavaScript and does not generate declaration files. TypeScript is both a typechecker *and* a compiler: it lowers source to JS (with `target`, `module`, `jsx`, etc.) and produces `.d.ts` declarations. Check out [tooling](./tools/babel.md) for more on setting up Flow. The Flow equivalent of TS `.d.ts` declaration files is [library definitions (libdefs)](./libdefs/creation.md) — separate `.js.flow` files that declare types for third-party modules.

## Concepts that transfer cleanly from TypeScript {#toc-transfer-cleanly}

These are the easy wins. The features below are close enough in syntax and semantics that you can reuse your TypeScript intuition more or less directly — and most of them are in this list because Flow has deliberately aligned with TypeScript's spelling over the last several releases (some, like `unknown` over `mixed` and `<T extends Bound>` over `<T: Bound>`, are recent renames of Flow's older equivalents).

- [Conditional types](./types/conditional.md) with `infer`.
- [Mapped types](./types/mapped-types.md).
- User-defined [type guards](./types/type-guards.md) of the form `param is T`.
- [`as const`](./types/const-expression.md) assertions.
- Generic bounds with `<T extends Bound>`.
- [`const` type parameters](./types/const-expression.md#const-type-parameters) — `function f<const T>(x: T): T`.
- The [`keyof T`](./types/utilities.md) operator.
- The [`unknown`](./types/unknown.md) top type.
- Array shorthand `T[]` (in addition to `Array<T>`).
- TS-aligned [utility types](./types/utilities.md): `Readonly`, `ReadonlyArray`, `ReadonlyMap`, `ReadonlySet`, `Pick`, `Omit`, `Record`, `Partial`, `Required`, `Exclude`, `Extract`, `NonNullable`, `Parameters`, `ReturnType`, `Awaited`, `ThisParameterType`, `OmitThisParameter`.
- JSX type-argument syntax at call sites — `<Box<number> value={42} />`.
- `declare const` and `declare let`.

One important caveat: `as` casts *look* like they belong here — Flow accepts the form — but Flow's `as` is checked (it only widens or asserts) while TypeScript's permits unsafe downcasts. Covered in [Same syntax, different semantics](#toc-same-syntax) below, not here.

## Same syntax, different semantics {#toc-same-syntax}

This is the bucket where Flow most often surprises a reader coming from TypeScript. The syntax compiles, the names match, but the type system enforces something different. Most of the subsections below are Flow rejecting (or correctly modeling) a pattern that TypeScript accepts but that has a runtime failure waiting in it — exactness, variance, type-guard validation, refinement invalidation, type-level object spread, and the rest of the static-safety story all show up here. A couple of items (notably the `void`/`undefined` distinction) are simpler shape mismatches without a direct static-safety angle. Either way, every subsection is a place where TypeScript-shaped code will look correct in Flow and either be rejected or accepted with different meaning.

> **Nominal vs. structural typing.** TypeScript is primarily structural — two types with the same public shape are interchangeable, with narrow nominal carve-outs (`#private` fields, `private`/`protected` modifiers, and `unique symbol`). Flow is structural for plain objects and functions, but deliberately *nominal* for [classes](./lang/nominal-structural.md), [opaque types](./types/opaque-types.md), and [Flow Enums](./enums/index.md). The reason is that identity carries real information at those boundaries — a `UserId` is not a `PostId`, a `Celsius` is not a `Fahrenheit`, two distinct classes with the same fields are different concepts. Treating identity nominally rather than structurally lets the type system catch entire categories of logic bugs (the "right shape, wrong meaning" class of mistakes) and lets users model their domain at the level of *what something is*, not just *what it looks like*. The [Classes are nominal](#toc-classes-nominal) subsection below is the concrete instance of this stance; opaque types and Flow Enums (covered later in [Flow-only concepts](#toc-flow-only)) are the others.

### Object exactness {#toc-object-exactness}

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

### Classes are nominal; the object/interface/class triangle is asymmetric {#toc-classes-nominal}

TypeScript types classes structurally — `interface`, `type`, and class instances are largely interchangeable as long as the shapes match. Flow uses [nominal typing](./lang/nominal-structural.md) for classes (two distinct classes with the same members are not interchangeable) and a one-way subtyping triangle:

- An *object type* accepts only object literals.
- An [*interface*](./types/interfaces.md) accepts both object literals and class instances.
- A *class instance* is **not** a subtype of an object type.
- An *interface*-typed value is **not** a subtype of an object type either (it might be backed by a class instance).

The reason for the asymmetry is that classes and interfaces are inherently *inexact*: a class can be subclassed and an interface can be implemented by a class that adds further properties, so an instance typed as `Foo` or `I` could always carry properties beyond what the declaration lists. That is incompatible with the [exact-by-default object type](#toc-object-exactness) rule, which forbids unlisted properties. Object literals don't have this problem because their shape is fully known at the point of construction. (The Flow docs state this directly for interfaces: "interfaces cannot be exact, as they can always have other, unknown properties" — see [interfaces](./types/interfaces.md).)

The error surfaces with two different codes depending on the target's exactness:

- Against an *exact* object type (the default), the error is `[incompatible-exact]`.
- Against an *inexact* object type (`{a: number, ...}`), the error is `[class-object-subtyping]` with text "Class instances are not subtypes of object types; consider rewriting object type as an interface."

The asymmetry is the same in both cases — only the diagnostic text differs.

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

Note that the interface-to-object-type error reuses the `[class-object-subtyping]` code and the diagnostic text "Class instances are not subtypes of object types" — even though the value is interface-typed rather than a class instance. The wording reflects the underlying reason: the interface might be backed by a class instance, so the same static-safety rule applies.

The canonical fix when you hit this in Flow is to switch the parameter type from object type to interface.

One more direction-of-travel note: TypeScript's class-structurality is *almost* total — `const c: C = {x: 1}` type-checks in TS even though `c` is annotated as a class instance. TS preserves a handful of nominal channels on top of the structural default — ECMAScript `#private` fields, the `private` / `protected` access modifiers (both of which block assignability across distinct declarations), and `unique symbol` — but they're carve-outs from an otherwise structural model. Flow's class nominalism is total: no nominal opt-in is *required* because the class identity itself is the nominal channel (Flow tracks `#private` fields nominally too — see [TypeScript class syntax extensions](#toc-class-extensions) — but it's reinforcement, not the source). This is why Flow's class/object error fires far more often than the inverse experience would suggest.

### `implements` and `extends` clauses must name an interface or class {#toc-implements-extends-rhs}

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

### Primitives are not subtypes of interfaces or object types {#toc-primitives-interfaces}

TypeScript treats `string` / `number` / `boolean` as structurally assignable to any interface or object type they satisfy — the primitive is implicitly promoted to its boxed wrapper (`String` / `Number` / `Boolean`) and then structurally checked. Flow does not perform that promotion in `.js` files: a primitive flowing into an interface errors with `[incompatible-type]` ("Cannot use string as a subtype of interface"), and a primitive flowing into an object type errors with the generic `[incompatible-type]` code.

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

### Distinct `void` vs `undefined` types {#toc-void-vs-undefined}

TypeScript treats `void` and `undefined` as two separate types: `undefined` is the type whose sole inhabitant is the value `undefined`, while `void` is primarily a function-return marker meaning "no useful return value". The assignability between them is asymmetric — a `() => undefined` is assignable to a `() => void` slot, but a `() => void` is *not* assignable to a `() => undefined` slot:

```ts
// TypeScript:
declare const u: () => undefined;
declare const v: () => void;
const a: () => void = u;       // OK
const b: () => undefined = v;  // ERROR — Type 'void' is not assignable to type 'undefined'.
```

Flow has only `void`. Using `undefined` as a *type annotation* is a hard error: `[unsupported-syntax]` "The equivalent of TypeScript's `undefined` type in Flow is `void`. Flow does not have separate `void` and `undefined` types." (`undefined` remains a valid *value* — it is the inhabitant of `void`.)

```js flow-check
function f(): undefined { // ERROR
  return undefined;
}
```

```js flow-check
function f(): void {
  return undefined; // OK — `undefined` is the value inhabiting `void`
}
```

This comes up most often when a TS-shaped function signature gets typed in Flow verbatim (`function f(): undefined`, `(x: string | undefined) => ...`, callback shapes that explicitly type `undefined` parameters), and on TS utility-typed code (`Exclude<T, undefined>`, `T extends undefined ? ...`) where the `undefined` literal type appears inside a generic. Canonical Flow forms: `undefined` → `void` for annotations; `T | undefined` → `?T` if `null` is also intended (most JS APIs — see [maybe types](#toc-maybe-types) below) or `T | void` if only the absent case is intended.

### Maybe types vs union with null/undefined {#toc-maybe-types}

Flow's [maybe type](./types/maybe.md) `?T` is shorthand for `T | null | void` — fully equivalent as a type. Coming from TypeScript, the natural reach is `T | null | undefined`, which won't work because Flow has no `undefined` type (see [above](#toc-void-vs-undefined)). The Flow equivalent is `?T`, or equivalently `T | null | void`.

One adjacent gotcha: `T | void` (without `null`) is *not* the same as `?T` — it lacks `null`. The Flow form for the TS shape `T | null | undefined` is `?T`; for plain `T | undefined` (no null) it is `T | void`.

At function-parameter position, any parameter type that includes `void` — whether spelled `?T`, `T | null | void`, or `T | void` — makes the argument implicitly optional, so callers can omit it entirely:

```js flow-check
function f(x: ?number) {}
f(null);      // OK
f(undefined); // OK
f();          // OK — `?T` includes `void`, which makes the arg optional
```

### `as` casts are stricter in Flow {#toc-as-casts}

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

### Conditional types and mapped types {#toc-conditional-mapped}

[Conditional types](./types/conditional.md) and [mapped types](./types/mapped-types.md) match TypeScript in shape and in the most common behaviors: distributivity over unions, `keyof`-based source iteration, `infer` on the right-hand side of `extends`, the `[T] extends [...]` non-distributive opt-out, key-type constraint to `string | number | symbol`, distribution over `null` / `undefined`, and the homomorphic mapped-type form `{[K in keyof T]: ...}`. The TypeScript pattern translates directly.

The remaining divergence is one mapped-type modifier feature that Flow does not yet support.

**Variance modifiers on tuple/array sources are not supported.** When the source is a tuple or array type, only `?` (optionality) is allowed. `+` and `-` error.

```js flow-check
type Tuple = [number, string];
type Bad1 = {-[K in keyof Tuple]: string}; // ERROR
type Bad2 = {+[K in keyof Tuple]: string}; // ERROR
type OK   = {[K in keyof Tuple]?: string}; // OK
```

> Mapped Types do not yet support variance annotations on array inputs. [invalid-mapped-type]

For conditional types, the only Flow-vs-TS gap is `infer T extends Bound`, which is covered in [TypeScript-only features](#toc-infer-extends).

### Generic type arguments cannot be elided {#toc-generic-default-elision}

TypeScript lets you write a generic type unparameterized when every type parameter has a default — `Foo<T = string>` followed by `type A = Foo` resolves `A` to `Foo<string>`. Flow rejects the bare form and requires an explicit type-argument list (or an empty `<>` to fall back on defaults), reporting `[missing-type-arg]` "Cannot use `Foo` without 0-1 type arguments."

```js flow-check
type Foo<T = string> = {x: T};
type A = Foo;   // ERROR — [missing-type-arg]
type B = Foo<>; // OK    — uses the default `T = string`
```

The rewrite is mechanical: `Foo` → `Foo<>` for all-defaulted generics, or supply the args explicitly. The rationale for the explicit form is that Flow reserves the bare name `Foo` for the *type constructor itself* (so that operations on the type — type-level functions and the like — can take the unapplied form as input), rather than overloading it as shorthand for an applied instantiation.

### Variance keywords (`readonly` / `writeonly`, `in` / `out`) {#toc-variance-keywords}

Flow's standard syntax for variance uses the TS-aligned keyword forms: `readonly` / `writeonly` on properties and `in` / `out` on type parameters. The older `+foo` / `-foo` property sigils and `+T` / `-T` tparam sigils still parse for legacy code, but the keyword forms are preferred for new code. Note that `writeonly` is Flow-specific — TypeScript has no write-only equivalent.

In the other direction, TypeScript's combined `<in out T>` (explicit invariance) has no Flow counterpart. TypeScript infers variance from usage and preserves several compatibility-oriented exceptions, so users sometimes need to opt *back into* invariance to recover the stricter guarantee they wanted; Flow's default is invariance, so the stricter choice is what you get when you write nothing. See [Generic type arguments are invariant by default](#toc-variance-generics) below for the defaults contrast in detail.

Beyond the spelling, Flow validates that a tparam declared `out T` (or `in T`) is only used in body positions that match the declared variance — `out T` in an input position errors `[incompatible-variance]` "Cannot use `T` in an input position because `T` is expected to occur only in output positions." TypeScript also validates `in` / `out` against the body in many positions (e.g. `interface Box<out T> { set: (t: T) => void }` errors in TS too, since the function-typed *field* puts `T` contravariantly). The narrower gap is that TS keeps *method shorthand* bivariant even under an `out`/`in` annotation, so the Flow form below — written with method shorthand — errors in Flow but compiles in TS.

```js flow-check
type Box<out T> = {
  set(t: T): void; // ERROR — [incompatible-variance]
};
```

This subsection is about the *syntax*; for the much more important *semantic* divergence in how variance is enforced at each position, see the next subsection. See [`lang/variance.md`](./lang/variance.md) for full mechanics.

### Variance positions are stricter by default in Flow {#toc-variance-positions}

This is the single biggest cluster of TypeScript code that type-checks but relies on weaker static guarantees — every example in this subsection is a place where TypeScript accepts a program that can either throw at runtime or produce values whose static type has gone stale. Flow defaults variance positions to stricter choices; TypeScript trades some static precision for compatibility and ergonomics. The five positions below cover the cases TypeScript intuition will most often lead you into.

#### Mutable object properties are invariant in Flow, covariant in TS {#toc-variance-mutable-props}

Assigning `{x: number}` to `{x: number | string}` widens the slot's read type but also widens what can be written into it, so a downstream `obj.x = "oh no"` would corrupt the original.

```js flow-check
function f(obj: {x: number | string}) {}
const o: {x: number} = {x: 1};
f(o); // ERROR — property `x` is invariantly typed
```

```ts
// TypeScript allows this under structural covariance.
function f(obj: {x: number | string}) {}
const o: {x: number} = {x: 1};
f(o);
```

The TypeScript workaround is to mark the property `readonly`, but see the next sub-bullet.

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

The TypeScript-side workaround is `ReadonlyArray<T>`. Worth noting that `$ReadOnlyArray` / `ReadonlyArray` exist in Flow precisely *because* the mutable form is invariant — a fact often missed when reaching for the covariant TS pattern.

#### Generic type arguments are invariant by default {#toc-variance-generics}

Flow defaults generic parameters to invariance and asks the user to opt into co/contravariance with `out T` / `in T` (or the deprecated `+T` / `-T`). TypeScript infers variance from usage and preserves compatibility-oriented exceptions, which can leave read-write fields with weaker static guarantees than the Flow default.

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

A `{compare(x: number, y: number): number}` is not a subtype of `{compare(x: number | string, y: number | string): number}` in Flow; TypeScript treats it as one. Under TypeScript's `--strictFunctionTypes`, function-typed *fields* are contravariant but methods stay bivariant — this asymmetry is itself a TS-only wrinkle.

In Flow, both forms reject the widening, but for different reasons and with different error messages: method shorthand fails contravariance (`[incompatible-type]` "the first parameter: number is incompatible with string"), while function fields fail *invariance* (the property itself is mutable, so the error says the property is invariantly typed). Switching from method shorthand to function field in Flow makes the check stricter, not looser — to allow widening in Flow you need `readonly compare` so the property itself becomes covariant.

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

TypeScript's bivariance hole is invisible at the call site, which can make Flow look "stricter for no reason" if you are coming from TypeScript — except the strictness is exactly what stops `nn.compare("oh", "no")` from doing string subtraction at runtime. Each Flow rejection in this section blocks a pattern that can produce a runtime failure or inaccurate static type; the cost of writing the Flow form is real, but it is the cost of making bugs visible at type-check time instead of in production.

See [`lang/variance.md`](./lang/variance.md) and [`lang/depth-subtyping.md`](./lang/depth-subtyping.md) for the full mechanics.

### Class methods cannot be unbound from their `this` {#toc-method-unbinding}

Method-shorthand properties on a *class* track their `this` binding in the type system; extracting one (`const f = c.m`) would lose that binding and is rejected with `[method-unbinding]` "Cannot get `c.m` because property `m` cannot be unbound from the context where it was defined." TypeScript treats methods as plain function values and lets the same extraction through silently — the resulting call then has the wrong `this` at runtime.

```js flow-check
class C {
  x: number = 0;
  m(): number { return this.x; }
}
const c = new C();
const f: () => number = c.m; // ERROR — [method-unbinding]
```

The Flow rewrites are either keep the call bound (`c.m()` directly), wrap with an arrow that captures `this` (`const f = () => c.m()`), or call `.bind` (`const f = c.m.bind(c)`). Note this is a class-instance rule — method-shorthand on plain object types (`{m(x: number): number}`) doesn't carry a `this` context to lose, so extraction is allowed there.

### Optional properties cannot be silently re-introduced {#toc-optional-reintroduction}

TypeScript allows a property to be forgotten via inexact subtyping and then re-introduced at a different (optional) type — the path `{x: number, y: string}` → `{x: number}` → `{x: number, y?: number}` type-checks in TypeScript, leaving `y` typed as `number | undefined` while it actually holds the string `"Uh oh"`. Flow blocks this because exactness gates both directions: a property can only be forgotten when the target is inexact, and re-introduced only when the source is exact. This is the same underlying mechanism as [object exactness](#toc-object-exactness) showing up in a different shape — typical when modeling a Flow function on a TypeScript signature that takes a "looser" type and adds optional fields.

### Tuple spread after an optional element is banned {#toc-tuple-spread-optional}

Spreading a tuple type with optional elements into another tuple is allowed in TypeScript but produces an inaccurate tuple type: `const x: [a?: 1] = []; const y: [0, 1 | undefined, 2] = [0, ...x, 2];` compiles in TS but `y[2]` is `undefined` at runtime. Flow rejects the spread with `[invalid-tuple-arity]` ("array literal has an unknown number of elements").

```js flow-check
const x: [a?: 1] = [];
const y: [0, 1 | void, 2] = [0, ...x, 2]; // ERROR
```

The Flow rewrite is explicit branching on the optional element, since the result tuple's element positions cannot be statically known when the spread source has variable arity.

### User-defined type guard bodies are validated {#toc-type-guard-validation}

TypeScript does not check the body of an `x is T` predicate function — the predicate is trusted blindly, so the following type-checks in TypeScript even though the body has nothing to do with `number`, and any caller relying on this guard will be lied to:

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

See [`types/type-guards.md`](./types/type-guards.md#toc-consistency-checks-of-type-guard-functions) for the full consistency rules.

### The top/bottom lattice {#toc-top-bottom}

Flow's top type is [`unknown`](./types/unknown.md); the [bottom](./types/empty.md) type is `empty`. TypeScript names the same two `unknown` and `never`. `never` is the natural reach from TypeScript when `empty` is what Flow expects. See [type hierarchy](./lang/type-hierarchy.md) for where these sit relative to the rest of Flow's types.

### Object type spread is type-level in Flow, value-level only in TS {#toc-type-spread}

Flow lifts `{...A, b: T}` to the type level — `type C = {...A, b: T}` is a real type expression that combines `A`'s properties with `b: T`. TypeScript has no type-level spread; it uses intersection (`type C = A & {b: T}`) instead.

This isn't a stylistic choice — it falls out of [exact object types](./types/objects.md#exact-and-inexact-object-types). Because Flow's exact object types forbid unlisted properties, intersecting two exact object types produces an *impossible* type: a value would have to be exactly `A` *and* exactly `B` simultaneously, which is uninhabitable as soon as `A` and `B` differ at all (see [impossible intersection types](./types/intersections.md#toc-impossible-intersection-types)). So Flow needs a different operation to combine exact object types. Type-level spread (`{...A, ...B}`) is that operation, and it mirrors the runtime semantics of value-level spread directly: own properties only (so [interfaces can't be spread](./types/objects.md#object-type-spread), since they don't track own-vs-prototype), later keys overwrite earlier ones, and exactness propagates — spreading an inexact type forces the result inexact, since the source could carry unknown properties.

The intersection form `A & {b: T}` is the natural reach if you're thinking in TypeScript, but it's the wrong tool for the job in Flow: `&` keeps its TypeScript intersection semantics, so writing `A & {b: T}` when `A` already declares `b` silently produces an uninhabitable type rather than the merged shape you wanted. The Flow idiom is `{...A, b: T}` — same shape as runtime spread, accurate semantics, no accidental impossibility. The [objects docs](./types/objects.md#object-type-spread) cover the full spread rules.

```js flow-check
type A = {x: number, y: string};
type C = {...A, z: boolean};
const c: C = {x: 1, y: "hi", z: true};
```

### Refinement invalidation rules differ {#toc-refinement-invalidation}

Both Flow and TypeScript narrow types via `typeof`, `instanceof`, equality, type guards, etc. — but the rules for when a refinement is dropped diverge in ways that have no syntactic signal. Flow invalidates a refinement when intervening code could have changed the underlying value at that storage location:

- A write to the refined binding or property (`x = ...`, `obj.k = ...`).
- A refinement on an object property where the property is reachable through aliasing or could be mutated by a callee.
- A refinement on a binding captured by a closure that an intervening call could invoke.

A bare call to a function that does not visibly touch the refined location does **not** by itself drop a refinement on a local — that is the most common over-correction. TypeScript's narrowing has its own (also non-trivial) invalidation model that does not agree with Flow's in detail; the same code may type-check in TS and not in Flow, or vice versa. See [refinement invalidations](./lang/refinements.md#toc-refinement-invalidations) for the full rule set.

```js flow-check
declare function sideEffect(): void;

function localCase(x: ?number) {
  if (x != null) {
    sideEffect();           // bare call does NOT drop the refinement on a local
    const a: number = x;    // OK
  }
}

function propertyCase(obj: {x: ?number}) {
  if (obj.x != null) {
    sideEffect();           // bare call DROPS the refinement on a property
    const a: number = obj.x; // ERROR — callee could have mutated `obj.x`
  }
}

function writeCase(x: ?number) {
  if (x != null) {
    x = null;
    const a: number = x;    // ERROR — direct write invalidates the refinement
  }
}
```

### Annotations are required at module boundaries {#toc-annotations-boundaries}

Flow uses [Local Type Inference](./lang/annotation-requirement.md): it requires annotations on function parameters, exports, and other key boundaries, and reports `[signature-verification-failure]` if a module's exports cannot be typed from annotations alone. If you're used to leaving exports unannotated and letting the typechecker infer them across modules, that will not work in Flow — the annotations have to be there.

This is a deliberate design choice that enables Flow to scale to repositories with millions of files. Because each module's exports are fully described by its annotations, Flow can extract a "typed interface" for the module without analyzing the module body, then typecheck every other module against that interface in parallel. A change inside a module's implementation invalidates only that one module — its dependents can be re-checked against the unchanged interface.

See [`lang/annotation-requirement.md`](./lang/annotation-requirement.md) and the [`Module Exports`](./lang/annotation-requirement.md#toc-module-exports) subsection for full mechanics.

### Type-only bindings cannot cross the value/type seam {#toc-type-export-validation}

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

### Error suppressions are coded and scoped {#toc-suppressions}

Flow's `$FlowFixMe[code]` (and `$FlowExpectedError[code]` / `$FlowIssue[code]`) suppresses **only** the named error code at that location — any other error on the same line still surfaces, and the suppression itself errors as unused if the targeted code doesn't fire. TypeScript's `// @ts-ignore` silences every error on the next line indiscriminately, and `// @ts-expect-error` similarly silences everything but errors when nothing was suppressed. The Flow form is strictly more granular and keeps suppression debt auditable. See [`errors/index.md`](./errors/index.md).

```js flow-check
declare function takesNumber(n: number): void;
// $FlowFixMe[incompatible-type] - intentional for demo
takesNumber("not a number");
```

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

[`hook` syntax](./react/hook-syntax.md) is a first-class Flow keyword for declaring React hooks. Flow uses the keyword to enforce the [Rules of React](https://react.dev/reference/rules) at the type level on hook call sites. TypeScript has no equivalent — hook rules in TS are enforced by ESLint at lint time, not at type-check time.

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

Flow has [`match` expressions and statements](./match/index.md) for pattern matching with structural patterns, guards, and exhaustiveness checking. TypeScript has no `match`; the closest analogue is hand-coded discriminated-union switches with an `assertNever` fallthrough.

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

[Opaque type aliases](./types/opaque-types.md) hide their underlying type outside the file in which they are defined, enforcing nominal abstraction across module boundaries. TypeScript has no native equivalent; the common idiom there is "branded types" using intersection with a private symbol-typed property, which is a pattern rather than a language feature.

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

[Flow Enums](./enums/index.md) and TypeScript `enum` look superficially similar but are very different in detail. The substantive differences are:

**Exhaustive checking.** Flow errors with `[invalid-exhaustive-check]` if a `switch` over an enum forgets a member. TypeScript has no built-in switch exhaustiveness diagnostic for enums; users typically encode it with `never` or lint rules.

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

**Implicit coercion.** Flow blocks both directions — `number` is not assignable to a Flow Enum, and a Flow Enum is not assignable to its underlying primitive. Use the `.cast()` method for safe value-to-enum conversion and `<expr> as number` for the explicit reverse cast. TypeScript still permits a `number`-typed value to be assigned to a number-enum slot (though as of TS 5.x a *literal* like `42` that doesn't match any member errors), and TypeScript still freely coerces enums back to numbers and into arithmetic.

**Default member values.** A Flow `enum X of number { Active, Paused, Off }` errors with `[invalid-enum]` — number-enum members must be explicitly initialized. TypeScript auto-numbers from `0`. Flow's rationale is that adding or removing a member from the middle of an auto-numbered enum silently renumbers everything after it, which is a serialization/logging hazard. Flow's *string* enums (the default when no `of` is given) mirror their member names, so they don't have this problem.

**Extending / re-declaring.** Flow errors with `[name-already-bound]` if you redeclare the enum name. TypeScript allows the multiple-declaration form, which can interact unexpectedly with default values — `enum Status { Disabled }` after a previous `enum Status { Active = 0, ... }` makes `Disabled === Active === 0`.

**Reverse mapping.** Flow exposes a `.getName(value)` method that works for both number and string enums. TypeScript's number enums get a runtime reverse-map (`Status[Status.Off]` returns `"Off"`), but TypeScript explicitly errors on the same access for string enums — `StatusStr[StatusStr.Off]` errors with `TS2551 Property 'off' does not exist on type 'typeof StatusStr'. Did you mean 'Off'?`, because the literal type of `StatusStr.Off` is its string value (`"off"`) rather than the member key (`Off`).

**Iterating members.** Flow provides `Status.members()`, which returns just the enum's values. TypeScript's number enums require a `for...in` over the runtime object, which produces *both* the numeric keys and the member names — for a 3-member enum, the output is `[ '0', '1', '2', 'Active', 'Paused', 'Off' ]`.

**Symbol enums.** Flow supports `enum X of symbol { ... }`. TypeScript has no symbol-enum form.

**Definition restrictions.** Flow rejects heterogeneous initializers (`[invalid-enum]` "inconsistent member initializers"), non-literal initializers (`[ParseError]` "needs to be a literal"), and lowercase-leading member names (`[invalid-enum-member-name]`, since lowercase identifiers are reserved for enum methods like `.cast`). TypeScript permits all three.

See the [Flow Enums docs](./enums/index.md) for full mechanics.

### One-sided type guards (`implies x is T`) {#toc-one-sided-guards}

A predicate function whose return type is `implies param is T` refines the parameter to `T` only when the function returns `true`, and leaves it unchanged when the function returns `false`. This is the escape hatch for the body-validation rule covered above when only the positive direction holds. TypeScript has no equivalent.

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

### Flow-only syntactic forms {#toc-flow-only-syntax}

A handful of Flow type-expression forms have no TypeScript spelling — `tsc` rejects them at parse time. None of them change the type system; they are surface-syntax niceties.

- **Inline `interface` type expression** — `type T = interface { foo: number }`. Lets an interface appear inside a type expression instead of as a top-level declaration. TypeScript requires a separate `interface I { ... }` statement.
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

Setting `relay_integration=true` in `[options]` makes Flow natively understand `graphql` tagged template literals and infer their types from the Relay compiler's emitted artifacts, so users can omit explicit type parameters on `useFragment`, `usePreloadedQuery`, etc. Companion options: `relay_integration.esmodules` (resolve artifacts as ES module default exports rather than CommonJS) and `relay_integration.excludes` (per-directory opt-out). See [`config/options.md`](./config/options.md#toc-relay-integration).

TypeScript has no typechecker-level equivalent. TypeScript users either pass the generated type explicitly (`useFragment<MyFragment$key>(...)`), use a TypeScript *language service plugin* for editor hints (not typechecking), or use document-node patterns like `graphql-typed-document-node` / `gql.tada` that require explicit imports of generated types.

## TypeScript-only features that do not exist in Flow {#toc-ts-only}

These are TypeScript features that have no Flow equivalent. Some are missing simply because Flow hasn't implemented them yet (`infer T extends Bound`, parts of full template-literal types). Others Flow has deliberately not adopted, either because they overlap a Flow feature with different (usually more conservative) defaults or because they introduce footguns Flow's design avoids. Reaching for these in Flow code won't work — and in some cases the TypeScript syntax will parse, so the failure shows up later than expected.

### Coming soon {#toc-coming-soon}

Features that are in-progress and will be released in the future:

- `satisfies` expression — checks an expression against a type without changing the resulting type. Mirrors TypeScript: `as` widens to the annotation while `satisfies` only validates, so the precise inferred shape is preserved.
- Mapped type modifier features — optionality removal `-?`, variance removal `-readonly`, and `as` key remapping (`{[K in keyof T as ...]: ...}`).
- Template literal types — e.g. `` `${'a' | 'b'}-${'x' | 'y'}` ``, including wide-type interpolation (`` `${number}px` ``, `` `is_${boolean}` ``, etc.)
- Additional TS utility types not yet in Flow — `ConstructorParameters`, `InstanceType`, `ThisType`.
- Assertion functions — `function isStr(x: mixed): asserts x is string { ... }` refines the caller's binding by throwing on failure
- `override` keyword on class members — `class B extends A { override m() {} }`
- Abstract methods — `abstract class Shape { abstract area(): number }`
- Constructor types — `type Ctor = new (x: number) => R`
- Symbol-keyed properties (and `unique symbol` for unique symbol types).
- Inline `import()` type expression — `type A = import('./m').A` as an alternative to a top-level `import type {A} from './m';`
- `import X = require('foo')` — TypeScript's CommonJS-style import binding; today the Flow form is `const X = require('foo')`

### TS-only syntactic forms {#toc-ts-only-syntax}

A handful of TS surface-syntax forms have no Flow spelling, but the *concept* is available in Flow under a different name. Flow rejects the TS form at parse/type-check time with a `[unsupported-syntax]` diagnostic that points at the Flow rewrite directly.

- **`readonly` type operator on tuples** — TS `readonly [T, S]` → Flow `Readonly<[T, S]>`.
- **`readonly` type operator on array shorthand** — TS `readonly T[]` → Flow `ReadonlyArray<T>`.
- **Optional unlabeled tuple elements** — TS `[number, string?]` → Flow `[a: number, b?: string]`. Flow requires the labeled variant when any element is optional.

Note that `readonly` as a *property modifier* (`{readonly x: T}`) and on tparams (`out T`) works the same way in both languages — see [Variance keywords](#toc-variance-keywords). The two `readonly` forms above are uses of `readonly` as a *type operator* (a prefix on a structural type), which is a TS-only spelling — Flow uses the wrapper utility instead.

```js flow-check
type A = readonly [number, string]; // ERROR — use Readonly<[number, string]>
type B = readonly number[];          // ERROR — use ReadonlyArray<number>
type C = [number, string?];          // ERROR — use the labeled form below
type OkA = Readonly<[number, string]>;
type OkB = ReadonlyArray<number>;
type OkC = [a: number, b?: string];
```

### Decorators {#toc-decorators}

Flow has no decorator support in any mode. TypeScript now supports two incompatible modes: **stage-3 decorators** (the default in TS 5.x, with a context-object parameter) and **legacy decorators** (under `--experimentalDecorators`, with the old `(target, key)` signature).

### TypeScript class syntax extensions {#toc-class-extensions}

TypeScript has several class-syntax extensions Flow has deliberately not adopted, asking users to write the equivalent JS instead. Each errors with `[unsupported-syntax]` and a fix-it message in the diagnostic.

**Parameter properties** (`constructor(public x: number)`) — a TS-only shorthand that emits runtime code: it auto-declares the field and assigns it from the constructor argument. Flow's diagnostic: "Flow does not support TypeScript parameter properties. To fix, declare the property in the class body and assign it in the constructor."

```js flow-check
class C {
  constructor(public x: number) {} // ERROR — [unsupported-syntax]
}
```

**`public` / `protected` / `private` access modifiers** — TS-checked access control. These are type-checker-only in TypeScript (the field is still publicly accessible at runtime), so dropping them is safe. Flow's diagnostics:
- `public`: "Flow does not support using `public` in classes. Fields and methods are public by default."
- `protected`: "Flow does not support using `protected` in classes. To fix, remove the `protected` modifier."
- `private`: "Flow does not support using `private` in classes. Use JavaScript private elements instead. To fix, change `private foo` to `#foo`."

```js flow-check
class C {
  public a: number = 1;    // ERROR — drop the modifier
  protected b: number = 2; // ERROR — drop the modifier
  private c: number = 3;   // ERROR — use `#c` instead
}
```

The `private` rewrite lands at a different runtime shape from the TS form: ECMAScript `#private` fields are nominally private at runtime, while TS `private` is erased. Flow tracks `#private` nominally as part of the class identity.

### Runtime `namespace` blocks {#toc-namespace-blocks}

No source-level `namespace { ... }` blocks. Flow has [`declare namespace`](./libdefs/creation.md) for ambient declarations inside libdefs, but not source-level namespace blocks that produce runtime values.

### `const enum` {#toc-const-enum}

No equivalent. Flow Enums are a runtime construct by design and do not have an inlined-at-compile-time mode. If you need a constants table that the bundler can tree-shake, use exported `const` values.

### `infer extends` {#toc-infer-extends}

No equivalent. Flow's `infer` exists in conditional types but does not support TypeScript's `infer T extends Bound` constraint form. Restructure the conditional or move the bound check elsewhere.

### User-side module augmentation {#toc-module-augmentation}

No equivalent at the source level. TypeScript users routinely re-open third-party modules from source code via `declare module 'name' { ... }` to add types. Flow's `declare module` is only used inside *libdefs* under `flow-typed/`, not from arbitrary source files.

### Note: declaration merging is partially supported {#toc-declaration-merging}

Underpinning the merging cases below: Flow uses TypeScript's split-namespace model. Each name independently inhabits a *value* namespace and a *type* namespace, so a single identifier can be both a value and a type without colliding — `const A = 1; interface A {}` is accepted, value-side uses of `A` resolve to the const, type-side uses resolve to the interface. Constructs usable in both namespaces — classes, enums, opaque types — register once in the value namespace and the type side falls back to it.

On top of that namespace model, Flow supports the merging cases that matter most. Specifically, Flow merges:

- `interface` + `interface` — members union (first-wins on conflicts), `extends` lists concatenate, call signatures overload as intersections, type-param arities must match. Supported in both type-sig and local checking.
- `declare module` + `declare module` — exports union (first-wins on name collisions), incompatible export styles (CJS vs. ES) error, star re-exports concatenate.
- `declare class` + `interface` — interface members fold into the class (either order).
- `function` / `declare function` + `declare namespace` — namespace's type members fold into the function (either order).
- `class` / `declare class` + `declare namespace` — namespace's type members fold into the class (either order).

What Flow does *not* do is the *runtime-merging* cases — for example, arbitrary `function` + `namespace` value-side merging where the namespace contributes runtime members, and the user-side `declare module 'name' { ... }` source-level augmentation pattern above.

## Syntax convergence with TypeScript {#toc-convergence}

Several of Flow's older syntactic forms have been renamed to match TypeScript over the last few releases. If you are reading older Flow code, expect to see the legacy form; new code should use the TS-aligned form.

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

The table above covers forms that still parse. A second set of legacy Flow utilities has been removed entirely but maps cleanly to TS-aligned forms — worth knowing when reading code that predates the removals:

- `%checks` predicate functions → user-defined [type guards](./types/type-guards.md) (`function isString(x: unknown): x is string`).
- `$ObjMap<O, F>` / `$ObjMapi<O, F>` / `$TupleMap<T, F>` / `$TupleMapi<T, F>` → [mapped types](./types/mapped-types.md) (`{[K in keyof O]: ...}`) with the function body inlined.
- `$PropertyType<T, K>` / `$ElementType<T, K>` → [indexed access](./types/indexed-access.md) (`T[K]`).
- `$Call<F, ...Args>` → [`ReturnType<F>`](./types/utilities.md) plus indexed access, or a [conditional type](./types/conditional.md) with `infer`.
- `$Diff<A, B>` / `$Rest<A, B>` → typically [`Omit<A, keyof B>`](./types/utilities.md), case by case (not always semantically identical).

For the full picture — including Flow-specific utilities that have no rename (`$KeyMirror`, `$Exports`) and the exact Flow version each form was removed in — see [Modernizing Legacy Flow Syntax](./modernizing-legacy-syntax.md).

## `.flowconfig` options aligned with TypeScript {#toc-shared-options}

A few Flow `[options]` toggles correspond directly to TypeScript `compilerOptions` strictness flags — same semantics, same individual default (`false`), same opt-in intent. One thing to watch when porting: on the TypeScript side, `useUnknownInCatchVariables` is included in `--strict` and therefore defaults to `true` whenever `--strict` is on, while `noUncheckedIndexedAccess` is not part of `--strict` and stays `false` unless explicitly set. Flow has no `--strict` umbrella — each flag is opt-in individually.

### `no_unchecked_indexed_access` ↔ `noUncheckedIndexedAccess` {#toc-no-unchecked-indexed-access}

Both options make indexed access through an array or dictionary widen the result type with `undefined` (Flow: `void`), so reading `arr[i]` or `dict[k]` returns `T | void` instead of `T` and forces the caller to refine before use. Tuple access with a *literal* index is unaffected in both languages. See [`no_unchecked_indexed_access`](./config/options.md#toc-no-unchecked-indexed-access) for the full Flow rules.

### `use_unknown_in_catch_variables` ↔ `useUnknownInCatchVariables` {#toc-use-unknown-in-catch-variables}

Both options change the default type of an un-annotated `catch` binding from `any` to `unknown` (Flow uses [`unknown`](./types/unknown.md) too, after the [`mixed` → `unknown` rename](#toc-convergence)). The caller has to narrow the value (`instanceof Error`, `typeof e === 'string'`, …) before using it. See [`use_unknown_in_catch_variables`](./config/options.md#toc-use-unknown-in-catch-variables).

### Looser overlaps {#toc-shared-options-loose}

A few other Flow options share intent with a TypeScript flag but differ in mechanism enough that they aren't drop-in equivalents:

- [`experimental.strict_es6_import_export`](./config/options.md#toc-experimental-strict-es6-import-export) is closest in spirit to TypeScript's `isolatedModules` / `verbatimModuleSyntax` — both enforce stricter import/export discipline so modules can be processed in isolation — but the specific lints differ.
- [`module.name_mapper`](./config/options.md#toc-module-name-mapper) overlaps with TypeScript's `paths`, with the difference that Flow uses regex substitution while TypeScript uses glob templates.

## See also {#toc-see-also}

- [Glossary](./glossary.md) — carries a one-line TypeScript note on concepts that have one, and serves as a quick index when you only need to look up a single term.
- [Modernizing Legacy Flow Syntax](./modernizing-legacy-syntax.md) — the full reference for migrating Flow's legacy `$`-prefixed utilities and other older syntactic forms to their modern (often TS-aligned) equivalents.
