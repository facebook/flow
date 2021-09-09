---
title: Lint Rule Reference
slug: /linting/rule-reference
---

### Available Lint Rules {#toc-available-lint-rules}

* [`all`](#toc-all)
* [`ambiguous-object-type`](#toc-ambiguous-object-type)
* [`deprecated-type`](#toc-deprecated-type)
* [`deprecated-utility`](#toc-deprecated-utility)
* [`implicit-inexact-object`](#toc-implicit-inexact-object)
* [`nonstrict-import`](#toc-nonstrict-import)
* [`sketchy-null`](#toc-sketchy-null)
* [`sketchy-number`](#toc-sketchy-number)
* [`unclear-type`](#toc-unclear-type)
* [`unnecessary-invariant`](#toc-unnecessary-invariant)
* [`unnecessary-optional-chain`](#toc-unnecessary-optional-chain)
* [`unsafe-getters-setters`](#toc-unsafe-getters-setters)
* [`untyped-import`](#toc-untyped-import)
* [`untyped-type-import`](#toc-untyped-type-import)

#### `all` {#toc-all}
While `all` isn't technically a lint rule, it's worth mentioning here. `all` sets the default
level for lint rules that don't have a level set explicitly. `all` can only
occur as the first entry in a `.flowconfig` or as the first rule in a `--lints`
flag. It's not allowed in comments at all because it would have different
semantics than would be expected.

#### `ambiguous-object-type` {#toc-ambiguous-object-type}
Like [`implicit-inexact-object`](#toc-implicit-inexact-object), except triggers even when the `exact_by_default` option is set to `true`.

#### `deprecated-type` {#toc-deprecated-type}
Triggers when you use the `*` (existential) type, as this type is unsafe and usually just equivalent to `any`.
The effect of `*` can generally be achieved by simply not providing a type annotation.

#### `deprecated-utility` {#toc-deprecated-utility}
Triggers when you use the `$Supertype` or `$Subtype` utility types, as these types are
unsafe and equivalent to `any`.

#### `implicit-inexact-object` {#toc-implicit-inexact-object}
Triggers when you use object type syntax without explicitly specifying exactness or inexactness.

This lint setting is ignored when `exact_by_default` is set to `true`.

```
type A = {x: number}; // Error
type B = {x: number, ...} // Ok
type C = {| x: number |} // Ok
```

#### `nonstrict-import` {#toc-nonstrict-import}
Used in conjuction with [Flow Strict](../../strict/). Triggers when importing a non `@flow strict` module. When enabled, dependencies of a `@flow strict` module must also be `@flow strict`.

#### `sketchy-null` {#toc-sketchy-null}
Triggers when you do an existence check on a value that can be either null/undefined or falsey.

For example:
```js
const x: ?number = 5;
if (x) {} // sketchy because x could be either null or 0.

const y: number = 5;
if (y) {} // not sketchy because y can't be null, only 0.

const z: ?{foo: number} = {foo: 5};
if (z) {} // not sketchy, because z can't be falsey, only null/undefined.
```

Setting `sketchy-null` sets the level for all sketchy null checks, but there are more granular rules for particular types. These are:
* `sketchy-null-bool`
* `sketchy-null-number`
* `sketchy-null-string`
* `sketchy-null-mixed`

The type-specific variants are useful for specifying that some types of sketchy null checks are acceptable while others should be errors/warnings. For example, if you want to allow boolean sketchy null checks (for the pattern of treating undefined optional booleans as false) but forbid other types of sketchy null checks, you can do so with this `.flowconfig` `[lints]` section:
```
[lints]
sketchy-null=warn
sketchy-null-bool=off
```
and now
```js
function foo (bar: ?bool): void {
  if (bar) {
    ...
  } else {
    ...
  }
}
```
doesn't report a warning.

Suppressing one type of sketchy null check only suppresses that type, so, for example
```js
// flowlint sketchy-null:warn, sketchy-null-bool:off
const x: ?(number | bool) = 0;
if (x) {}
```
would still have a sketchy-null-number warning on line 3.

#### `sketchy-number` {#toc-sketchy-number}
Triggers when a `number` is used in a manner which may lead to unexpected results if the value is falsy.
Currently, this lint triggers if a `number` appears in:
* the left-hand side of an `&&` expression.

As a motivating example, consider this common idiom in React:

```js
{showFoo && <Foo />}
```

Here, `showFoo` is a boolean which controls whether or not to display the `<Foo />` element. If `showFoo` is true, then this evaluates to `{<Foo />}`. If `showFoo` is false, then this evaluates to `{false}`, which doesn't display anything.

Now suppose that instead of a boolean, we have a numerical value representing, say, the number of comments on a post. We want to display a count of the comments, unless there are no comments. We might naively try to do something similar to the boolean case:

```js
{count && <>[{count} comments]</>}
```

If `count` is, say, `5`, then this displays "[5 comments]". However, if `count` is `0`, then this displays "0" instead of displaying nothing. (This problem is unique to `number` because `0` and `NaN` are the only falsy values which React renders with a visible result.) This could be subtly dangerous: if this immediately follows another numerical value, it might appear to the user that we have multiplied that value by 10! Instead, we should do a proper conditional check:

```js
{count ? <>[{count} comments]</> : null}
```

#### `unclear-type` {#toc-unclear-type}
Triggers when you use `any`, `Object`, or `Function` as type annotations. These
types are unsafe.

#### `unnecessary-invariant` {#toc-unnecessary-invariant}
Triggers when you use `invariant` to check a condition which we know must be truthy based on the available type information. This is quite conservative: for example, if all we know about the condition is that it is a `boolean`, then the lint will not fire even if the condition must be `true` at runtime.

Note that this lint does not trigger when we know a condition is always `false`. It is a common idiom to use `invariant()` or `invariant(false, ...)` to throw in code that should be unreachable.

#### `unnecessary-optional-chain` {#toc-unnecessary-optional-chain}

Triggers when you use `?.` where it isn't needed. This comes in two main flavors. The first is when the left-hand-side cannot be nullish:

```js
type Foo = {
  bar: number
}

declare var foo: Foo;
foo?.bar; // Lint: unnecessary-optional-chain
```

The second is when the left-hand-side could be nullish, but the short-circuiting behavior of `?.` is sufficient to handle it anyway:

```js
type Foo = {
  bar: {
    baz: number
  }
}

declare var foo: ?Foo;
foo?.bar?.baz; // Lint: unnecessary-optional-chain
```

In the second example, the first use of `?.` is valid, since `foo` is potentially nullish, but the second use of `?.` is unnecessary. The left-hand-side of the second `?.` (`foo?.bar`) can only be nullish as a result of `foo` being nullish, and when `foo` is nullish, short-circuiting lets us avoid the second `?.` altogether!

```js
foo?.bar.baz;
```

This makes it clear to the reader that `bar` is not a potentially nullish property.

#### `unsafe-getters-setters` {#toc-unsafe-getters-setters}
Triggers when you use getters or setters. Getters and setters can have side
effects and are unsafe.

For example:

```js
const o = {
  get a() { return 4; }, // Error: unsafe-getters-setters
  set b(x: number) { this.c = x; }, // Error: unsafe-getters-setters
  c: 10,
};
```

#### `untyped-import` {#toc-untyped-import}
Triggers when you import from an untyped file. Importing from an untyped file
results in those imports being typed as `any`, which is unsafe.

#### `untyped-type-import` {#toc-untyped-type-import}
Triggers when you import a type from an untyped file. Importing a type from an
untyped file results in an `any` alias, which is typically not the intended behavior.
Enabling this lint brings extra attention to this case and can help improve Flow
coverage of typed files by limiting the spread of implicit `any` types.
