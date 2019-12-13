---
layout: guide
---

> **Want a lint that isn't here?** We're looking to our community to
> [add lints](https://github.com/facebook/flow) that leverage Flow’s type system.
> (Tutorial blog post coming soon!)

### Available Lint Rules <a class="toc" id="toc-available-lint-rules" href="#toc-available-lint-rules"></a>

* [`all`](#toc-all)
* [`sketchy-null`](#toc-sketchy-null)
* [`sketchy-number`](#toc-sketchy-number)
* [`untyped-type-import`](#toc-untyped-type-import)
* [`untyped-import`](#toc-untyped-import)
* [`unclear-type`](#toc-unclear-type)
* [`unsafe-getters-setters`](#toc-unsafe-getters-setters)
* [`deprecated-declare-exports`](#toc-deprecated-declare-exports)
* [`nonstrict-import`](#toc-nonstrict-import)
* [`unnecessary-optional-chain`](#toc-unnecessary-optional-chain)
* [`unnecessary-invariant`](#toc-unnecessary-invariant)
* [`deprecated-utility`](#toc-deprecated-utility)
* [`dynamic-export`](#toc-dynamic-export)
* [`implicit-inexact-object`](#toc-implicit-inexact-object)

#### `all` <a class="toc" id="toc-all" href="#toc-all"></a>

While `all` isn't technically a lint rule, it's worth mentioning here. `all` sets the default
level for lint rules that don't have a level set explicitly. `all` can only
occur as the first entry in a `.flowconfig` or as the first rule in a --lints
flag. It's not allowed in comments at all because it would have different
semantics than would be expected. (A different form of comment with the expected semantics is in the works.)

#### `sketchy-null` <a class="toc" id="toc-sketchy-null" href="#toc-sketchy-null"></a>

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

#### `sketchy-number` <a class="toc" id="toc-sketchy-number" href="#toc-sketchy-number"></a>

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

#### `untyped-type-import` <a class="toc" id="toc-untyped-type-import" href="#toc-untyped-type-import"></a>

Triggers when you import a type from an untyped file. Importing a type from an
untyped file results in an `any` alias, which is typically not the intended behavior.
Enabling this lint brings extra attention to this case and can help improve Flow
coverage of typed files by limiting the spread of implicit `any` types.

#### `untyped-import` <a class="toc" id="toc-untyped-import" href="#toc-untyped-import"></a>
Triggers when you import from an untyped file. Importing from an untyped file
results in those imports being typed as `any`, which is unsafe.

#### `unclear-type` <a class="toc" id="toc-unclear-type" href="#toc-unclear-type"></a>
Triggers when you use `any`, `Object`, or `Function` as type annotations. These
types are unsafe.

#### `unsafe-getters-setters` <a class="toc" id="toc-unsafe-getters-setters" href="#toc-unsafe-getters-setters"></a>
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

#### `nonstrict-import` <a class="toc" id="toc-nonstrict-import" href="#toc-nonstrict-import"></a>
Used in conjuction with [Flow Strict](../../strict/). Triggers when importing a non `@flow strict` module. When enabled, dependencies of a `@flow strict` module must also be `@flow strict`.

#### `deprecated-declare-exports` <a class="toc" id="toc-deprecated-declare-exports" href="#toc-deprecated-declare-exports"></a>

Note: This lint was removed in Flow version 0.68, along with the `declare var exports` syntax.

Triggers when the deprecated syntax is used to declare the default export of a [declared CommonJS module](../../libdefs/creation/#toc-declaring-a-commonjs-module).

Before Flow version 0.25, the way to declare the default exports looked like this:

```js
declare module "foo" {
  declare var exports: number; // old, deprecated syntax
}
```

In version 0.25, we introduced an alternative syntax:

```js
declare module "foo" {
  declare module.exports: number;
}
```

The new syntax is simpler and less magical. The old syntax will be removed in a future version of Flow.

This lint is enabled by default. If you see an error, you should try to rewrite the offending declaration. If you are unable to rewrite the declaration (for example, if it's part of a node_module dependency), you can disable the lint in your `.flowconfig`.

To disable this lint, add a line to the `[lints]` section of your project's `.flowconfig` file.

```
[lints]
deprecated-declare-exports=off
```

However, note that this syntax will be removed soon, so you should file issues with any projects that still use it.

#### `unnecessary-optional-chain` <a class="toc" id="toc-unnecessary-optional-chain" href="#toc-unnecessary-optional-chain"></a>

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

#### `unnecessary-invariant` <a class="toc" id="toc-unnecessary-invariant" href="#toc-unnecessary-invariant"></a>

Triggers when you use `invariant` to check a condition which we know must be truthy based on the available type information. This is quite conservative: for example, if all we know about the condition is that it is a `boolean`, then the lint will not fire even if the condition must be `true` at runtime.

Note that this lint does not trigger when we know a condition is always `false`. It is a common idiom to use `invariant()` or `invariant(false, ...)` to throw in code that should be unreachable.

#### `deprecated-utility` <a class="toc" id="toc-deprecated-utility" href="#toc-deprecated-utility"></a>
Triggers when you use the `$Supertype` or `$Subtype` utility types, as these types are
unsafe and usually just equivalent to `any`. If the utilities were being used in a sound manner, the
desired behavior can usually be recovered through the [`$Shape`](../../types/utilities/#toc-shape) utility or [bounded generics](../../types/generics/#toc-generic-types-act-as-bounds).

#### `deprecated-type` <a class="toc" id="toc-deprecated-type" href="#toc-deprecated-type"></a>
Triggers when you use the `*` (existential) utility type, as this type is
unsafe and usually just equivalent to `any`.
The effect of `*` can generally be achieved by simply not providing a type annotation.

#### `dynamic-export` <a class="toc" id="toc-dynamic-export" href="#toc-dynamic-export"></a>
Triggers when a dynamic type (usually `any`) appears in a position exported from a file. Note that this is a very noisy lint, and can be triggered even
when exporting types that are defined in our library definitions to include `any` types. For this reason we recommend turning it on on a per-file or even
per-line basis using [flowlint comments](../flowlint-comments/#toc-flowlint) rather than for your whole repository in the `flowconfig`.

#### `implicit-inexact-object` <a class="toc" id="toc-implicit-inexact-object" href="#toc-implicit-inexact-object"></a>
Triggers when you use object type syntax without explicitly specifying exactness or inexactness.

This lint setting is ignored when `exact_by_default` is set to `true`.

```
type A = {x: number}; // Error
type B = {x: number, ...} // Ok
type C = {| x: number |} // Ok
```
