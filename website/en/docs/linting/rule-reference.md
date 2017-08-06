---
layout: guide
---

> **Want a lint that isn't here?** We're looking to our community to
> [add lints](https://github.com/facebook/flow) that leverage Flow’s type system.
> (Tutorial blog post coming soon!)

### Available Lint Rules <a class="toc" id="toc-available-lint-rules" href="#toc-available-lint-rules"></a>

* [`all`](#toc-all)
* [`sketchy-null`](#toc-sketchy-null)
* [`untyped-type-import`](#toc-untyped-type-import)
* [`unknown-property`](#toc-unknown-property)

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

#### `untyped-type-import` <a class="toc" id="toc-untyped-type-import" href="#toc-untyped-type-import"></a>
Triggers when you import a type from an untyped file. Importing a type from an
untyped file results in an `any` alias, which is typically not the intended behavior.
Enabling this lint brings extra attention to this case and can help improve Flow
coverage of typed files by limiting the spread of implicit `any` types.

#### `unknown-property` <a class="toc" id="toc-unknown-property" href="#toc-unknown-property"></a>
Triggers when you access an unknown property on a defined type. Sealed and unsealed
objects are not affected by this rule. This prevents conditional expressions from
inferring types for properties that were not explicitly included in the type.
```
type FooType = { foo: string };
let FooObj: FooType = { foo: 'fooStr' };

// bar was not defined in FooType, so this will trigger the lint rule
if (FooObj.bar != null) { }
```
