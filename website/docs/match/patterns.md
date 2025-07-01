---
title: Match Patterns
description: "Match patterns both define a condition that must be matched, and new variables that are extracted (like destructuring)."
slug: /match/patterns
---

Match patterns both define a condition that must be matched, and new variables that are extracted (like destructuring).

## Primitive value patterns

Primitive value patterns include string literals (e.g. `'light'`), number literals (e.g. `42`), BigInt literals (e.g. `10n`), boolean literals (e.g. `true`), `null`, and `undefined`.

You can use variables (e.g. `name`) or property accesses (e.g. `Status.Active`) which are either typed as a [literal type](../../types/literals), or a [Flow Enum](../../enums) member. Using variables that are a general type like `string` or `number` is not allowed for match patterns \- they must be a literal type like `'light'`. You can add a type annotation or [`as const`](../../types/const-expression/) to your string value to type it with a literal type. Computed properties in match patterns only allow literals like `foo['bar']` or `foo[2]`.

The identifier `_` is special cased for “Wildcard patterns”, if you want to match against a value with that name, rename it to something else first. If you want to create a new variable, take a look at “variable declaration patterns” below \- it is done by doing `const x`.

You can use a number literal prefixed with  `+` or `-`, or BigInt literals prefixed with `-` (`+` on a BigInt is an error in JS). `+0` and `-0` are not allowed (Flow doesn’t differentiate these type-wise from `0`). `NaN` is special-cased, since `NaN === NaN` is always `false`. It is matched using `Number.isNaN`.

Other types of expressions are not supported.

Example:
```js
match (x) {
  1 => {}
  'foo' => {}
  null => {}
  -1 => {}
  foo => {}
  bar.baz => {}
  bar['bort'] => {}
  xs[2] => {}
}
```

Flow will never output an “unused pattern” error for the `undefined` pattern, so you can always add it even if the input type does not contain `void`. This is to cover the case where Flow does not compute a type that is 100% accurate to the runtime value (for example, from indexed access of an array past its length).

At runtime, these checks are done using triple equals `===`.

## Wildcard patterns

Wildcard patterns, which are a single underscore `_`, match everything. If you want to match against the value of a variable named `_`, assign it to a different name first.

If part of your input type cannot be matched exhaustively (e.g. `string`), then the `match` will require a wildcard.

Example:
```js
match (x) {
  _ => {}
}
```

## Variable declaration patterns

Variable declaration patterns, like `const name`, take whatever value is at that position and assign it to a new variable. Conditional check wise, they act like a wildcard and match everything.

Example:
```js
const e = match (x) {
  const x => x,
};
```

While `let` variables are also supported by the runtime, these are a type error for now and only `const` variables are allowed. If you have a use case, please share it with the team.

## Object patterns

Object patterns match object values with the same structure. For example, the pattern `{type: 'light', num: 42}` matches objects which have the property `type` with value `'light'`, and a `num` property with value `42`.  If the object value has additional unlisted properties, or is inexact, you need to make your pattern inexact using `...`, for example `{type: 'light', num: 42, ...}`.

Like destructuring, a variable declaration pattern nested inside an object pattern creates a new variable with the value of that property. E.g. `match (arg) { {prop: const x} => x }` will initialize `x` with the value `arg.prop`. You can use object rest to gather the rest of the object’s own properties: `{foo: 1, ...const rest}`.

Doing just `{name}` is ambiguous \- it could mean `{name: name}` (matching against the value of variable `name`) or `{name: const name}` (extracting the property’s value as a new variable called `name`), so it’s not allowed. If you want a shorthand for creating new variables, you can use `{const name}` to mean `{name: const name}`.

If you have a property with either a wildcard or variable declaration pattern, e.g. `{foo: _}` or `{foo: const x}`, it is checked that the property is [in](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in) the object (checks own and non-own properties).

When checking objects with optional properties, in order to make the check exhaustive you must include a pattern that doesn’t include the optional properties. For example, for `{name: string, age?: number}`, if you first match with the pattern `{name: _, age: _}`, you still need to match with the pattern `{name: _, ...}` in order to handle the cases where the `age` property doesn’t exist.

Property names can be identifiers (e.g. `foo: pattern`), string literals (e.g. `'foo': <pattern>)`, or number literals (e.g. `2: <pattern>`). Repeated object keys are banned, and BigInts are not supported as object keys (Flow doesn’t yet support them).

Example:
```js
const e = match (x) {
  {foo: 1, bar: const a} => a,
  {const baz} => baz,
  {foo: 2, ...const rest} => rest,
};
```

[Getters](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/get) are also not supported \- at runtime they will be evaluated multiple times, once for each conditional check done against them. Flow doesn’t support getters anyway.

## Array patterns

Array patterns match both tuple and array values. For example, the pattern `['small', true]` matches array values whose length is `2` and whose elements match the pattern’s elements. If you want a looser length check, you can make the pattern inexact using `...`, for example `['small', true, ...]` will match arrays whose length is `>= 2` and whose first elements match the pattern’s elements. You can match any array or tuple with `[...]`.

Like destructuring, a variable declaration pattern nested inside an array pattern creates a new variable with the value of that element.  E.g. `match (arg) { [const x] => x }` will initialize `x` with the value `arg[0]`. You can use array rest at the end of the pattern to gather the remaining elements, and check that the length is greater or equal to the pattern length: e.g. `[1, 2, ...const rest]`.

Array patterns match values which pass `Array.isArray`, so they won’t match array-like objects that aren’t actually arrays, and won’t match iterables. Use `Array.from` on those types of values first first if you want to match them with array patterns.

Example:
```js
const e = match (x) {
  [1, 2] => [],
  [3, 4, ...] => [],
  [5, 6, ...const rest] => rest,
};
```

## “Or” patterns

Or patterns allow you to combine multiple patterns using `|`, for example `'active' | 'paused'` will match either string literal.

[Variable declaration patterns](#variable-declaration-patterns) inside of "or" patterns are not yet supported.

Example:
```js
match (x) {
  1 | 2 => {}
  [4] | [5] => {}
}
```

## “As” patterns

As patterns both match a pattern and create a new variable. For example, `[_, _] as pair` will first match any arrays whose length is `2`, and then assign that value to a new variable called `pair`. The syntax `as const pair` also works, we’ll decide which one to keep based on feedback.

Example:
```js
const e = match (x) {
  // Either one works for now
  [2, _] as x => x,
  [1, _] as const x => x,
};
```
