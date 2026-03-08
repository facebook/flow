---
title: Match Expressions and Statements
description: "match an input value against a series of patterns, which conditionally check the structure of the input and extract values, and either produce an expression (match expressions) or execute a block (match statements)."
slug: /match
---

*An experimental Flow language feature. See [adoption](#adoption) for how to enable.*

`match` an input value against a series of [patterns](./patterns), which conditionally check the structure of the input and extract values, and either produce an expression (`match` expressions) or execute a block (`match` statements).

You can [replace `switch` statements](./migration#replacing-switch) using `match`, avoiding the problems associated with `switch` (like fall-through behavior) while gaining benefits like [exhaustiveness checks](#exhaustive-checking) and complex pattern support.

You can also [replace nested conditional ternary expressions](./migration#replacing-conditional-ternary-expressions) using `match` expressions, making your code much more readable while gaining the power of `match`.

```js
component AgePane(
  maybeAge?: {type: 'valid', age: number} | {type: 'error', msg: string}
) {
  return match (maybeAge) {
    {type: 'valid', const age} if (age < 0) =>
      <Err>Age cannot be negative!</Err>,
    {type: 'valid', const age} => <Pane>Age: {age}</Pane>,
    {type: 'error', const msg} => <Err>{msg}</Err>,
    undefined => <Err>Age is not defined.</Err>,
  };
}
```

## Match Expressions

Match expressions allow you to define conditional logic as an expression. They can replace nested conditional ternary expressions.

A match expression is made up of its argument and a series of cases, each which define a pattern and an expression body. Each pattern is checked in sequence, and the resulting expression is the one accompanying the matching pattern. The resulting expression is typed as the union of every case expression type.

A pattern can be followed by a guard, with the `if (<cond-expression>)` syntax. If the pattern matches, the expression in the guard is also executed, and the entire case only matches if the result is truthy.

Example structure:
```js
const e = match (<arg>) {
  <pattern-1> => <expression-1>,
  <pattern-2> if (<cond>) => <expression-2>,
  <pattern-3> => <expression-3>,
};
```

The guard applies to the entire pattern, including ["or" patterns](./patterns#or-patterns), e.g. `1 | 2 if (cond)` will first match if the value is `1 | 2`, and then finally succeed if `cond` is also true. Guarded cases do not count toward [exhaustiveness checks](#exhaustive-checking), since they may or may not match based on the condition.

You can initialize two or more variables using a `match`:
```js
// Using a tuple:
const [color, size] = match (status) {
  Status.Active => ['green', 2],
  Status.Paused => ['yellow', 1],
  Status.Off => ['red', 0],
};

// Using an object (especially useful for more than two variables):
const {color, size} = match (status) {
  Status.Active => {color: 'green', size: 2},
  Status.Paused => {color: 'yellow', size: 1},
  Status.Off => {color: 'red', size: 0},
};
```

Match expressions cannot be used in an expression statement position, as that is reserved for match statements.

```js
match (<arg>) {} //  This is a match statement, not a match expression
```

If no pattern matches, Flow will error due to a non-exhaustive match, and an exception will be thrown at runtime. You can use a [wildcard](./patterns#wildcard-patterns) (`_`) or [variable declaration pattern](./patterns#variable-declaration-patterns) (`const x`) as the last case of a match to catch all remaining possible matches.

To throw an exception in a `match` expression case body you can't use `throw` as it is a statement, and `match` expressions require expression bodies. Instead, you can use `invariant(false, <msg>)`, which Flow understands will always throw (with the supplied message).

Match expression case bodies do not yet support usage of `yield`, `yield*`, or `await`. Unlike match expressions, [match statements](#match-statements) do support these keywords.

*Fine print:* The opening brace `{` is required to be on the same line as the match argument `match (<arg>)`. This way, we can introduce this feature in a way that is backwards compatible with all existing syntax: `match(x);` is still a call to a function called `match`. Prettier will automatically format match expressions in this way.

## Match Statements

Match statements can replace `switch` statements or chained `if`\-`else` statements. Similar to match expressions, they have an argument and a series of cases. The difference is each case body is a block (i.e. `{ ...statements... }`) instead of an expression, and the construct is a statement so it does not result in a value. No `break` needed: the cases don’t fall\-through (but you can still combine multiple patterns using ["or" patterns](./patterns#or-patterns) `|`).

Example structure:
```js
match (<arg>) {
  <pattern-1> => {
    <statements-1>;
  }
  <pattern-2> if (<cond>) => {
    <statements-2>;
  }
  <pattern-3> => {
    <statements-3>;
  }
}
```

*Fine print:* Like match expressions, the opening brace `{` is required to be on the same line as the match argument `match (<arg>)`.

## Exhaustive Checking

`match` requires that you have considered all cases of the input. If you don't, Flow will error and tell you what patterns you could add to make the match exhaustive:

```js flow-check
declare const tab: 'home' | 'details' | 'settings';

match (tab) { // ERROR
  'home' => {}
  'settings' => {}
}
```

Checks on [disjoint object unions](../types/unions#toc-disjoint-object-unions) are supported.
```js flow-check
declare const result: {type: 'ok', value: number} | {type: 'error'};

match (result) { // ERROR
  {type: 'error'} => {}
}
```

It even works for nested structures:

```js flow-check
function getStyle(
  align: 'start' | 'end',
  position: 'above' | 'below',
) {
  return match ([align, position]) { // ERROR
    ['start', 'above'] => 0,
    ['start', 'below'] => 1,
    ['end', 'above'] => 2,
  };
}
```

Flow will also error if a pattern is unused:

```js flow-check
declare const tab: 'home' | 'details' | 'settings';

match (tab) {
  'home' => {}
  'details' => {}
  'settings' => {}
  'invalid' => {} // ERROR
}
```

## More

Learn more about [match patterns](./patterns), including primitive value patterns, array and object patterns, variable declaration patterns, and “or” and “as” patterns.

You can also [migrate from existing patterns](./migration) like `switch` or conditional ternary expressions.

## Adoption

* Flow: 0.274.1+ with option: `experimental.pattern_matching=true`
* Babel: use the [babel-plugin-syntax-hermes-parser](https://www.npmjs.com/package/babel-plugin-syntax-hermes-parser) plugin version 0.29+, see our [Babel guide](../tools/babel) for more details.
* ESLint: use [hermes-eslint](https://www.npmjs.com/package/hermes-eslint) plugin version 0.29+, see our [ESLint guide](../tools/eslint) for more details.
