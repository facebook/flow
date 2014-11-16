---
id: dynamic-type-tests
title: Dynamic Type Tests
layout: docs
permalink: /docs/dynamic-type-tests.html
prev: operators.html
---

In addition to `null` checks, there are several other dynamic type tests
(//predicates//) on local variables that Flow recognizes and uses to refine
types. Refining a type with a predicate means narrowing the original type with
the type satisfied by values satisfying the predicate. Some of these
predicates are:

- `typeof x == 'string'`, `typeof x == 'number'`, and so on: narrows the type
  of `x` with `string`, `number`, and so on;
- `x instanceof A`: narrows the type of `x` with `A`;
- combinations of simpler predicates with logical operators `&&`, `||`, and
  `!:` narrows the types of local variables appearing in the predicates with
  types describing the union, intersection, or complement of values satisfying
  those predicates.

For example, the following code typechecks:

```javascript
/* @flow */
function foo(b) { if (b) { return 21; } else { return ''; } }
function bar(b): number {
  var x = foo(b);
  var y = foo(b);
  if (typeof x == 'number' && typeof y == 'number') { return x + y; }
  return 0;
}
```

In this code, the return type of `foo` can be `number` or `string`.
However, the use of `+` is guaranteed to return `number` since both of its
arguments are dynamically tested to be `number`. Thus, irrespective of whether
`b` is `true` or `false`, the return type of `bar` is always
`number`. (Without path-sensitivity, Flow would conclude that the arguments to
`+` could be `number` or `string`, and so the return type of `bar` would also
be `number` or `string`.)
