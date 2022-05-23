---
title: Announcing Disjoint Unions
short-title: Disjoint Unions
author: Avik Chaudhuri
hide_table_of_contents: true
---

Sometimes programs need to deal with different kinds of data all at once, where the shape of the data can be different based on what kind of data the code is looking at. This kind of programming is so common in functional programming languages that almost all such languages come with a way of:

* Specifying such data by a set of disjoint cases, distinguished by “tags”, where each tag is associated with a different “record” of properties. (These descriptions are called “disjoint union” or “variant” types.)
* Doing case analysis on such data, by checking tags and then directly accessing the associated record of properties. (The common way to do such case analysis is by pattern matching.)

Examples of programs that analyze or transform such data range from compilers working with abstract syntax trees, to operations that may return exceptional values,  with much more in between!

As of Flow 0.13.1 it is now possible to program in this style in JavaScript in a type-safe manner. You can define a disjoint union of object types and do case analysis on objects of that type by switching on the value of some common property (called a "sentinel") in those object types.

Flow's syntax for disjoint unions looks like:

```javascript
type BinaryTree =
  { kind: "leaf", value: number } |
  { kind: "branch", left: BinaryTree, right: BinaryTree }

function sumLeaves(tree: BinaryTree): number {
  if (tree.kind === "leaf") {
    return tree.value;
  } else {
    return sumLeaves(tree.left) + sumLeaves(tree.right);
  }
}
```

<!--truncate-->

## The problem

Consider the following function that returns different objects depending on the data passed into it:

```javascript
type Result = { status: string, errorCode?: number }

function getResult(op): Result {
  var statusCode = op();
  if (statusCode === 0) {
    return { status: 'done' };
  } else {
    return { status: 'error', errorCode: statusCode };
  }
}
```

The result contains a `status` property that is either `'done'` or `'error'`,
and an optional `errorCode` property that holds a numeric status code when the
`status` is `'error'`.

One may now try to write another function that gets the error code from a result:

```javascript
function getErrorCode(result: Result): number {
  switch (result.status) {
    case 'error':
      return result.errorCode;
    default:
      return 0;
  }
}
```

Unfortunately, this code does not typecheck. The `Result` type does not precisely
capture the relationship between the `status` property and the `errorCode` property.
Namely it doesn't capture that when the `status` property is `'error'`, the `errorCode`
property will be present and defined on the object. As a result, Flow thinks that
`result.errorCode` in the above function may return `undefined` instead of `number`.

Prior to version 0.13.1 there was no way to express this relationship, which meant
that it was not possible to check the type safety of this simple, familiar idiom!

## The solution

As of version 0.13.1 it is possible to write a more precise type for `Result`
that better captures the intent and helps Flow narrow down the possible shapes
of an object based on the outcome of a dynamic `===` check. Now, we can write:

```javaScript
type Result = Done | Error
type Done = { status: 'done' }
type Error = { status: 'error', errorCode: number }
```

In other words, we can explicitly list out the possible shapes of results. These
cases are distinguished by the value of the `status` property. Note that here
we use the string literal types `'done'` and `'error'`. These match exactly the strings
`'done'` and `'error'`, which means that `===` checks on those values are enough for
Flow to narrow down the corresponding type cases. With this additional reasoning, the
function `getErrorCode` now typechecks, without needing any changes to the code!

In addition to string literals, Flow also supports number literals as singleton types
so they can also be used in disjoint unions and case analyses.

## Why we built this

Disjoint unions are at the heart of several good programming practices pervasive in functional programming languages. Supporting them in Flow means that JavaScript can use these practices in a type-safe manner. For example, disjoint unions can be used to write type-safe [Flux dispatchers](https://facebook.github.io/flux/docs/dispatcher.html). They are also heavily used in a recently released [reference implementation of GraphQL](https://github.com/graphql/graphql-js).
