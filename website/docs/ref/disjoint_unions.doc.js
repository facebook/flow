/* @flow */
/*
---
id: disjoint-unions
title: Disjoint Unions
permalink: /docs/disjoint-unions.html
prev: dynamic-type-tests.html
next: modules.html
---
*/

/*
  Sometimes programs need to deal with different kinds of data all at once,
  where the shape of the data can be different based on what kind of data the
  code is looking at. This kind of programming is so common in functional
  programming languages that almost all such languages come with a way of:

  * Specifying such data by a set of disjoint cases, distinguished by "tags,"
  where each tag is associated with a different "record" of properties. (These
  descriptions are called "disjoint union" or "variant" types.)

  * Doing case analysis on such data, by checking tags and then directly
  accessing the associated record of properties. (The common way to do such case
  analysis is by pattern matching.)

  Examples of programs that analyze or transform such data range from compilers
  working with abstract syntax trees, to operations that may return exceptional
  values, with much more in between!

  With Flow, it is possible to program in this style in JavaScript in a
  type-safe manner. You can define a disjoint union of object types and do case
  analysis on objects of that type by switching on the value of some common
  property (called a "sentinel") in those object types.

  ## Example

  Consider a computation that takes an argument and a callback that is invoked
  when the computation is complete, with a success or failure result. As a
  concrete example, suppose that we are interested in computing the inverse of a
  matrix (which may or may not exist).

  The result type can be modeled as a disjoint union type. In terms of syntax, a
  disjoint union type is just a specific form of [union
  type](union-intersection-types.html). In addition, a disjoint union type makes
  heavy use of [literal types](builtins.html#literal-types): this is important,
  because we will see below how Flow can relate these types back to dynamic
  equality checks with literals that appear commonly in JavaScript code.
*/
type Matrix = number[][]; // type of input and output for our function

type Result = Done | Error; // a disjoint union type with two cases
type Done = { status: 'done', answer: Matrix };
type Error = { status: 'error', message: string };
/*
  Here, the property `status` serves as the sentinel property in `Result`: this
  is the property that actually makes the union type "disjoint."

  Case analysis can be performed on results of type `Result` by matching the
  value of the `status` property with the literals `done` or `error`. Note that
  matching must be performed with the `===` operator. Conveniently, `switch`
  statements already use this operator, but using `if` with this operator also
  works.

  Flow will correctly narrow down the `Result` type to the corresponding cases,
  `Done` and `Error`, so that the properties `answer` and `message` can be
  accessed safely based on context.
*/
function invert(matrix: Matrix, callback: (result: Result) => void) {
  // Try to compute the inverse of `matrix`.
  // On success, invoke callback with `{ status: 'done', answer: ... }`.
  // On failure, involve callback with `{ status: 'error', message: ... }`.

  // ...
}

function showInverseOf(matrix: Matrix) {
  invert(matrix, result => {
    switch (result.status) {
    case 'error':
      console.log ("Uh oh!", result.message);
      break;  // see what happens if you forget to break! :)
    default:
      console.log ("Inverse:", result.answer);
    }
  });
}
