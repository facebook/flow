/* @flow */
/*
---
id: dynamic-type-tests
title: Dynamic Type Tests
permalink: /docs/dynamic-type-tests.html
prev: typeof.html
next: disjoint-unions.html
---
*/

/*
  Flow understands many idiomatic constructs used to determine the type of a
  value at runtime, and incorporates that knowledge into its static analysis.

  There are several dynamic type tests (predicates) on local variables that Flow
  recognizes and uses to refine types. Refining a type with a predicate means
  narrowing the original type with the type satisfied by values satisfying the
  predicate.

  Type tests can occur in `if` and `switch` statements, the test block in loop
  constructs like `for`, `for-in`, `for-of`, and `do-while`, conditional
  expressions (ternary statements), and inline logical expressions like `a &&
  a.b`.

  ### Maybe, Null, and Undefined
*/

function maybe_test(x: ?string): string {
  if (x == null) {
    // The condition will pass if `x` is `null` or `undefined`.
    // $ExpectError
    return x;
  } else {
    // In this branch, `x` must be a string.
    return x;
  }
}

function null_undefined_tests(x: ?string): string {
  if (x === null) {
    // The condition will pass if `x` is `null`.
    // $ExpectError
    return x;
  } else if (x === undefined) {
    // The condition will pass if `x` is `undefined`.
    // $ExpectError
    return x;
  } else {
    // In this branch, `x` must be a string.
    return x;
  }
}

/*
  Read more about <a href="/docs/nullable-types.html">Maybe Types</a>.

  ### Truthiness and Existence
*/

function boolean_truthiness(x: boolean): true {
  if (x) {
    // In this branch, `x` must be `true`.
    return x;
  } else {
    // Flow understands that `x` must be `false` in this branch, and therefore
    // that the expression !x must be `true`.
    return !x;
  }
}

function string_truthiness(x: string): "" {
  if (x) {
    // Flow understands that `x` can be any non-empty string in this branch.
    // $ExpectError
    return x;
  } else {
    // Flow understands that `x` can only be "" in this branch.
    return x;
  }
}

function number_truthiness(x: number): 0 {
  if (x) {
    // Flow understands that `x` can be any non-zero number in this branch.
    // $ExpectError
    return x;
  } else {
    // Flow understands that `x` can only be 0 in this branch.
    return x;
  }
}

function sketchy_null_check(x: ?string): string {
  // Since "" is not truthy, we will replace "" with "default" in this function.
  // Currently Flow does not complain about this pattern, but it's a common
  // request which may be added in the future.
  if (x) {
    return x;
  } else {
    return "default";
  }
}

/*
  ### `typeof`

  This type test is particularly useful in conjunction with <a
  href="/docs/union-intersection-types.html">union types</a>.
*/

function typeof_test(x: number | string): number {
  if (typeof x === "string") {
    // In this branch, `x` must be a string, and thus has a `length` method.
    return x.length;
  } else {
    // By deduction, `x` must be a number in this branch.
    return x;
  }
}

/*
  In JavaScript, `typeof null` is `"object"`, but don't worry, Flow won't let
  you make that common mistake. (Hint: use `x == null` instead.)
*/

function typeof_null(x: ?Object): Object {
  if (typeof x === "object") {
    // $ExpectError
    return x; // x can still be null
  } else {
    return {};
  }
}

/*
  ### Array.isArray
*/

type NestedArray<T> = Array<T|NestedArray<T>>

function flatten<T>(xs: NestedArray<T>): Array<T> {
  let result = [];
  for (let x of xs) {
    if (Array.isArray(x)) {
      // In this branch, `x` must be a `NestedArray<T>`
      result.push(...flatten(x));
    } else {
      // By deduction, `x` must be a `T` in this branch.
      result.push(x);
    }
  }
  return result;
}

/*
  ### A instanceof B
*/

declare function businessLogic(x: string): void;

function myEventHandler(e: Event) {
  // We only know that e.target is an EventTarget
  // $ExpectError
  e.target.value;
  if (e.target instanceof HTMLInputElement) {
    // Now we know it's an <input />, with a `value` property.
    businessLogic(e.target.value);
  } else {
    // error handling
  }
}

/*
  ### Tagged Unions
*/

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

/*
  ## Caveats

  Flow is pessimistic about refinements. If it is possible that a refinement may
  become invalid, Flow will throw away the refinement. This can often happen
  when invoking a function that might refer to the refined value.
*/

declare function something(): void;

function foo(x: { y: ?string }): string {
  if (x.y) {
    something();
    // $ExpectError
    return x.y; // error: x.y may be null/undefined
  } else {
    return "default";
  }
}

/*
  In the above code, `something` might mutate `x`, invalidating the refinement.
  It is unsafe to expect that `x.y` will always be a string after calling this
  function. It is simple to work around this, however. You can copy the object's
  property value to a local variable, which can't be mutated from the outside.
*/

declare function something(): void;

function foo(x: { y: ?string }): string {
  if (x.y) {
    var y = x.y;
    something();
    return y; // OK: something couldn't have changed y
  } else {
    return "default";
  }
}

/*
  Another way to help Flow keep a refinement is to use a `const` binding.
*/

function foo(x: ?string) {
  if (x) {
    () => {
      // We don't know when this function will be invoked, and `null` might be
      // written to `x` before it is.
      // $ExpectError
      (x: string);
    }
  }

  const const_x = x;
  if (const_x) {
    () => {
      // Regardless of when this function is invoked, `null` can never be
      // written to `const_x`, so we can keep the refinement.
      (const_x: string);
    }
  }
}

/*
  In some cases, Flow will throw away a refinement that is always safe to keep.
  In the following example, Flow doesn't have enough information to realize that
  `console.log` will not mutate `x`.
*/

function foo(x: { y: ?string }): string {
  if (x.y) {
    console.log("*obviously* this doesn't mutate x");
    // $ExpectError
    return x.y; // error: Flow doesn't know that
  } else {
    return "default";
  }
}

/*
  Flow does perform a mutation analysis and where it is safe to do so, will
  preserve refinements after function calls which it knows do not invalidate the
  refinement.
*/

function bar(x: ?string): string {
  function baz() { /* this doesn't mutate x */ }
  if (x) {
    baz();
    return x; // Flow understands that `baz` can't invalidate the refinement.
  } else {
    return "default";
  }
}
