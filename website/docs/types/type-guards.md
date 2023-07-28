---
title: Type Guards
slug: /types/type-guards
---

Flow lets you define functions whose return expression encodes some type predicate over a parameter `param`. This predicate is annotated in place of a return type annotation as `param is PredicateType`. It declares that if the function returns `true` then `param` is of type `PredicateType`.

The syntax for a function like this is:
```js
function predicate(param: InputType): param is PredicateType {
  return <some_expression>;
}
```
The type of this function can also be written in terms of a type guard annotation:
```js
type PredicateFunc = (param: InputType) => param is PredicateType;
```


## Basic Usage {#toc-basic-usage}

Let's see a simple example where we define a type guard function and then use it to refine some values.

### Defining a type guard function

```js flow-check
type A = { type: "A"; data: string };
type B = { type: "B"; data: number };
type AorB = A | B;

function isA(value: AorB): value is A {
  return value.type === "A";
}
```
We have defined a data type `AorB` that is a disjoint union of two types `A` and `B` that each have a property `type` used as tag.

We have also written a *user defined type guard* function `isA` defined over objects of type `AorB`. This function returns `true` when the value of of the `type` property of its input is `"A"`. Using the definitions of `A` and `B`, Flow can prove that when the value of `type` is `"A"` then the type of `value` will be `A`.

### Using a type guard function to refine values

Functions that have a declared type guard can be used to refine values in conditionals. In the example above, we can use `isA` to refine a variable of type `AorB` to just `A`:

```js flow-check
type A = { type: "A"; data: string };
type B = { type: "B"; data: number };
type AorB = A | B;

function isA(value: AorB): value is A {
  return value.type === "A";
}

function test(x: AorB) {
  if (isA(x)) {
    // `x` has now been refined to type A.
    // We can assign it variables of type A ...
    const y: A = x;
    // ...and access A's properties through `x`
    const stringData: string = x.data;

    // As a sanity check, the following assignment to B will error
    const error: B = x;
  }
}
```
In the then-branch of the conditional `if (isA(x))`, `x` will have the type `A`.




## Refine with `Array.filter`

Flow recognizes when you call `filter` on an array of type `Array<T>` with a callback function that holds a type guard with type `(value: T) => value is S`.
It will use this to produce an output array of type `Array<S>`. Note that `S` needs to be a subtype of the type of the array element `T`.

For example
```js flow-check
type Success = $ReadOnly<{type: 'success', value: 23}>;
type Error = $ReadOnly<{type: 'error', error: string}>;

type Response =
  | Success
  | Error

function filterSuccess(response: Array<Response>): Array<Success> {
  return response.filter(
    (response): response is Success => response.type === 'success'
  );
}

function filterError1(response: Array<Response>): Array<Error> {
  const result = response.filter(
    (response): response is Success => response.type === 'success'
  );
  // The following is expected to error
  return result;
}

function filterError2(response: Array<Response>): Array<Error> {
  const result = response.filter(
    // The following is expected to error
    (response): response is Error => response.type === 'success'
  );
  return result;
}
```
In `filterError1`, filtering produces `Array<Success>` that is not compatible with the expected return type `Array<Error>`.

In `filterError2`, the predicate `response.type === 'success'` is used to refine `Response`s to `Success`s, not `Error`s.


## Defining Type Guard Functions {#toc-restrictions-of-type-guard-functions}

To ensure that refinement with type guard functions is sound, Flow runs a number of checks associated with these functions.

### Predicate parameter is a regular parameter to the function

In a type guard annotation of the form `parameter is Type`, `parameter` needs to belong to the current function's parameter list.
```js flow-check
function missing(param: mixed): prop is number {
  return typeof param === "number";
}
```

It cannot be a parameter bound in a destructuring pattern, or a rest paramter:
```js flow-check
function destructuring({prop}: {prop: mixed}): prop is number {
  return typeof prop === "number";
}
```
```js flow-check
function rest(...value: Array<mixed>): value is Array<mixed> {
  return Array.isArray(value);
}
```
### Predicate type is consistent with the parameter type

The type guard `Type` needs to be compatible with the type of the parameter. In other words, given a definition
```js
function isT(x: ParamType): x is Type {
  return ...
}
```
Flow will check that `Type` is a subtype of `ParamType`. So the following will be an error:
```js flow-check
function isNumber(x: string): x is number {
  return typeof x === "number";
}
```

### Type guard function returns boolean

A type guard function needs to return a boolean expression. The following are invalid declarations:
```js flow-check
function isNumberNoReturn(x: string): x is string {}
```
```js flow-check
function nonMaybe<V: {...}>(x: ?V): x is V {
  return x;
}
```
A correct version of `nonMaybe` would be
```js flow-check
function nonMaybe<V: {...}>(x: ?V): x is V {
  return !!x;
}
```

### Predicate type is consistent with refined type

In addition to the above checks, Flow also ensures that the declared type guard is consistent with the check happening in the body of the function. To establish this it needs to guarantee two things:

1. The type of the refined parameter at the return location *after* the predicate of the return expression has been applied is a subtype of the guard type. For example, the following definitions are correct:
```js flow-check
function numOrStr(x: mixed): x is number | string {
  return (typeof x === "number" || typeof x === "string");
}

function numOrStrWithException(x: mixed): x is number | string {
  if (typeof x === "number") {
    return true;
  } else {
    if (typeof x === "string") {
        return true;
    } else {
        throw new Error("");
    }
  }
}
```
But in the following Flow will raise errors:
```js flow-check
function numOrStrError(x: mixed): x is number | string {
  return (typeof x === "number" || typeof x === "boolean");
}
```

2. The parameter that is refined cannot be reassigned in the body of the type guard function. Therefore the following are errors:
```js flow-check
function isNumberError1(x: mixed): x is number {
  x = 1;
  return typeof x === "number";
}
```
```js flow-check
function isNumberError2(x: mixed): x is number {
  function foo() {
    x = 1;
  }
  foo();
  return typeof x === "number";
}
```


## Adoption {#toc-adoption}

To use type guards, you need to upgrade your infrastructure so that it supports the syntax:

- `flow` and `flow-parser`: 0.209.1. Between v0.209.1 to v0.211.1, you need to explicitly enable it in your .flowconfig, under the `[options]` heading, add `type_guards=true`.
- `prettier`: 3
- `babel` with `babel-plugin-syntax-hermes-parser`. See [our Babel guide](../../tools/babel/) for setup instructions.
- `eslint` with `hermes-eslint`. See [our ESLint guide](../../tools/eslint/) for setup instructions.
