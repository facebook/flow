---
title: Functions
slug: /types/functions
description: "How to type functions in Flow, including parameters, return types, rest parameters, generics, and callable objects."
---

Functions have two places where types are applied: parameters (input) and the return value (output).

```js flow-check
function concat(a: string, b: string): string {
  return a + b;
}

concat("foo", "bar"); // Works!
concat(true, false);  // Error!
```

Using inference, return types are often optional:

```js flow-check
function concat(a: string, b: string) {
  return a + b;
}

const s: string = concat("foo", "bar"); // Works!
```

If defined where we can get the type from the context of the expression, type annotations can be optional:

```js flow-check
[1, 2, 3].map(x => x * x); // From the context, we know parameter `x` has type `number`
```

## Syntax of functions

There are three forms of functions that each have their own slightly different syntax.

### Function Declarations

```js flow-check
function func(str: string, bool?: boolean, ...nums: Array<number>): void {
  // ...
}
```

### Arrow Functions {#toc-arrow-functions}

```js flow-check
let func = (str: string, bool?: boolean, ...nums: Array<number>): void => {
  // ...
};
```

### Function Types

```js flow-check
type T = (str: string, bool?: boolean, ...nums: Array<number>) => void;
```

You may also optionally leave out the parameter names.

```js flow-check
type T = (string, boolean | void, Array<number>) => void;
```

You might use these functions types for something like a callback.

```js flow-check
function func(callback: (error: Error | null, value: string | null) => void) {
  // ...
}
```

### Type arguments

Functions can have type arguments:

```js flow-check
function f<T>(x: T): Array<T> {
  return [x];
}

const g = <T>(x: T): Array<T> => [x];

type H = <T>(T) => Array<T>;
```

## Function Parameters

Function parameters can have types by adding a colon `:` followed by the type
after the name of the parameter.

```js flow-check
function func(param1: string, param2: boolean) {
  // ...
}
```

### Optional Parameters

You can also have optional parameters by adding a question mark `?` after the
name of the parameter and before the colon `:`.

```js flow-check
function func(optionalValue?: string) {
  // ...
}
```

Optional parameters will accept missing, `undefined`, or matching types. But
they will not accept `null`.

```js flow-check
function func(optionalValue?: string) {
  // ...
}

func();          // Works.
func(undefined); // Works.
func("string");  // Works.

func(null);      // Error!
```

### Rest Parameters

JavaScript also supports having rest parameters or parameters that collect an
array of arguments at the end of a list of parameters. These have an ellipsis
`...` before them.

You can also add type annotations for rest parameters using the same syntax but
with an `Array`.

```js flow-check
function func(...args: Array<number>) {
  // ...
}
```

You can pass as many arguments as you want into a rest parameter.

```js flow-check
function func(...args: Array<number>) {
  // ...
}

func();        // Works.
func(1);       // Works.
func(1, 2);    // Works.
func(1, 2, 3); // Works.
```

> Note: If you add a type annotation to a rest parameter, it must always
> explicitly be an `Array` or `ReadonlyArray` type.

### `this` parameter

Every function in JavaScript can be called with a special context named `this`.
You can call a function with any context that you want. Flow allows you to annotate
the type for this context by adding a special parameter at the start of the function's parameter list:

```js flow-check
function func<T>(this: { x: T }) : T {
  return this.x;
}

const num: number = func.call({x : 42});
const str: string = func.call({x : 42}); // Error!
```

This parameter has no effect at runtime, and is erased along with types when Flow is transformed into JavaScript.
When present, `this` parameters must always appear at the very beginning of the function's parameter list, and must
have an annotation. Additionally, [arrow functions](./#toc-arrow-functions) may not have a `this` parameter annotation, as
these functions bind their `this` parameter at the definition site, rather than the call site.

If an explicit `this` parameter is not provided, Flow will attempt to infer one based on usage. If `this` is not mentioned
in the body of the function, Flow will infer `unknown` for its `this` parameter.


## Function Returns

Function returns can also add a type using a colon `:` followed by the type
after the list of parameters.

```js flow-check
function func(): number {
  return 1;
}
```

Return types ensure that every branch of your function returns the same type.
This prevents you from accidentally not returning a value under certain
conditions.

```js flow-check
function func(): boolean { // Error
  if (Math.random() > 0.5) {
    return true;
  }
}
```

Async functions implicitly return a promise, so the return type must always be a `Promise`.

```js flow-check
async function func(): Promise<number> {
  return 123;
}
```

You can also use a special return type to declare that your function refines the type of a parameter.
These are called [type guards](../type-guards):

```js flow-check
function isString(x: mixed): x is string {
  return typeof x === 'string';
}
```

## Callable Objects

Callable objects can be typed, for example:

```js flow-check
type CallableObj = {
  (number, number): number,
  bar: string,
  ...
};

function add(x: number, y: number) {
  return x + y;
}

add.bar = "hello world";

add as CallableObj;
```

In general, functions can have properties assigned to them if they are function declarations, or
simple variable declarations of the form `const f = () => ...`. The properties must be assigned in
the format `f.prop = <expr>;`, in the same statement list as the function definition (i.e. not conditionally).

Note that the object representing the static properties assigned to the function is inexact.

## Overloaded functions
You can use intersection types to define [overloaded function types](../intersections/#toc-intersection-of-function-types):

```js flow-check
declare const fn:
  & ((x: 'string') => string)
  & ((x: 'number') => number)

const s: string = fn('string');
const n: number = fn('number');
```

## Any function

If you want to specify you want to allow any function, and do not care what it is, you can use this pattern:

```js flow-check
function useCallback<T: (...ReadonlyArray<empty>) => unknown>(
  callback: T,
  inputs: ?ReadonlyArray<unknown>,
): T {
  return callback;
}
useCallback((x: string) => true); // OK
useCallback((x: number) => [1]); // OK
```

You could use type arguments to capture the arguments and return type, to do more complicated transformations:

```js flow-check
function func<TArgs: ReadonlyArray<unknown>, TReturn>(
  callback: (...TArgs) => TReturn,
): (boolean, ...TArgs) => Array<TReturn> {
  return (b, ...args): Array<TReturn> => {
    if (b) {
      return [callback(...args)];
    } else {
      return [];
    }
  };
}

const f: (boolean, string, number) => Array<string> =
  func((x: string, y: number) => x.slice(y)); // OK
```

The type `Function` is just an alias for [`any`](../any), and is unsafe.
You can ban its use in your code with the [unclear-type lint](../../linting/rule-reference/#toc-unclear-type).

## See Also {#toc-see-also}

- [Generics](../generics) — parameterized types used with functions for type-safe reusable code
- [Type Guards](../type-guards) — special return types that refine parameter types
- [Intersections](../intersections) — defining overloaded function types
