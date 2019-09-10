---
layout: guide
---

Functions have two places where types are applied: Parameters (input) and the
return value (output).

```js
// @flow
function concat(a: string, b: string): string {
  return a + b;
}

concat("foo", "bar"); // Works!
// $ExpectError
concat(true, false);  // Error!
```

Using inference, these types are often optional:

```js
// @flow
function concat(a, b) {
  return a + b;
}

concat("foo", "bar"); // Works!
// $ExpectError
concat(true, false);  // Error!
```

Sometimes Flow's inference will create types that are more permissive than you
want them to be.

```js
// @flow
function concat(a, b) {
  return a + b;
}

concat("foo", "bar"); // Works!
concat(1, 2);         // Works!
```

For that reason (and others), it's useful to write types for important
functions.

## Syntax of functions <a class="toc" id="toc-syntax-of-functions" href="#toc-syntax-of-functions"></a>

There are three forms of functions that each have their own slightly different syntax.

### Function Declarations <a class="toc" id="toc-function-declarations" href="#toc-function-declarations"></a>

Here you can see the syntax for function declarations with and without types
added.

```js
function method(str, bool, ...nums) {
  // ...
}

function method(str: string, bool?: boolean, ...nums: Array<number>): void {
  // ...
}
```

### Arrow Functions <a class="toc" id="toc-arrow-functions" href="#toc-arrow-functions"></a>

Here you can see the syntax for arrow functions with and without types added.

```js
let method = (str, bool, ...nums) => {
  // ...
};

let method = (str: string, bool?: boolean, ...nums: Array<number>): void => {
  // ...
};
```

### Function Types <a class="toc" id="toc-function-types" href="#toc-function-types"></a>

Here you can see the syntax for writing types that are functions.

```js
(str: string, bool?: boolean, ...nums: Array<number>) => void
```

You may also optionally leave out the parameter names.

```js
(string, boolean | void, Array<number>) => void
```

You might use these functions types for something like a callback.

```js
function method(callback: (error: Error | null, value: string | null) => void) {
  // ...
}
```

## Function Parameters <a class="toc" id="toc-function-parameters" href="#toc-function-parameters"></a>

Function parameters can have types by adding a colon `:` followed by the type
after the name of the parameter.

```js
function method(param1: string, param2: boolean) {
  // ...
}
```

## Optional Parameters <a class="toc" id="toc-optional-parameters" href="#toc-optional-parameters"></a>

You can also have optional parameters by adding a question mark `?` after the
name of the parameter and before the colon `:`.

```js
function method(optionalValue?: string) {
  // ...
}
```

Optional parameters will accept missing, `undefined`, or matching types. But
they will not accept `null`.

```js
// @flow
function method(optionalValue?: string) {
  // ...
}

method();          // Works.
method(undefined); // Works.
method("string");  // Works.
// $ExpectError
method(null);      // Error!
```

### Rest Parameters <a class="toc" id="toc-rest-parameters" href="#toc-rest-parameters"></a>

JavaScript also supports having rest parameters or parameters that collect an
array of arguments at the end of a list of parameters. These have an ellipsis
`...` before them.

You can also add type annotations for rest parameters using the same syntax but
with an `Array`.

```js
function method(...args: Array<number>) {
  // ...
}
```

You can pass as many arguments as you want into a rest parameter.

```js
// @flow
function method(...args: Array<number>) {
  // ...
}

method();        // Works.
method(1);       // Works.
method(1, 2);    // Works.
method(1, 2, 3); // Works.
```

> Note: If you add a type annotation to a rest parameter, it must always
> explicitly be an `Array` type.

### Function Returns <a class="toc" id="toc-function-returns" href="#toc-function-returns"></a>

Function returns can also add a type using a colon `:` followed by the type
after the list of parameters.

```js
function method(): number {
  // ...
}
```

Return types ensure that every branch of your function returns the same type.
This prevents you from accidentally not returning a value under certain
conditions.

```js
// @flow
// $ExpectError
function method(): boolean {
  if (Math.random() > 0.5) {
    return true;
  }
}
```

Async functions implicitly return a promise, so the return type must always be a `Promise`.

```js
// @flow
async function method(): Promise<number> {
  return 123;
}
```

### Function `this` <a class="toc" id="toc-function-this" href="#toc-function-this"></a>

Every function in JavaScript can be called with a special context named `this`.
You can call a function with any context that you want.

In Flow you don't type annotate `this` and Flow will check whatever context you
call the function with.

```js
function method() {
  return this;
}

var num: number = method.call(42);
// $ExpectError
var str: string = method.call(42);
```

### Predicate Functions <a class="toc" id="toc-predicate-functions" href="#toc-predicate-functions"></a>

Sometimes you will want to move the condition from an `if` statement into a function:

```js
function concat(a: ?string, b: ?string): string {
  if (a && b) {
    return a + b;
  }
  return '';
}
```

However, Flow will flag an error in the code below:

```js
function truthy(a, b): boolean {
  return !!a && !!b;
}

function concat(a: ?string, b: ?string): string {
  if (truthy(a, b)) {
    // $ExpectError
    return a + b;
  }
  return '';
}
```

This is because the refinement information of `a` and `b` as `string` instead of `?string` is lost when returning from the `truthy` function.

In order to keep the refinement, you can make `truthy` a *predicate function*, by using the `%checks` annotation like so:

```js
function truthy(a, b): boolean %checks {
  return !!a && !!b;
}

function concat(a: ?string, b: ?string): string {
  if (truthy(a, b)) {
    return a + b;
  }
  return '';
}
```

The body of these predicate functions need to be expressions (i.e. local variable declarations are not supported).
But it's possible to call other predicate functions inside a predicate function.
For example:

```js
function isString(y): %checks {
  return typeof y === "string";
}

function isNumber(y): %checks {
  return typeof y === "number";
}

function isNumberOrString(y): %checks {
  return isString(y) || isNumber(y);
}

function foo(x): string | number {
  if (isNumberOrString(x)) {
    return x + x;
  } else {
    return x.length; // no error, because Flow infers that x can only be an array
  }
}

foo('a');
foo(5);
foo([]);
```

### Callable Objects <a class="toc" id="toc-callable-objects" href="#toc-callable-objects"></a>

Callable objects can be typed, for example:

```js
type CallableObj = {
  (number, number): number,
  bar: string
};

function add(x, y) {
  return x + y;
}

// $ExpectError
(add: CallableObj);

add.bar = "hello world";

(add: CallableObj);
```

### `Function` Type <a class="toc" id="toc-function-type" href="#toc-function-type"></a>

> NOTE: For new code prefer `any` or `(...args: Array<any>) => any`. `Function` has become an alias to `any` and will be
> deprecated and removed in a future version of Flow.

Sometimes it is useful to write types that accept arbitrary functions, for
those you should write `() => mixed` like this:

```js
function method(func: () => mixed) {
  // ...
}
```

However, if you need to opt-out of the type checker, and don't want to go all
the way to `any`, you can instead use `(...args: Array<any>) => any`. (Note that [`any`](../any/) is unsafe and
should be avoided). For historical reasons, the `Function` keyword is still avaiable.

For example, the following code will not report any errors:

```js
function method(func: (...args: Array<any>) => any) {
  func(1, 2);     // Works.
  func("1", "2"); // Works.
  func({}, []);   // Works.
}

method(function(a: number, b: number) {
  // ...
});
```

Neither will this:

```js
function method(obj: Function) {
  obj = 10;
}

method(function(a: number, b: number) {
  // ...
});
```

> ***You should follow [all the same rules](../any/) as `any` when using
> `Function`.***
