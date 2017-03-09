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

There are three forms of functions that each have their own slightly syntax.

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
array of arguments at the end of a list of parameters. These have an elipsis
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

### `Function` Type <a class="toc" id="toc-function-type" href="#toc-function-type"></a>

Sometimes it is useful to write types that accept arbitrary functions, for
those you should write `() => mixed` like this:

```js
function method(func: () => mixed) {
  // ...
}
```

However, if you need to opt-out of the type checker, and don't want to go all
the way to `any`, you can instead use `Function`. **`Function` is unsafe and
should be avoided.**

For example, the following code will not report any errors:

```js
function method(func: Function) {
  func(1, 2);     // Works.
  func("1", "2"); // Works.
  func({}, []);   // Works.
}

method(function(a: number, b: number) {
  // ...
});
```

> ***You should follow [all the same rules](../any/) as `any` when using
> `Function`.***.
