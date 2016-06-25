/* @flow */
/*
---
id: five-simple-examples
title: Five simple examples
permalink: /docs/five-simple-examples.html
prev: getting-started.html
next: new-project.html
---
*/

/*

Throughout each of the following examples, we will check our code using the
`flow` CLI. You can install this CLI using
[npm](http://blog.npmjs.org/post/85484771375/how-to-install-npm) by running
`npm install --global flow-bin`.

Note also that the first time you run `flow` on a given example (or project), it
will normally take a few seconds, but subsequent runs are *much* quicker. This
is because running `flow` will start a process in the background which monitors
the project you are working within and incrementally recalculate type errors
with each change you save to disk. **If you wish to stop the background Flow
process, you can do so with the `flow stop` command.**

If you wish to just typecheck a project without a persistent process (and
you don't mind waiting a few extra seconds each time) you can use `flow check`.
This will ensure that Flow shuts down immediately after checking your project.

## 1. Hello Flow!

Inside the Flow GitHub repo, you'll find an
[`examples` directory](https://github.com/facebook/flow/tree/master/examples).
This directory contains the examples for this tutorial. To get a feel for
Flow, let's look at the first one:

```bash
$> cd flow/examples/01_HelloWorld
$> flow
```

You should see an error that looks something like this:

```text
hello.js:7
  7: foo("Hello, world!");
     ^^^^^^^^^^^^^^^^^^^^ function call
  4:   return x * 10;
              ^ string. This type is incompatible with
  4:   return x * 10;
              ^^^^^^ number
```
{: .cli-error}

Looking at the `hello.js` example file, it's easy to see why:

*/
// $NoCliOutput
// $WithLineNums
// @flow

(function() { // $DocHide
function foo(x) {
  return x * 10;
}

// $ExpectError
foo('Hello, world!');
}); // $DocHide

/*
  We're calling a function with a string, when that function clearly expects a
  number. Flow detects this problem and gives an error. One fix for this example
  would be to call `foo` with a number instead:

*/
// $WithLineNums
// @flow

(function() { // $DocHide
function foo(x) {
  return x * 10;
}

// This is fine, because we're passing a number now
foo(10);
}); // $DocHide

/*
  Throughout this tutorial, you will find solutions for each example in the
  example's `answer` directory.

  You may have noticed this header line in the example file:

*/
// @flow
/*
  This is important: it tells Flow that this file should be typechecked. **Flow
  will ignore any files that don't have this header.** This allows you to
  convert and/or typecheck a JS project one file at a time.

## 2. Adding type annotations

  Flow can infer the type of most things within a file, so you don't always have
  to annotate every function and variable to get typechecking to work. However,
  even if Flow can infer a type, you can still add annotations to be explicit.
  The only time that Flow strictly *requires* an annotation is when a
  variable/function/class is exported from a module (defined in one file and
  used in another).

  The second example (`02_TypeAnnotations`) shows usage of some basic type
  annotations in Flow:

*/
// $NoCliOutput
// $WithLineNums
// @flow

(function() { // $DocHide
function foo(x: string, y: number): string {
  // $ExpectError
  return x.length * y;
}

foo('Hello', 42);
}); // $DocHide

/*
  Here we have annotated the `foo` function to say that it's two parameters are
  of type `string` and `number` and that it returns a `string`.

  With these annotations in place, if we run `flow check` we'll see an error:

  ```text
  type_annotations.js:4
    4:   return x.length * y;
                ^^^^^^^^^^^^ number. This type is incompatible with the expected return type of
    3: function foo(x: string, y: number): string {
                                           ^^^^^^ string
  ```
  {: .cli-error}

  In this case it is the return type for `foo` that is wrong. Even though we
  have annotated it as returning a `string`, the actual type that it returns (a
  `number`) does not match! Flow flags this issue and you can fix it by
  correcting the return type:

*/
// $WithLineNums
// @flow

(function() { // $DocHide
// Changing the return type to number fixes the error
function foo(x: string, y: number): number {
  return x.length * y;
}

foo('Hello', 42);
}); // $DocHide

/*

## 3. Nullable types

  Flow handles `null` differently than most type systems. When a type system
  does not track usage of `null` carefully, it becomes possible for your program
  to misleadingly typecheck as correct but still crash when `null` is accessed
  in an unsafe way. In Flow, accessing `null` in an unsafe way will incur an
  error as shown by our third example (`03_Null`):

*/
// $NoCliOutput
// $WithLineNums
// @flow

(function() { //$DocHide
function length(x) {
  return x.length;
}

// $ExpectError
var total = length('Hello') + length(null);
}) //$DocHide
/*

  This program would fail at runtime, with a `TypeError` when it tries to read
  the property `length` on `null`. Running `flow` will detect this bug:

  ```text
  nulls.js:7
    7: var total = length("Hello") + length(null);
                                     ^^^^^^^^^^^^ function call
    4:   return x.length;
                  ^^^^^^ property `length`. Property cannot be accessed on possibly null value
    4:   return x.length;
                ^ null
  ```
  {: .cli-error}

  The file in the `answer` directory fixes this bug just like you might if you
  had discovered it at runtime. This fix, in turn, makes the type error go away:

*/
// $WithLineNums
// @flow

(function() { // $DocHide
function length(x) {
  if (x !== null) {
    return x.length;
  } else {
    return 0;
  }
}

var total = length('Hello') + length(null);
}); // $DocHide
/*

  Because we've checked that `x` is not `null`, Flow knows that this code is
  now safe and doesn't emit a type error.

## 4. Arrays

  Flow is of course not limited to simple types like numbers and strings. Our
  next example, `04_Arrays`, illustrates support for annotating functions on
  arrays:

*/
// $NoCliOutput
// $WithLineNums
// @flow

function total(numbers: Array<number>) {
  var result = 0;
  for (var i = 0; i < numbers.length; i++) {
    result += numbers[i];
  }
  return result;
}

// $ExpectError
total([1, 2, 3, 'Hello']);
/*

  Flow will flag the string `'Hello'` here since the `total()` function accepts
  only an array of numbers:

  ```text
  arrays.js:11
   11: total([1, 2, 3, "Hello"]);
       ^^^^^^^^^^^^^^^^^^^^^^^^^ function call
   11: total([1, 2, 3, "Hello"]);
                       ^^^^^^^ string. This type is incompatible with
    3: function total(numbers: Array<number>) {
                                     ^^^^^^ number
  ```
  {: .cli-error}

  If we replace `"Hello"` with a number, the code will pass Flow's checks:

*/
// $WithLineNums
// @flow

function total(numbers: Array<number>) {
  var result = 0;
  for (var i = 0; i < numbers.length; i++) {
    result += numbers[i];
  }
  return result;
}

total([1, 2, 3, 4]);

/*


## 5. Dynamic code

  In our final example, `05_DynamicCode`, we haven't annotated the function, but we are passing in two different types of arguments:

*/
// $NoCliOutput
// $WithLineNums
// @flow

(function() { // $DocHide
function foo(x) {
  // $ExpectError
  return x.length;
}

var res = foo('Hello') + foo(42);
}); // $DocHide

/*

  In this case, Flow detects that the second time the function is called (with a number), the `length` property will fail:

  ```text
  dynamic.js:4
    4:   return x.length;
                  ^^^^^^ property `length`. Property not found in
    4:   return x.length;
                ^ Number
  ```
  {: .cli-error}

  One fix is to simply detect what the type is within the function:

*/
// $WithLineNums
// @flow

(function() { // $DocHide
function foo(x) {
  if (typeof x === 'string') {
    return x.length;
  } else {
    return x;
  }
}

var res = foo('Hello') + foo(42);
}); // $DocHide
/*

  Flow is smart enough to detect that this conditional check is sufficient to
  avoid any potential failures at run time, and will give you a clean bill of
  health.

## Next Steps

  These simple examples just scratch the surface. You're now ready to start a
  new project with Flow and use the [offline transform tool](/docs/running.html#using-the-offline-transform-tool)
  to compile type annotations before publishing. Or you could incrementally
  [try Flow using flow on some existing code](/docs/existing.html). You may also
  want to check out our much bigger [React example](/docs/react.html) to
  see Flow in more representative use cases.
*/
