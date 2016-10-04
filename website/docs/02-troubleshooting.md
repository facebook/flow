---
id: troubleshooting
title: Troubleshooting
permalink: /docs/troubleshooting.html
prev: running.html
next: cli.html
---

## Common errors and how to fix them

### Global not found

These errors are due to global references in your code, and possibly also due to typos. If the former, you can [declare them in an interface file](/docs/third-party.html#interface-files) if you know that they are going to be available when you run the code.

```js +line_numbers
declare var foo: <type>;
```

### Required module not found

These errors are due to `require(...)` or `import` statements in your code that don't resolve to the set of modules exported by files under `<root>`. To specify additional code directories to Flow, you can add the following lines to your project's `.flowconfig` under `<root>`.

```
[include]
../node_modules/
../lib/
```

Alternatively, you may not have the code available for those modules, or otherwise want to specify declarations for them. In that case, as in the 'global not found' case above you need to add an interface file and point to it from within your `.flowconfig`.

```js +line_numbers
declare module Bar {
  ...
}
```

For more information about writing interface files, see [this guide](third-party.html) Note that if both an implementation and a declaration is found for a module, Flow will choose the implementation only if it has been opted-in. Otherwise it will use the declaration.

### Operation not allowed on `null` / `undefined`

Flow considers types to be incompatible with `null` / `undefined` in general (the only compatible types are "nullable" types, denoted `?<type>`. Thus, it will complain if it finds that an operation may happen on `null` / `undefined` that couldn't normally happen on the type as well as null.

The general way to deal with this is to store the value in a local variable, and guard the operation with a dynamic check on the local variable.

```js +line_numbers
// var result = foo().bar
var x = foo();
var result = x != null ? x.bar : ...
```

You can also try other variations of this basic pattern:

```js +line_numbers
// foo.bar()
foo && foo.bar()
```

### Function call with too few arguments

In JavaScript, function calls can pass too many or too few arguments: Additional arguments get dropped, and missing arguments get initialized with `undefined`. Flow admits the former pattern because it is mostly harmless; but it complains about the latter.

The most common way to fix these errors is to mark optional parameters with a trailing-`?` in the definition for the function being called:

```js +line_numbers
function foo(x?) { ... }
foo();
```

Doing this might shift the problem to the function definition, where `x` now has a "optional" type. So operations on `x` may then need to be guarded by dynamic checks.

```js +line_numbers
function foo(x?) {
  if (x != undefined) {
    // operation on x
  }
}
foo();
```

Alternatively, you might want to just provide a default value to `x`, in which case the dynamic check is not required.

```js +line_numbers
function foo(x = 0) {
  // operation on x
}
foo();
```

### Other type confusions

Some operations only make sense when they're performed on a particular set of values. (They may still work on other values, but may have unintended consequences).

For example, multiplication (`*`) should be performed only on numbers even though it may happen to work when you pass strings (though usually they're converted to `NaN`).

Iteration using `for-in` should be performed only on objects even though it may still work on arrays (the keys are converted to strings and other, often unexpected, properties are also included).

Non-strict equality `==` should be performed only on values that have the same type (otherwise, some sequence of type conversions are tried).

Flow will error on many of these operations because they tend to be hazardous. Usually there is a safer way to express the intent of the code in these cases: e.g., use `Array.forEach` and `===` in the latter two cases or use `Number(...)` in the former case).
