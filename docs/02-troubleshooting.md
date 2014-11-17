---
id: troubleshooting
title: Troubleshooting
layout: docs
permalink: /docs/troubleshooting.html
prev: running.html
next: react-example.html
---

## Common errors and how to fix them

### Global not found

These errors are due to global references in your code, and possibly also due to typos. If the former, you can declare them if you know that they are going to be available when you run the code.

```javascript
declare var foo: <type>
```

Alternatively, if you want to have a common set of global declarations so that they are available to multiple files at once, create a directory (say `globals_lib`), put a file in there (say `globals.js`), and do the declaration there. Then rerun Flow with option `--lib globals_lib` so that Flow knows where to find them.

### Required module not found

These errors are due to `require(...)` statements in your code that don't resolve to the set of modules exported by files under `<root>`. To specify additional code directories to Flow, add the following lines to `.flowconfig` under `<root>`.

```javascript
[include]
<path1>
<path2>
```

Alternatively, you may not have the code available for those modules, or otherwise want to specify declarations for them. In that case, put another file under `globals_lib` (say `modules.js`), and declare the module interface there.

```javascript
declare module Bar {
  ...
}
```

If you don't know what the module interface should be, you can try to find it at [DefinitelyTyped](https://github.com/borisyankov/DefinitelyTyped), which hosts a bunch of TypeScript interface declarations for popular modules, and try to run it through our convertion tool **[TODO]**.

Note that if both an implementation and a declaration is found for a module, Flow will choose the implementation if it has been opted-in, the declaration otherwise.

### Operation not allowed on `null` / `undefined`

Flow considers types to be incompatible with `null` / `undefined` in general (the only compatible types are maybe types, denoted `?<type>`. Thus, it will complain if it finds than an operation may happen on `null` / `undefined`.

The general way to get around this problem is to store the value in a local variable, and guard the operation with a dynamic check on the local variable.

```javascript
// var result = foo().bar
var x = foo();
var result = x != null ? x.bar : ...
```

You can try other variations of this basic pattern.

```javascript
// foo.bar()
foo && foo.bar()
```

### Function call with too few arguments

In JavaScript, function calls can pass too many or too few arguments: additional arguments are dropped, and missing arguments are initialized with `undefined`. Flow admits the former pattern because it is mostly harmless; but it complains about the latter.

The usual way to fix these errors is to add optional parameter markers to the function being called.

```javascript
function foo(x?) { ... }
foo();
```

Doing this might shift the problem to the function definition, where `x` now has a maybe type. So operations on `x` may require to be guarded by dynamic checks.

```javascript
function foo(x?) {
  if (x != undefined) {
    // operation on x
  }
}
foo();
```

Alternatively, we may provide a default value to `x`, in which case the dynamic check is not required.

```javascript
function foo(x = 0) {
  // operation on x
}
foo();
```

### Other type confusions

Some operations only make sense when they're performed on particular set of values. (They may still work on other values, but may have unintended consequences.) For example, multiplication should be performed only on numbers even though they may still work when you pass strings to them (they're usually converted to `NaN`). Iteration using `for-in` should be performed only on objects even though they may still work on arrays (the keys are converted to strings, and other properties are also included). Non-strict equality `==` should be performed only on values that have the same type (otherwise, some sequence of type conversions are tried). Flow complains about many of these operations. Usually, there is a workaround: either there is a better way to do these operations (e.g., use `Array.forEach` and `==` in the latter two cases, respectively), or a simple workaround (e.g., use `Number(...)` in the former case).


## What to do if there are too many errors
