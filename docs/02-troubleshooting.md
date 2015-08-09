---
id: troubleshooting
title: Troubleshooting
layout: docs
permalink: /docs/troubleshooting.html
prev: running.html
next: advanced-configuration.html
---

## Common errors and how to fix them

### Global not found

These errors are due to global references in your code, and possibly also due to typos. If the former, you can declare them if you know that they are going to be available when you run the code.

{% highlight javascript linenos=table %}
declare var foo: <type>
{% endhighlight %}

Alternatively, if you want to have a common set of global declarations so that they are available to multiple files at once, create a directory (say `globals_lib`), put a file in there (say `globals.js`), and do the declaration there. Then rerun Flow with option `--lib globals_lib` so that Flow knows where to find them.

### Required module not found

These errors are due to `require(...)` statements in your code that don't resolve to the set of modules exported by files under `<root>`. To specify additional code directories to Flow, add the following lines to `.flowconfig` under `<root>`.

```
[include]
../node_modules/
../lib/
```

Alternatively, you may not have the code available for those modules, or otherwise want to specify declarations for them. In that case, as in the 'global not found' case above you need to add an interface file, and run Flow with the `--lib` option to point Flow to them. For example, use `--lib globals_lib`, put another file under the `globals_lib` directory (say `modules.js`), and declare the module interface there.

{% highlight javascript linenos=table %}
declare module Bar {
  ...
}
{% endhighlight %}

For more information about writing interface files, see [this guide](third-party.html) Note that if both an implementation and a declaration is found for a module, Flow will choose the implementation if it has been opted-in, the declaration otherwise.

### Operation not allowed on `null` / `undefined`

Flow considers types to be incompatible with `null` / `undefined` in general (the only compatible types are maybe types, denoted `?<type>`. Thus, it will complain if it finds than an operation may happen on `null` / `undefined`.

The general way to get around this problem is to store the value in a local variable, and guard the operation with a dynamic check on the local variable.

{% highlight javascript linenos=table %}
// var result = foo().bar
var x = foo();
var result = x != null ? x.bar : ...
{% endhighlight %}

You can try other variations of this basic pattern.

{% highlight javascript linenos=table %}
// foo.bar()
foo && foo.bar()
{% endhighlight %}

### Function call with too few arguments

In JavaScript, function calls can pass too many or too few arguments: additional arguments are dropped, and missing arguments are initialized with `undefined`. Flow admits the former pattern because it is mostly harmless; but it complains about the latter.

The usual way to fix these errors is to add optional parameter markers to the function being called.

{% highlight javascript linenos=table %}
function foo(x?) { ... }
foo();
{% endhighlight %}

Doing this might shift the problem to the function definition, where `x` now has a maybe type. So operations on `x` may require to be guarded by dynamic checks.

{% highlight javascript linenos=table %}
function foo(x?) {
  if (x != undefined) {
    // operation on x
  }
}
foo();
{% endhighlight %}

Alternatively, we may provide a default value to `x`, in which case the dynamic check is not required.

{% highlight javascript linenos=table %}
function foo(x = 0) {
  // operation on x
}
foo();
{% endhighlight %}

### Other type confusions

Some operations only make sense when they're performed on particular set of values. (They may still work on other values, but may have unintended consequences.) For example, multiplication should be performed only on numbers even though they may still work when you pass strings to them (they're usually converted to `NaN`). Iteration using `for-in` should be performed only on objects even though they may still work on arrays (the keys are converted to strings, and other properties are also included). Non-strict equality `==` should be performed only on values that have the same type (otherwise, some sequence of type conversions are tried). Flow complains about many of these operations. Usually, there is a workaround: either there is a better way to do these operations (e.g., use `Array.forEach` and `===` in the latter two cases, respectively), or a simple workaround (e.g., use `Number(...)` in the former case).
