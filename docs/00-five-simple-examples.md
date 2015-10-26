---
id: five-simple-examples
title: Five simple examples
layout: docs
permalink: /docs/five-simple-examples.html
prev: getting-started.html
next: new-project.html
---

## 1. Hello Flow!

Inside the installed folder you'll find an `examples` directory. This contains the examples for this tutorial. To get a feel for Flow, let's look at the first one:

```bash
$> cd flow/examples/01_HelloWorld
$> flow check
```

You should see an error a little like this:

```bbcode
hello.js:7
  7: foo("Hello, world!");
     ^^^^^^^^^^^^^^^^^^^^ function call
  4:   return x*10;
              ^ string. This type is incompatible with
  4:   return x*10;
              ^^^^ number
```

Looking at the `hello.js` example file, it's easy to see why:

{% highlight javascript linenos=table %}
/* @flow */

function foo(x) {
  return x * 10;
}

foo('Hello, world!');
{% endhighlight %}

We're calling a function that clearly expects a number with a string. Flow detects that and returns an error. One fix for this example would be to call `foo` with an integer instead: 

{% highlight javascript linenos=table %}
/* @flow */

function foo(x) {
  return x * 10;
}

// This is fine, because we're passing a number now
foo(10);
{% endhighlight %}

Throughout this tutorial, you will find solutions for each example in the example's `answer` directory.

You may have noticed this header line in the example file:

{% highlight javascript linenos=table %}
/* @flow */
{% endhighlight %}

This is important: it tells Flow that this file should be typechecked. Flow will ignore any files that don't have this header. This allows you to convert and/or type-check a JS project one file at a time.

## 2. Adding type annotations

Flow can infer the type of most things within a file, so you don't always have to annotate every function and variable to get typechecking. However, even if Flow can infer a type, you can still add annotations to be explicit. In fact, Flow requires that you add type annotations for functions that are exported from a module (defined in one file and used in another).

The second example (`02_TypeAnnotations`) shows basic type annotations in Flow:

{% highlight javascript linenos=table %}
/* @flow */

function foo(x: string, y: number): string {
  return x.length * y;
}

foo('Hello', 42);
{% endhighlight %}

Again, running `flow check` gives an error:

```bbcode
type_annotations.js:4
  4:   return x.length * y;
              ^^^^^^^^^^^^ number. This type is incompatible with
  3: function foo(x: string, y: number): string {
                                         ^^^^^^ string
```

In this case it is the return type of `foo` that is wrong - we've declared it to be a `string` even though the function is actually returning a `number`. Flow flags that, and you can fix it by changing the return type:

{% highlight javascript linenos=table %}
/* @flow */

// Changing the return type to number fixes the error
function foo(x: string, y: number): number {
  return x.length * y;
}

foo('Hello', 42);
{% endhighlight %}

## 3. Nullable types

Flow handles `null` differently than most type systems - which ignore `null`. When a type system ignores `null`, that means your program can type check as correct but still crash when `null` is accessed in an unsafe way. In Flow, doing this is an error as shown by our third example (`03_Null`):

{% highlight javascript linenos=table %}
/* @flow */

function length(x) {
  return x.length;
}

var total = length('Hello') + length(null);
{% endhighlight %}

This program would fail at runtime, with a `TypeError` when it tries to read the property `length` on `null`. Running `flow check` will detect that:

```bbcode
nulls.js:7
  7: var total = length("Hello") + length(null);
                                   ^^^^^^^^^^^^ function call
  4:   return x.length;
                ^^^^^^ property `length`. Property cannot be accessed on possibly null value
  4:   return x.length;
              ^ null
```

The file in the `answer` directory fixes both the code and the type error:

{% highlight javascript linenos=table %}
/* @flow */

function length(x) {
  if (x !== null) {
    return x.length;
  } else {
    return 0;
  }
}

var total = length('Hello') + length(null);
{% endhighlight %}

Because we've checked that `x` is not `null`, Flow knows this is safe and doesn't emit a type error.

## 4. Arrays

Flow is of course not limited to simple types like numbers and strings. For example, `04_Arrays` illustrates the support for annotating functions on arrays:

{% highlight javascript linenos=table %}
/* @flow */

function total(numbers: Array<number>) {
  var result = 0;
  for (var i = 0; i < numbers.length; i++) {
    result += numbers[i];
  }
  return result;
}

total([1, 2, 3, 'Hello']);
{% endhighlight %}

Flow will flag the call to `total` as an error, since that function needs an array of numbers, and one of the items of the array passed in is a string:

```bbcode
arrays.js:11
 11: total([1, 2, 3, "Hello"]);
     ^^^^^^^^^^^^^^^^^^^^^^^^^ function call
 11: total([1, 2, 3, "Hello"]);
                     ^^^^^^^ string. This type is incompatible with
  3: function total(numbers: Array<number>) {
                                   ^^^^^^ number
```

This, however, will pass:

{% highlight javascript linenos=table %}
/* @flow */

function total(numbers: Array<number>) {
  var result = 0;
  for (var i = 0; i < numbers.length; i++) {
    result += numbers[i];
  }
  return result;
}

total([1, 2, 3, 4]);
{% endhighlight %}


## 5. Dynamic code

In our final example, `05_DynamicCode`, we haven't annotated the function, but we are passing in two different types of argument:

{% highlight javascript linenos=table %}
/* @flow */

function foo(x) {
  return x.length;
}

var res = foo('Hello') + foo(42);
{% endhighlight %}

In this case, Flow detects that the second time the function is called (with a number), the `length` property will fail:

```bbcode
dynamic.js:4
  4:   return x.length;
              ^^^^^^^^ property `length`
  4:   return x.length;
                ^^^^^^ property `length`. Property not found in
  4:   return x.length;
              ^ Number
```

One fix is to simply detect what the type is within the function:

{% highlight javascript linenos=table %}
/* @flow */

function foo(x) {
  if (typeof x === 'string') {
    return x.length;
  } else {
    return x;
  }
}

var res = foo('Hello') + foo(42);
{% endhighlight %}

Flow is smart enough to detect that this conditional check is sufficient to avoid any potential failures at run time, and will give you a clean bill of health.

## Next Steps

These simple examples just scratch the surface. You're now ready to [start a new project with Flow](new-project.html) or incrementally [try Flow on existing code](existing.html). You may also want to check out our much bigger [React example](react-example.html) to see Flow in more representative use cases.
