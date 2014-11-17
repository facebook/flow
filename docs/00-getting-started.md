---
id: getting-started
title: Getting started with Flow
layout: docs
permalink: /docs/getting-started.html
prev: about-flow.html
next: new-project.html
---

## Installing Flow

We provide pre-built binaries for Flow, depending on your operating system:

  * Mac OS X: [http://flow-lang.org/downloads/flow-osx-latest.zip](/downloads/flow-osx-latest.zip)
  * Linux (64 bit): [http://flow-lang.org/downloads/flow-linux64-latest.zip](/downloads/flow-linux64-latest.zip)

Flow is packaged as a zip file. To install, simply unzip it:

```bash
$> unzip flow.zip
```

This creates a directory called `flow` containing the executable binary (also called `flow`) and a folder of examples. It's recommended you add this directory to your path so that you can simply run `flow` from anywhere on your system.

## 1. Hello Flow!

Inside the installed folder you'll find an `examples` directory. This contains the examples for this tutorial. To get a feel for Flow, let's look at the first one:

```bash
$> cd flow/examples/01_HelloWorld
$> flow check
```

You should see an error a little like this:

```bbcode
01_HelloWorld/hello.js:7:5,17: string
This type is incompatible with
  01_HelloWorld/hello.js:4:10,13: number
```

Looking at the example itself it's easy to see why:

{% highlight javascript linenos %}
/* @flow */

function foo(x) {
  return x * 10;
}

foo('Hello, world!');
{% endhighlight %}

We're calling a function that clearly expects a number with a string. Flow detects that and returns an error. To fix this example, you can call `foo` with an integer instead. Running `flow check` should no longer find an error. A possible fix is in the `answer` directory:

{% highlight javascript linenos %}
/* @flow */

function foo(x) {
  return x * 10;
}

// This is fine, because we're passing a number now
foo(10);
{% endhighlight %}

Throughout this tutorial, you will find solutions in the `answer` directory each time.

You may have noticed this header line in the example file:

{% highlight javascript linenos %}
/* @flow */
{% endhighlight %}

This is important: it tells Flow that this file should be typechecked. Flow will ignore any files that don't have this header, so you can start converting your project one file at a time.

## 2. Adding type annotations

Flow infers type within a file, so you don't have to annotate every function to get typechecking. However you can always add annotations, and in fact Flow requires them for functions that are exported (defined in one file and used in another).

The second example (`02_TypeAnnotations`) shows basic type annotations in Flow:

{% highlight javascript linenos %}
/* @flow */

function foo(x: string, y: number): string {
  return x.length * y;
}

foo('Hello', 42);
{% endhighlight %}

Again, running `flow check` gives an error:

```bbcode
02_TypeAnnotations/type_annotations.js:4:10,21: number
This type is incompatible with
  02_TypeAnnotations/type_annotations.js:3:37,42: string
```

In this case it is the return type of `foo` that is wrong - we've declared it to be a `string` even though the function is returning a `number`. Flow flags that, and you can fix it by changing the return type:

{% highlight javascript linenos %}
/* @flow */

// Changing the return type to number fixes the error
function foo(x: string, y: number): number {
  return x.length * y;
}

foo('Hello', 42);
{% endhighlight %}

## 3. Nullable types

Flow handles `null` differently than most type systems. Most type systems ignore `null`, meaning that your program can be type correct but crash because they access `null`. In Flow, doing this is an error, as shown by our third example (`03_Null`):

{% highlight javascript linenos %}
/* @flow */

function length(x) {
  return x.length;
}

var total = length('Hello') + length(null);
{% endhighlight %}

This program would fail at runtime, with a `TypeError` when it tries to read the property `length` on `null`. Running `flow check` will detect that:

```bbcode
03_Null/nulls.js:4:10,17: property length
Property cannot be accessed on possibly null value
  03_Null/nulls.js:7:38,41: null
```

The file in the `answer` directory fixes both the code and the type error:

{% highlight javascript linenos %}
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

{% highlight javascript linenos %}
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
04_Arrays/arrays.js:11:17,23: string
This type is incompatible with
  04_Arrays/arrays.js:3:31,36: number
```

This, however, will pass:

{% highlight javascript linenos %}
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

In our final example, we haven't annotated the function, but we're passing in two different types of argument:

{% highlight javascript linenos %}
/* @flow */

function foo(x) {
  return x.length;
}

var res = foo('Hello') + foo(42);
{% endhighlight %}

In this case, Flow detects that the second time the function is called (with a number), the `length` property will fail:

```bbcode
05_DynamicCode/dynamic.js:4:10,17: property length
Property not found in
  /lib/core.js:50:1,62:1: Number
```

One fix is to simply detect what the type is within the function. Flow is able to detect that this will avoid any potential failures at run time, and will give you a clean bill of health:

{% highlight javascript linenos %}
/* @flow */

function foo(x) {
  if (typeof(x) === 'string') {
    return x.length;
  } else {
    return x;
  }
}

var res = foo('Hello') + foo(42);
{% endhighlight %}

## Next Steps

These simple examples just scratch the surface. You're now ready to [start a new project with Flow](new-project.html). You may also want to check out our much bigger [React example](react-example.html) to see Flow in action.
