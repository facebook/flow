---
id: getting-started
title: Getting Started with Flow
layout: docs
permalink: /docs/getting-started.html
prev: installing-flow.html
next: flow-basics.html
---

Please follow the [installation instructions](installing-flow.html). This should create a directory called `flow`, and a `flow` binary on your path.

## First Steps

Under `flow` you'll find an `examples` directory. This contains the examples for this tutorial. To get a feel for Flow let's look at the first one:

```
cd flow/examples/01_HelloWorld
flow check
```

You should see an error a little like this:

```
01_HelloWorld/hello.js:7:5,17: string
This type is incompatible with
  01_HelloWorld/hello.js:4:10,13: number
```

Looking at the example itself it's easy to see why:

```javascript
/* @flow */

function foo(x) {
  return x*10;
}

foo("Hello, world!");
```

We're calling a function that clearly expects a number with a string. Flow detects that and returns an error. To fix this example, you can call `foo` with an integer instead. Running `flow check` should no longer find an error. A possible fix is in the `answer` directory. Throughout this tutorial, you will find solutions in the `answer` directory each time.

You may have noticed this header line in the example file:

```javascript
/* @flow */
```

This is important: it tells Flow that this file should be typechecked. Flow will ignore any files that don't have this header, so you can start converting your project one file at a time. 

## Adding typehints

Flow infers type within a file, so you don't have to annotate every function to get typechecking. However you can always add annotations, and in fact Flow requires them for functions that are exported (defined in one file and used in another). 

The second example (`02_TypeAnnotations`) shows basic type annotations in Flow:

```javascript
/* @flow */

function foo(x: string, y: number): string {
  return x.length * y;
}

foo("Hello", 42);
```

Again, running `flow check` gives an error. In this case it is the return type of `foo` that is wrong - we've declared it to be a `string` even though the function is returning a `number`. Flow flags that, and you can fix it by changing the return type.

## Nullable types

Flow handles `null` differently than most type systems. Most type systems ignore `null`, meaning that your program can be type correct but crash because they access `null`. In Flow, doing this is an error, as shown by our third example (`03_Null`):

```javascript
/* @flow */

function length(x) {
  return x.length;
}

var total = length("Hello") + length(null);
```

This program would crash at runtime, with a `TypeError` when it tries to read the property `length` on `null`. Running `flow check` will detect that.

The file in the `answer` directory fixes both the code and the type error:

```javascript
/* @flow */

function length(x) {
  if (x !== null) {
    return x.length;
  } else {
    return 0;
  }
}

var total = length("Hello") + length(null);
```

Because we've checked that `x` is not `null`, Flow knows this is safe and doesn't emit a type error. 
