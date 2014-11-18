---
id: type-annotations
title: Type Annotations
layout: docs
permalink: /docs/type-annotations.html
prev: underscore.html
next: base-types.html
---

JavaScript is inherently a dynamically-typed language. As such, explicitly
typing your code is not part of the JavaScript lexicon. This is normal
JavaScript code:

{% highlight javascript linenos=table %}
function add(num1, num2) {
  return num1 + num2;
}
var x = add(3, '0');
console.log(x);
{% endhighlight %}

What is the value of `x`? `3`? `30`? `undefined`? The answer is `30`, and, in most
cases, this probably not the behavior you would prefer.

Flow helps mitigate these sort of subtle bugs by trying to keep your code sane
through static analysis and type annotations.

## Type Annotations

Type annotations are generally prefixed by `:`. And they can be placed on
function parameters, function return types and variable declarations. e.g.,

{% highlight javascript linenos=table %}
function foo(a: mixed, b: number): void { ... }
var x: boolean;
class Bar {
  y: string;
}
{% endhighlight %}

## Simple Example

We can easily take this code and make it Flow aware by adding a simple
annotation `@flow` at the top in a comment block:

{% highlight javascript linenos=table %}
/* @flow */
function add(num1, num2) {
  return num1 + num2;
}
var x = add(3, '0');
console.log(x);
{% endhighlight %}

However, Flow will find no errors with the above code. That's because the `+`
operator is perfectly acceptable on `number`s and `string`s, and we didn't
specify that the parameters to `add` must be `number`s.

{% highlight javascript linenos=table %}
/* @flow */
function add(num1: number, num2: number): number {
  return num1 + num2;
}
var x: number = add(3, '0');
console.log(x);
{% endhighlight %}

Running the type checker against the above code will yield type errors
since we have explicitly typed all parameters and variables.

```bbcode
file.js:5:24,26: string
This type is incompatible with
  file.js:2:34,39: number
```

## Type Annotation Requirements

Type annotations are not always strictly necessary to use Flow. As shown above,
all that is strictly required to make your JavaScript file Flow aware is
the `@flow` annotation. And this annotation by itself can be enough for Flow to
deduce all that is necessary to type check your code.

{% highlight javascript linenos=table %}
/* @flow */
function multPI(num1, num2) {
  return Math.PI * num1 * num2;
}
var x = multPI(3, '0');
console.log(x);
{% endhighlight %}

Since the multiplication operator makes no real sense on a string, Flow is
smart enough to deduce a problem here without explicit type annotations.

```bbcode
file.js:5:19,21: string
This type is incompatible with
  file.js:3:10,30: number
```

### Module Boundaries

However, explicit type annotations are required at all module boundaries.
Flow's inference engine stops there.

{% highlight javascript linenos=table %}
/**
 * Size.js
 * @flow
 */
function size(input: string): number {
  return input.length;
}

module.exports = size;
{% endhighlight %}

{% highlight javascript linenos=table %}
/**
 * UseSize.js
 * @flow
 */
var size = require('./Size');
var result = size(null);
{% endhighlight %}

Type annotations are required in `Size.js` because `UseSize.js` is calling the
`size()` function from outside the module and that crosses the inference
boundary.

```bbcode
UseSize.js:6:19,22: null
This type is incompatible with
  Size.js:5:22,27: string
```

## `any`

`any` is a special type annotation that represents the universal dynamic type.
`any` can flow to any other type, and vice-versa. `any` is basically the "get
out of my way, I know what I am doing" annotation. Use it when Flow is getting
in your way, but you know your program is correct.

## Bottom Line

You can type annotate all your code. That would be the most expressive and
self-documenting approach. However, Flow does a lot of type inference inside
modules. It only requires type annotations across module boundaries.
