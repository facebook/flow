---
id: dynamic-type-tests
title: Dynamic Type Tests
layout: docs
permalink: /docs/dynamic-type-tests.html
prev: operators.html
next: primitives.html
---

In addition to `null` checks, there are several other dynamic type tests
(*predicates*) on local variables that Flow recognizes and uses to refine
types. Refining a type with a predicate means narrowing the original type with
the type satisfied by values satisfying the predicate.

Type tests can occur in `if` and `switch` statements, the test block in loop
constructs like `for`, `for-in`, `for-of`, and `do-while`, conditional
expressions (ternary statements), and inline logical expressions like `a &&
a.b`.

Flow understands many idiomatic constructs used to determine the type of a value
at runtime, and incorporates that knowledge into its static analysis.

### Maybe, Null, and Undefined

{% highlight javascript linenos=table %}
/* @flow */

function foo(x: ?string): string {
  if (x == null) {
    return x;
  } else {
    return "null or undefined";
  }
}

function bar(x: ?string): string {
  if (x === null) {
    return "null";
  } else if (x === undefined) {
    return "undefined";
  } else {
    return x;
  }
}
{% endhighlight %}

Read more about <a href="docs/nullable-types.html">Maybe Types</a>.

### Truthiness and Existance

{% highlight javascript linenos=table %}
/* @flow */

function foo(x: string): string {
  if (x) {
    return x;
  } else {
    return "the empty string";
  }
}

function bar(x: number): string {
  if (x) {
    return x.toString();
  } else {
    return "zero";
  }
}

function baz(x: boolean): string {
  if (x) {
    return "true";
  } else {
    return "false";
  }
}

function qux(x: { y?: number }): number {
    return x.y || 0;
}
{% endhighlight %}

### `typeof`

This type test is particularly useful in conjunction with <a
href="/docs/union-intersection-types.html">union types</a>.

{% highlight javascript linenos=table %}
/* @flow */

function foo(x: number | string): number {
  if (typeof x === "string") {
    return x.length;
  } else {
    return x;
  }
}
{% endhighlight %}

### Booleans

{% highlight javascript linenos=table %}
/* @flow */

declare function onlyTrue(t: true): void;
declare function onlyFalse(f: false): void;

function test(x: boolean) {
  if (x === true) {
    onlyTrue(x);
  } else {
    onlyFalse(x);
  }
}
{% endhighlight %}

### Array.isArray

{% highlight javascript linenos=table %}
/* @flow */

type NestedArray<T> = Array<T|NestedArray<T>>

function flatten<T>(xs: NestedArray<T>): Array<T> {
  var result = [];
  for (var i = 0; i < xs.length; i++) {
    if (Array.isArray(xs[i])) {
      result = result.concat(flatten(xs[i]));
    } else {
      result.push(xs[i]);
    }
  }
  return result;
}
{% endhighlight %}

### A instanceof B

{% highlight javascript linenos=table %}
/* @flow */

declare function businessLogic(x: string): void;

function myEventHandler(e: Event) {
  if (e.target instanceof HTMLInputElement) {
    businessLogic(e.target.value);
  }
}
{% endhighlight %}

### Tagged Unions

{% highlight javascript linenos=table %}
/* @flow */

type BinaryTree =
  { kind: "leaf", value: number } |
  { kind: "branch", left: BinaryTree, right: BinaryTree }

function sumLeaves(tree: BinaryTree): number {
  if (tree.kind === "leaf") {
    return tree.value;
  } else {
    return sumLeaves(tree.left) + sumLeaves(tree.right);
  }
}
{% endhighlight %}

### Logical Combinations and Negation

{% highlight javascript linenos=table %}
/* @flow */

function foo(x: string | number | boolean): string {
  if (typeof x === "number" || typeof x === "boolean") {
    return "number or boolean";
  } else {
    return x;
  }
}

function bar(x: string | number | boolean): string {
  if (!(typeof x === "number") && !(typeof x === "boolean")) {
    return x;
  } else {
    return "number of boolean";
  }
}

function baz(x: { y?: { z?: string }}): string {
  return x && x.y && x.y.z || "not found";
}
{% endhighlight %}

## Caveats

Flow is pessimistic about refinements. If it is possible that a refinement may
become invalid, Flow will throw away the refinement. This can often happen when
invoking a function that might refer to the refined value.

{% highlight javascript linenos=table %}
/* @flow */

declare function something(): void;

function foo(x: { y: ?string }): string {
  if (x.y) {
    something();
    return x.y; // error: x.y may be null/undefined
  } else {
    return "default";
  }
}
{% endhighlight %}

In the above code, `something` might mutate `x`, invalidating the refinement. It
is unsafe to expect that `x.y` will always be a string after calling this
function. It is simple to work around this, however. You can copy the object's
property value to a local variable, which can't be mutated from the outside.

{% highlight javascript linenos=table %}
/* @flow */

declare function something(): void;

function foo(x: { y: ?string }): string {
  if (x.y) {
    var y = x.y;
    something();
    return y; // OK: something couldn't have changed y
  } else {
    return "default";
  }
}
{% endhighlight %}

Flow does not currently perform mutation analysis, so in some cases, Flow will
throw away a refinement that is always safe to keep. As more analyses are
implemented, Flow will hopefully come to understand the following examples are
sound.

{% highlight javascript linenos=table %}
/* @flow */

function foo(x: { y: ?string }): string {
  if (x.y) {
    var y = x.y;
    console.log("*obviously* this doesn't mutate x");
    return y; // error: Flow doesn't know that
  } else {
    return "default";
  }
}

function bar(x: ?string): string {
  function baz() { /* this doesn't mutate x */ }
  if (x) {
    bar();
    return x; // error: Flow doesn't know that
  } else {
    return "default";
  }
}
{% endhighlight %}
