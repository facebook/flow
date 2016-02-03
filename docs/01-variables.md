---
id: variables
title: Variables
layout: docs
permalink: /docs/variables.html
prev: type-annotations.html
next: arrays.html
---

A variable is one of the simplest forms of code location. Usually you won't need
to annotate variables with types, since they are readily inferred by Flow. But
you can, if you so choose (e.g. for documentation purposes).

## Type Annotating Variables

{% highlight javascript linenos=table %}
/* @flow */
const x: number = 0;
const y: any = 4;
{% endhighlight %}

The actual type of a variable can change over its lifetime; at any point, the
type is based on the most recent assignment to the variable. (In technical
terms, the type of a variable is flow-sensitive.) As such, a type annotation
on a variable serves as a super-type of all the actual types it may have over
its lifetime. Put differently, the type annotation is a conservative
description of all the values that may be assigned to the variable over its
lifetime.

As a concrete example, a variable annotated with type `any` can be assigned a
`number`, and furthermore, will have the more precise number type following
that assignment. On the other hand, a variable annotated with type `string`
can never be assigned a `number` (it would be a type error).
