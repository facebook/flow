---
layout: default
title: A static type checker for JavaScript
id: home
hero: true
---

##What is Flow?

Flow is a static type checker for Javascript. It is designed to find type errors in existing Javascript programs:

{% highlight javascript linenos %}
/* @flow */
function foo(x) {
  return x * 10;
}
foo('Hello, world!');
{% endhighlight %}

```bash
$> flow
```

```bbcode
hello.js:5:5,19: string
This type is incompatible with
  hello.js:3:10,15: number
```

Flow also lets you gradually evolve Javascript code into typed code:

{% highlight javascript linenos %}
/* @flow */
function foo(x: string, y: number): string {
  return x.length * y;
}
foo('Hello', 42);
{% endhighlight %}

```bash
$> flow
```

```bbcode
hello.js:3:10,21: number
This type is incompatible with
  hello.js:2:37,42: string
```

Typed Flow code easily transforms down to regular Javascript, so it runs anywhere.

##Why Flow?

The goal of Flow is to find errors in Javascript code with little programmer effort. Flow relies heavily on <strong>type inference</strong> to find type errors even when the program has not been annotated - it precisely tracks the types of variables as they flow through the program.

At the same time, Flow is a <strong>gradual</strong> type system. Any parts of your program that are dynamic in nature can easily bypass the type checker, so you can mix statically typed code with dynamic code.

Flow also supports a highly <strong>expressive</strong> type language. Flow types can express much more fine-grained distinctions than traditional type systems. For example, Flow helps you catch errors involving `null`, unlike most type systems.

We first introduced Flow at the <a href="https://www.facebook.com/atscale2014">@Scale Conference</a> in September:

<iframe frameborder="0" allowfullscreen width="100%" height="320" src="http://www.youtube.com/embed/M8x0bc81smU?start=768&showinfo=0&modestbranding =1&rel=0&theme=light"></iframe>

##Using Flow

Start with our <a href="/docs/getting-started.html">Getting Started</a> guide to download and try Flow yourself. Flow is open-source, so you can also start with the code on the <a href="https://github.com/facebook/flow">GitHub repo</a>.

Flow is still evolving: it is already used within Facebook, and we want to continue develop it in the open. We hope it will be useful for other Javascript projects, so please try it out, join the community and give us feedback!
