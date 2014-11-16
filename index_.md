---
layout: default
title: A static type checker for JavaScript
id: home
hero: true
---

##What is Flow?

Flow is a static type checker for Javascript. It is designed to find type errors in existing Javascript programs, and to let you gradually evolve Javascript code into typed code. Flow code transforms down to regular Javascript, so it runs anywhere.

{% highlight javascript linenos %}
/* @flow */
function foo(x) {
  return x * 10;
}
foo('Hello, world!');
{% endhighlight %}

{% highlight text %}
$> flow
01_HelloWorld/hello.js:5:5,17: string
This type is incompatible with
  01_HelloWorld/hello.js:3:10,13: number
{% endhighlight %}

Flow is still in early stages. It is used within Facebook, and we hope it will be just as useful for many other Javascript projects. Try it out and give us feedback!


##Why Flow?

The goal of Flow is to find errors in Javascript code with little programmer effort. Flow relies heavily on <strong>type inference</strong> to find type errors even when the program has not been annotated - it precisely tracks the types of variables as they flow through the program. At the same time, Flow is a <strong>gradual</strong> type system. Any parts of your program that are dynamic in nature can easily bypass the type checker, so you can mix statically typed code with dynamic code.

Flow also supports a highly <strong>expressive</strong> type language. Flow types can express much more fine-grained distinctions than traditional type systems. For example, Flow helps you catch errors involving null, unlike most type systems.

For more information about Flow, check out this video from <a href="https://www.facebook.com/atscale2014">@Scale</a>:

<iframe frameborder="0" allowfullscreen width="100%" height="320" src="http://www.youtube.com/embed/M8x0bc81smU?start=768"></iframe>

##Using Flow

Follow our <a href="/docs/getting-started.html">Getting Started</a> guide to download and try Flow. Flow is open-source, so you can also check out our <a href="https://github.com/facebook/flow">GitHub repo</a>.
