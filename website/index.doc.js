/* @flow */
/*
---
layout: home
title: A static type checker for JavaScript
id: home
bodyClass: home
custom_footer: true
---
*/

/*

<header class='main hero'><div class='width'>
  <div class='flow-header'></div>
  <div>
    <p>A static type checker for JavaScript</p>
  </div>
</div></header>

<section class="marketing-row three"><div class="width">
<div class="col first" markdown="1">

## Type Inference

Flow uses type inference to find bugs even without type annotations. It
precisely tracks the types of variables as they flow through your program.

</div>
<div class="col" markdown="1">

## Idiomatic JS

Flow is designed for JavaScript programmers. It understands common JavaScript idioms and very dynamic
code.

</div>
<div class="col" markdown="1">

## Realtime Feedback

Flow incrementally rechecks your changes as you work, preserving the fast feedback cycle of developing
plain JavaScript.

</div>
</div></section>

<section class='content'><div class='width'>
<article markdown="1">

Flow can catch common bugs in JavaScript programs before they run, including:

* silent type conversions,
* `null` dereferences,
* and the dreaded `undefined is not a function`.

*/

// $WithLineNums
// @flow
function foo(x) {
  return x * 10;
}
// $ExpectError
foo('Hello, world!');

/*
Flow also lets you **gradually** add type assertions to your code:
*/

// $WithLineNums
// @flow
function bar(x): string {
  // $ExpectError
  return x.length;
}
bar('Hello, world!');

/*

JavaScript code with Flow annotations [easily transforms](/docs/running.html)
down to regular JavaScript, so it runs anywhere.

</article>
</div></section>

<section class='content'><div class='width'>
<article markdown="1">

## Use Flow

Start out with our [Getting Started](/docs/getting-started.html) guide and try Flow
for yourself.

Flow also powers advanced features for editors, including [Vim][flow-vim],
[Emacs][flow-emacs], and [Nuclide][flow-nuclide].

[flow-vim]: https://github.com/flowtype/vim-flow
[flow-emacs]: https://github.com/facebook/flow/blob/master/flow-types.el
[flow-nuclide]: http://nuclide.io/docs/languages/flow/

<header class='hero'><div class="width">
  <div class="buttons-unit">
    <a href="/docs/getting-started.html#_" class="button">Get Started</a>
  </div>
  <div>
    <a href="/docs/quick-reference.html" class="sub-header-link">Quick Reference</a>
    <a href="https://github.com/facebook/flow/releases/latest" class="sub-header-link">Download</a>
  </div>
</div></header>

</article>
</div></section>

<footer><div class="width">
  <div class="oss_logo">
    <a href="https://code.facebook.com/projects/">
      <img src="{% asset_path 'oss_logo' %}" alt="Facebook Open Source" />
    </a>
  </div>
  <div class="contributing" markdown="1">

Flow is [on GitHub](https://github.com/facebook/flow). It is used heavily within Facebook and
developed in the open. We hope it will be useful for other JavaScript projects, so please try it out,
join the community, and give us feedback!

  </div>
  <div class="copyright">
    &copy; Copyright 2014 - 2016, Facebook Inc.
  </div>
</div></footer>
*/
