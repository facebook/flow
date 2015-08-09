---
id: third-party
title: Checking third-party code
layout: docs
permalink: /docs/third-party.html
prev: existing.html
next: running.html
---

Most real JavaScript programs depend on third-party libraries. This guide shows how to use Flow in a project with external dependencies, without having to typecheck library code.

## Interface Files

Flow supports *interface files* for the purpose of understanding third party code you did not write. These files define the interface to a library, including types, separately from the actual code of the library. You never need to change library code to use interface files, but your code will be typechecked against the types declared in the interface file.

The workflow for dealing with library code is:

* Do not change the library files or add `@flow` to them
* Add one or more interface files for your libraries in a special directory in your project - for example `interfaces`
* Point Flow at those interface files by starting it with `flow start --lib  <path to your interface files>` or by specifying a `[lib]` section in your `.flowconfig` file as such:

```
[libs]
interfaces/
```

## Example

To illustrate this workflow, we'll pick the [Underscore](http://underscorejs.org/) library. Let's say we have this simple file using Underscore:

{% highlight javascript linenos=table %}
/* @flow */

var pizzas = [
  { title: 'Margherita', vegetarian: true },
  { title: 'Pepperoni', vegetarian: false },
  { title: 'Four cheese', vegetarian: true },
  { title: 'Hawaiian', vegetarian: false },
];

function vegetarianPizzas() {
  return _.findWhere(pizzas, {vegetarian: true});
}
{% endhighlight %}

Running `flow` will unsurprisingly produce an error:

```bbcode
underscore_example.js:11:10,10: unknown global name: _
```

This is because Flow doesn't know anything about the `_` variable/module. To fix this we need to bring in an interface file for Underscore.:

{% highlight javascript linenos=table %}
declare class Underscore {
  findWhere<T>(list: Array<T>, properties: {}): T;
}

declare var _: Underscore;
{% endhighlight %}

This only describes (part of) the interface for Underscore, eliding all implementation details - so Flow never has to understand the Underscore code itself.

If we now add the `interfaces/` directory to our flow config under a `[libs]` section:

```
[libs]
interfaces/
```

We can run flow again and see that the error goes away:

```bash
$> flow
```

```
Found 0 errors
```

If you temporarily modify your code that uses Underscore to purposefully introduce a type error, you can verify that it's now being checked against this interface file.

When defining the interface for a library, you can use the `any` type whenever you don't need Flow to check a value. This lets you gradually add type definitions for the parts of the library you care most about. See the reference guide on [declarations](declarations.html) for more details.
