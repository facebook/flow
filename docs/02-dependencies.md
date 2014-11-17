---
id: dependencies
title: Using external dependencies
layout: docs
permalink: /docs/dependencies.html
prev: new-project.html
next: library.html
---

Most real JavaScript programs depend on third-party libraries. This guide shows how to use Flow in a project with external dependencies, without having to typecheck library code.

## Interface Files

Flow support *interface files* for this purpose. These files define the interface to a library, including types, separately from the actual code of the library. You never need to change library code to use interface files, but your code will be typechecked against the types declared in the interface file.

The workflow for dealing with library code is:

* Do not change the library files or add `@flow` to them
* Add one or more interface files for your libraries in a special directory in your project - for example `interfaces`
* Point Flow at those interface files by starting it with `flow start --lib  <path to your interface files>`

It is possible to write interface files yourself, but fortunately that is rarely necessary. [DefinitelyTyped](http://definitelytyped.org/) provides TypeScript definition files for many open-source libraries, and Flow can convert those definition files to Flow interface files using the `flow convert` command. 

## Example

To illustrate this workflow, we'll pick the [*Underscore*](http://underscorejs.org/) library. You can find the example in `flow/examples/dependencies` in the Flow installation directory. This contains the simple file:

{% highlight javascript linenos %}
/* @flow */

var pizzas = [
  { title: "Margherita", vegetarian: true },
  { title: "Pepperoni", vegetarian: false },
  { title: "Four cheese", vegetarian: true },
  { title: "Hawaiian", vegetarian: false },
];

function vegetarianPizzas() {
  return _.findWhere(pizzas, {vegetarian: true});
}
{% endhighlight %}

Running `flow check` will unsurprisingly produce an error:

```bbcode
flow/examples/dependencies/underscore_example.js:11:10,10: unknown global name: _
```

This is because Flow doesn't know anything about the `_` variable. To fix this we need to bring in an interface file for Underscore. Writing one from scratch would be a serious undertaking, but we can start with the [DefinitelyTyped](http://definitelytyped.org/) definition file for Underscore. You can download this file [from DefinitelyTyped on GitHub](https://github.com/borisyankov/DefinitelyTyped/blob/master/underscore/underscore.d.ts).

We can now use `flow convert` to turn it into a Flow interface file:

```bash
cd flow/examples/dependencies
mkdir interfaces
cd interfaces
# Download underscore.d.ts here
flow convert underscore.d.ts
# This creates a file called underscore.js
# You can now delete underscore.d.ts
```

This takes a TypeScript definition file and creates a Flow interface file from it. The next step is to tell Flow where our interface files are located, by calling it with the `--lib` option:

```bash
flow --lib interfaces
```

TODO
