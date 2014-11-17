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

Flow support *interface files* for this purpose. These files define the interface to a library, including types, separately from the actual code of the library. You never need to change library code to use interface files, but your code will be typechecked against the types declared in the interface file.

The workflow for dealing with library code is:

* Do not change the library files or add `@flow` to them
* Add one or more interface files for your libraries in a special directory in your project - for example `interfaces`
* Point Flow at those interface files by starting it with `flow start --lib  <path to your interface files>`

## Example

To illustrate this workflow, we'll pick the [Underscore](http://underscorejs.org/) library. You can find the example in `flow/examples/dependencies/``01_WithoutInterface` in the Flow installation directory. This contains the simple file:

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

Running `flow check` will unsurprisingly produce an error:

```bbcode
underscore_example.js:11:10,10: unknown global name: _
```

This is because Flow doesn't know anything about the `_` variable. To fix this we need to bring in an interface file for Underscore.

In `02_HandWrittenInterface` you can see a minimalist interface for the portion of Underscore this example uses. This is only a small part of the Underscore library, so the interface file was quick to write (it does not need to document the whole library). In fact for this minimalist example the entire interface file fits below:

{% highlight javascript linenos=table %}
declare class UnderscoreStatic {
  findWhere<T>(list: Array<T>, properties: {}): T;
}

declare var _: UnderscoreStatic;
{% endhighlight %}

This only describes (part of) the interface for Underscore, eliding all implementation details - so Flow never has to understand Underscore itself.

Running with this interface file makes the error go away:

```bash
$> flow check --lib interfaces/
```

```
Found 0 errors
```

If you modify the code you can verify that it's being checked against this interface file - errors in the use of this very small slice of Underscore would be caught.

## Generating Interface Files

> NOTE
>
> This is highly experimental. The conversion tool may or may not work for any given d.ts file. Over time we will flesh out its functionality, in the meantime please help by filing bugs!

For a well-known library like Underscore, instead of writing an interface file for a subset we can start with the [DefinitelyTyped](http://definitelytyped.org/) definition file for Underscore. You can download this file [from DefinitelyTyped on GitHub](https://github.com/borisyankov/DefinitelyTyped/blob/master/underscore/underscore.d.ts).

We can now use `flow convert` to turn it into a Flow interface file:

```bash
$> cd interfaces
# Download underscore.d.ts here
$> flow convert underscore.d.ts
# This creates a file called underscore.js
# You can now delete underscore.d.ts
```

This takes a TypeScript definition file and creates a Flow interface file from it. As before, you should Flow and point it at the interfaces directory

```bash
$> flow --lib interfaces/
```

This will again typecheck the example with no errors.
