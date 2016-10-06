---
id: third-party
title: Checking third-party code
permalink: /docs/third-party.html
prev: type-annotations.html
next: running.html
---

Most real JavaScript programs depend on third-party libraries. 
To handle this, Flow supports the concept of a "Library Definition" 
(or "libdef" for short).

# Library Definitions

Often you will want to use libraries and code that you didn't write -- such as
npm packages. For these circumstances, Flow supports the concept of a 
"libdef" which allows you to describe the interface and types of the library 
seperate from the library and without needing to add types to or change the 
library itself. 

It's always possible to write a libdef file yourself if you need to, but for 
public libraries we recommend using [flow-*typed*](https://github.com/flowtype/flow-typed/).
flow-*typed* is a community-run project that organizes and shares existing, 
high-quality libdefs for popular libraries.

If you can't find the libdef you need there, consider writing one and then
contributing it back so that the next person who needs it can benefit!

Note that if a 3rd party library is present in your project that isn't itself
typed, Flow will treat it just like any other untyped JS file in your project: 
It and all imports from it will be marked as `any` by default. This is the only 
place in Flow where `any` is ever injected in to your program by Flow.

# Using flow-*typed*

Using the [flow-*typed*](https://github.com/flowtype/flow-typed/) project for npm 
projects is pretty straightforward. First, make sure you have the `flow-typed` 
CLI installed:

```bash
$> npm install -g flow-typed
```

Then, from within your project directory, run `flow-typed install` after running 
`npm install`:

```bash
$> npm install
$> flow-typed install
```

The `flow-typed install` command will do the following:

1. Read your `package.json` to identify the dependencies of your project
1. For each dependency, look in the flow-*typed* repo for a compatible libdef.
1. For each dependency:
  * **If a compatible libdef is found**, download it into the 
     `<PROJECT_ROOT>/flow-typed/` directory.
  * **If no compatible libdef is found** and the library does not come with types, 
     generate a stub libdef so that you can go fill in the types yourself. Don't
     forget to contribute your work back up to flow-*typed* when you're done :)

Since libdefs can be improved over time, **we recommend that you commit all
libdefs into version control.** This ensures that all developers working in your
project's repository have a consistent and predictable set of types. It is good 
to periodically run `flow-typed update` to check and see if there have been any 
improvements or bug fixes made to the libdefs you've installed for your project.

## Writing custom library definition files

Library definition files ("libdefs") define the type interface to a library 
separately from the actual code of the library. You never need to change library 
code to use a library definition, but your code will be typechecked against the 
types declared in the libdef.

For libdefs of public libraries, consider using and contributing to 
[flow-*typed*](https://github.com/flowtype/flow-typed/). If you have private 
libraries or just want to write your own libdefs, here is the workflow for doing 
so:

* Do not change the library files or add `@flow` to them
* Add one or more libdefs for your libraries in to the 
  "`<PROJECT_ROOT>/flow-typed/`" directory of your project. Flow recognizes this
  directory as special by default and interprets it as a directory that contains
  libdef files.

If you need to customize the directory where libdefs are stored in your project,
you can do so by adding a `[libs]` configuration to your .flowconfig file:

```
[libs]
my-custom-libdefs-directory/
```

## Example: Writing a custom libdef

To illustrate this workflow, we'll start with the [lodash](https://lodash.com) 
library. Let's say we have a simple file in our project using lodash:

**main.js**

```js +line_numbers
// @flow

import _ from "lodash";

var pizzas = [
  { title: 'Margherita', vegetarian: true },
  { title: 'Pepperoni', vegetarian: false },
  { title: 'Four cheese', vegetarian: true },
  { title: 'Hawaiian', vegetarian: false },
];

function vegetarianPizzas() {
  return _.ffind(pizzas, {vegetarian: true});
}
```


Running `flow` will produce no errors:

```bash
$> flow
Found 0 errors
```

If you look closely at our example, though, you'll notice there is a bug! We've
misspelled `find` as `ffind` on line 13.

The problem here is that Flow doesn't know anything about the lodash library
since it is untyped (i.e. its code lacks `@flow` comments). To fix this we need
a libdef file for lodash -- so let's write one ourselves:

**flow-typed/lodash.js**

```js
declare module "lodash" {
  declare module.exports: {
    find<T>(list: Array<T>, properties: Object): T;
  };
}
```

Here we've written a file that contains a `declare module` statement.
`declare module` inside a libdef tells Flow about the interface defined by a
module of the given name.

Within the body of this statement, we have another statement: 
`declare module.exports: ...`. This is how we explain to Flow what the type of
the `module.exports` object inside this module should be. In this case, we say
that it is an object type with one `find` method.

Everything after the `:` in "`declare module.exports: ...`" is just a normal type
annotation, so you can read more about type annotations 
[here](type-annotations.html).

Now we can run flow again and see that Flow gives us an error as we'd expect:

```bash
$> flow
main.js:13
 13:   return _.ffind(pizzas, {vegetarian: true});
                ^^^^^ property `ffind`. Property not found in
 13:   return _.ffind(pizzas, {vegetarian: true});
              ^ object type


Found 1 error
```

If we fix the typo in our code and run Flow again, we'll see that the Flow 
error goes away:

```bash
$> flow
Found 0 errors
```

For more information about `declare` statements in libdefs, see the reference 
guide on [declarations](declarations.html).
