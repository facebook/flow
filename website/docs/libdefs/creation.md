---
title: Creating Library Definitions
slug: /libdefs/creation
---

import {SinceVersion} from '../../components/VersionTags';

Before spending the time to write your own libdef, we recommend that you look to
see if there is already a libdef for the third-party code that you're addressing.
`flow-typed` is a [tool and repository](https://github.com/flowtype/flow-typed/)
for sharing common libdefs within the Flow community -- so it's a good way to
knock out a good chunk of any public libdefs you might need for your project.

However sometimes there isn't a pre-existing libdef or you have third-party
code that isn't public and/or you really just need to write a libdef yourself.
To do this you'll start by creating a `.js` file for each libdef you're going to
write and put them in the `/flow-typed` directory at the root of your project.
In these libdef file(s) you'll use a special set of Flow syntax (explained
below) to describe the interfaces of the relevant third-party code.

## Declaring A Global Function {#toc-declaring-a-global-function}

To declare a global function that should be accessible throughout your project,
use the `declare function` syntax in a libdef file:

**flow-typed/myLibDef.js**
```js flow-check
declare function foo(a: number): string;
```

This tells Flow that any code within the project can reference the
`foo` global function, and that the function takes one argument (a `number`) and
it returns a `string`.

## Declaring A Global Class {#toc-declaring-a-global-class}

To declare a global class that should be accessible throughout your project,
use the `declare class` syntax in a libdef file:

**flow-typed/myLibDef.js**
```js flow-check
declare class URL {
  constructor(urlStr: string): URL;
  toString(): string;

  static compare(url1: URL, url2: URL): boolean;
}
```

This tells Flow that any code within the project can reference the `URL` global
class. Note that this class definition does not have any implementation details
-- it exclusively defines the interface of the class.

## Declaring A Global Variable {#toc-declaring-a-global-variable}

To declare a global variable that should be accessible throughout your project,
use the `declare var`, `declare let`, or `declare const` syntax in a libdef file:

**flow-typed/myLibDef.js**
```js flow-check
declare const PI: number;
```

This tells Flow that any code within the project can reference the `PI` global
variable -- which, in this case, is a `number`.

## Declaring A Global Type {#toc-declaring-a-global-type}

To declare a global type that should be accessible throughout your project,
use the `declare type` syntax in a libdef file:

**flow-typed/myLibDef.js**
```js flow-check
declare type UserID = number;
```

This tells Flow that any code within the project can reference the `UserID`
global type -- which, in this case, is just an alias for `number`.

## Declaring A Global Namespace {#toc-declaring-a-global-namespace}

A namespace defines a collection of values and types:

```js flow-check
declare namespace Foo {
  declare const bar: string;
  type Baz = number;
}

Foo.bar as Foo.Baz; // error
```

To declare a global namespace that should be accessible throughout your project,
use the `declare namespace` syntax in a libdef file:

**flow-typed/myLibDef.js**
```js flow-check
declare namespace Foo {
  declare const bar: string;
  type Baz = number;
}
```

This tells Flow that any code within the project can reference the `Foo` global
namespace.

If a declared namespace only contains type declarations, then the namespace itself
can only be used in a type context.

**flow-typed/myTypeOnlyNamespace.js**
```js flow-check
declare namespace TypeOnlyFoo {
  type Baz = number;
}

TypeOnlyFoo; // error
type T = TypeOnlyFoo.Baz; // ok
```

## Declaring A Module {#toc-declaring-a-module}

Often, third-party code is organized in terms of modules rather than globals.
Flow offers two different ways to declare a module.

### Declaring a module in the `@flowtyped` directory  <SinceVersion version="0.251.0" />  {#toc-declaring-a-module-in-at-flowtyped}

Since v0.251.0, Flow has added support for easily declaring third-party modules in the
`@flowtyped` directory at the root of the project. Before looking into `node_modules` for
the module specifier `foo/bar/baz`, Flow will look into `@flowtyped/foo/bar/baz.js.flow` and
`@flowtyped/foo/bar/baz/index.js.flow`.

For example, if you want to declare the types for `react`, you can do:

```js title="@flowtyped/react.js.flow"
export type SetStateFunction<S> = ((S => S) | S) => void;
declare export function useState<S>(initial: S): [S, SetStateFunction<S>];

// Other stuff...
```

which will allow you to import these functions and types from `react`:

```js title="foo/bar/baz/my-product-code.jsx"
import * as React from 'react';

function MyComponent({onSelect}: {onSelect: React.SetStateFunction<string>}) {
  const [a, setA] = React.useState(new Set<string>());
  return <div />;
}
// Other stuff...
```

If you want to declare the types for a scoped package like `@my-company/library-a`, you can do

```js title="@flowtyped/@my-company/library-a.js.flow"
declare export const foo: string;
declare export const bar: number;
```

If you want to declare the types for a deeply nested module in a package like
`react-native/internals/foo`, you can do:

```js title="@flowtyped/react-native/internals/foo.js.flow"
declare export const SECRET_INTERNALS_Foo: {...};
```

This approach is preferrable to the approach described [below](#toc-declaring-a-module-globally),
because editing these files will not trigger a restart of Flow server.

### Declaring a module in the global namespace  {#toc-declaring-a-module-globally}

You can also declare modules using the `declare module` syntax:

```js
declare module "some-third-party-library" {
  // This is where we'll list the module's exported interface(s)
}
```

The name specified in quotes after `declare module` can be any string, but it
should correspond to the same string you'd use to `require` or `import` the
third-party module into your project. For defining modules that are accessed via
a relative `require`/`import` path, please see the docs on the [`.flow` files](../../declarations)

Within the body of a `declare module` block, you can specify the set of exports
for that module. However, before we start talking about exports we have to talk
about the two kinds of modules that Flow supports: CommonJS and ES modules.

Flow can handle both CommonJS and ES modules, but there are some relevant
differences between the two that need to be considered when using
`declare module`.

#### Declaring An ES Module {#toc-declaring-an-es-module}

[ES modules](https://developer.mozilla.org/en-US/docs/web/javascript/reference/statements/export)
have two kinds of exports: A **named** export and a **default** export. Flow supports the ability
to declare either or both of these kinds of exports within a `declare module` body as follows:

###### Named Exports {#toc-named-exports}

**flow-typed/some-es-module.js**
```js
declare module "some-es-module" {
  // Declares a named "concatPath" export
  declare export function concatPath(dirA: string, dirB: string): string;
}
```

Note that you can also declare other things inside the body of the
`declare module`, and those things will be scoped to the body of the
`declare module` -- **but they will not be exported from the module**:

**flow-typed/some-es-module.js**
```js
declare module "some-es-module" {
  // Defines the type of a Path class within this `declare module` body, but
  // does not export it. It can only be referenced by other things inside the
  // body of this `declare module`
  declare class Path {
    toString(): string;
  }

  // Declares a named "concatPath" export which returns an instance of the
  // `Path` class (defined above)
  declare export function concatPath(dirA: string, dirB: string): Path;
}
```

###### Default Exports {#toc-default-exports}

**flow-typed/some-es-module.js**
```js
declare module "some-es-module" {
  declare class URL {
    constructor(urlStr: string): URL;
    toString(): string;

    static compare(url1: URL, url2: URL): boolean;
  }

  // Declares a default export whose type is `typeof URL`
  declare export default typeof URL;
}
```

It is also possible to declare both **named** and **default** exports in the
same `declare module` body.

#### Declaring A CommonJS Module {#toc-declaring-a-commonjs-module}

CommonJS modules have a single value that is exported (the `module.exports`
value). To describe the type of this single value within a `declare module`
body, you'll use the `declare module.exports` syntax:

**flow-typed/some-commonjs-module.js**
```js
declare module "some-commonjs-module" {
  // The export of this module is an object with a "concatPath" method
  declare module.exports: {
    concatPath(dirA: string, dirB: string): string;
  };
}
```

Note that you can also declare other things inside the body of the
`declare module`, and those things will be scoped to the body of the
`declare module`, **but they will not be exported from the module**:

**flow-typed/some-commonjs-module.js**
```js
declare module "some-commonjs-module" {
  // Defines the type of a Path class within this `declare module` body, but
  // does not export it. It can only be referenced by other things inside the
  // body of this `declare module`
  declare class Path {
    toString(): string;
  }

  // The "concatPath" function now returns an instance of the `Path` class
  // (defined above).
  declare module.exports: {
    concatPath(dirA: string, dirB: string): Path
  };
}
```

NOTE: Because a given module cannot be both an ES module and a CommonJS module,
      it is an error to mix `declare export [...]` with
      `declare module.exports: ...` in the same `declare module` body.
