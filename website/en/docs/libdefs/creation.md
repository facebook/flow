---
layout: guide
---

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

## Declaring A Global Function <a class="toc" id="toc-declaring-a-global-function" href="#toc-declaring-a-global-function"></a>

To declare a global function that should be accessible throughout your project,
use the `declare function` syntax in a libdef file:

**flow-typed/myLibDef.js**
```js
declare function foo(a: number): string;
```

This tells Flow that any code within the project can reference the
`foo` global function, and that the function takes one argument (a `number`) and
it returns a `string`.

## Declaring A Global Class <a class="toc" id="toc-declaring-a-global-class" href="#toc-declaring-a-global-class"></a>

To declare a global class that should be accessible throughout your project,
use the `declare class` syntax in a libdef file:

**flow-typed/myLibDef.js**
```js
declare class URL {
  constructor(urlStr: string): URL;
  toString(): string;

  static compare(url1: URL, url2: URL): boolean;
}
```

This tells Flow that any code within the project can reference the `URL` global
class. Note that this class definition does not have any implementation details
-- it exclusively defines the interface of the class.

## Declaring A Global Variable <a class="toc" id="toc-declaring-a-global-variable" href="#toc-declaring-a-global-variable"></a>

To declare a global variable that should be accessible throughout your project,
use the `declare var` syntax in a libdef file:

**flow-typed/myLibDef.js**
```js
declare var PI: number;
```

This tells Flow that any code within the project can reference the `PI` global
variable -- which, in this case, is a `number`.

## Declaring A Global Type <a class="toc" id="toc-declaring-a-global-type" href="#toc-declaring-a-global-type"></a>

To declare a global type that should be accessible throughout your project,
use the `declare type` syntax in a libdef file:

**flow-typed/myLibDef.js**
```js
declare type UserID = number;
```

This tells Flow that any code within the project can reference the `UserID`
global type -- which, in this case, is just an alias for `number`.

## Declaring A Module <a class="toc" id="toc-declaring-a-module" href="#toc-declaring-a-module"></a>

Often, third-party code is organized in terms of modules rather than globals. To
write a libdef that declares the presence of a module you'll want to use the
`declare module` syntax:

```js
declare module "some-third-party-library" {
  // This is where we'll list the module's exported interface(s)
}
```

The name specified in quotes after `declare module` can be any string, but it
should correspond to the same string you'd use to `require` or `import` the
third-party module into your project. For defining modules that are accessed via
a relative `require`/`import` path, please see the docs on the [`.flow` files](../declarations)

Within the body of a `declare module` block, you can specify the set of exports
for that module. However, before we start talking about exports we have to talk
about the two kinds of modules that Flow supports: CommonJS and ES modules.

Flow can handle both CommonJS and ES modules, but there are some relevant
differences between the two that need to be considered when using
`declare module`.

#### Declaring An ES Module <a class="toc" id="toc-declaring-an-es-module" href="#toc-declaring-an-es-module"></a>

[ES modules](http://exploringjs.com/es6/ch_modules.html) have two kinds of
exports: A **named** export and a **default** export. Flow supports the ability
to declare either or both of these kinds of exports within a `declare module`
body as follows:

###### Named Exports <a class="toc" id="toc-named-exports" href="#toc-named-exports"></a>

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

###### Default Exports <a class="toc" id="toc-default-exports" href="#toc-default-exports"></a>

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

#### Declaring A CommonJS Module <a class="toc" id="toc-declaring-a-commonjs-module" href="#toc-declaring-a-commonjs-module"></a>

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
