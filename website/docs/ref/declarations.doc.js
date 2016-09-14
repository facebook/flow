/* @flow */
/*
---
id: declarations
title: Declarations
permalink: /docs/declarations.html
prev: modules.html
next: utility-types.html
---
*/

/*
  Sometimes we just want to _declare_ the types of some definitions, so that
  code that _uses_ those definitions may be typechecked, without worrying about
  the code that _implements_ those definitions. The implementation code may
  simply not be available for typechecking (such as for JavaScript built-ins
  like `Math` or `Array`). Alternatively, the implementation code may be
  available but we may not want to typecheck it (yet). Declarations also enable
  a powerful modular approach to typechecking libraries, separating the checking
  of uses from the checking of implementations.

  As we shall see below, declarations can not only express types of definitions
  in global scope (variables, functions, classes), but also express signatures
  of modules.

  ## Declarations should be distinct from regular code

  Note that declarations are exclusively _compile-time_ entities: they are
  transpiled away, so they have no effect at run time. In fact, declarations
  without backing implementations may cause typechecked uses to break at run
  time! Thus, it is important to distinguish declaration code from regular code.

  In Flow, there are two recommended styles of doing declarations: let's call
  them the [".flowconfig"-style](#pointing-your-project-to-declarations) and the
  [".js.flow" style](#declaration-files). The styles differ in their generality
  and convenience, but share some important characteristics: both involve
  locating the declarations separately from regular code, and both use similar
  syntax.

  ## Pointing your project to declarations

  Let's start with the ".flowconfig" style of doing declarations.

  In this style, declarations (see below for examples) can be made visible to an
  entire project by putting them in files with extension `.js` in a directory,
  say `decls`, and putting the following in the `.flowconfig` of the project:

*/
// $DocIssue{not a flow file}
[libs]
// $DocIssue{not a flow file}
decls/

/*
  ## Declaring definitions that should exist at run time

  Here's a simple example, where we declare a global variable.
*/
declare var DEBUG: bool;

/*
  Other global definitions, like functions and classes, can similarly be
  declared. Note how function and method bodies are elided.
*/
declare function isLeapYear(year: string): bool;

declare class Counter {
  val: number;
  incr(): void;
}

/*
  Such declarations can be made visible to all modules in a project (see
  [above](#pointing-your-project-to-declarations)). This means that any code
  that uses these definitions in a typesafe manner will typecheck.
*/
if (isLeapYear('2016')) console.log('Yay!');

/*
  Note that it is entirely up to the programmer to ensure that declared
  definitions actually exist, and have the correct types.

  ## Mixins

  You can declare a class which mixes in 1 or more other classes with the
  `mixins` keyword. Mixing class `B` into class `A` copies `B`'s fields and
  methods into `A`. Note, however, that any fields or methods that `B` inherits
  are not copied over. Mixins are for code reuse, not for multiple inheritance.
*/

// You can mixin more than one class
declare class MyClass extends Child mixins MixinA, MixinB {}
declare class MixinA {
  a: number;
  b: number;
}
// Mixing in MixinB will NOT mix in MixinBase
declare class MixinB extends MixinBase {}
declare class MixinBase {
  c: number;
}
declare class Child extends Base {
  a: string;
  c: string;
}
declare class Base {
  b: string;
}

var c = new MyClass();
(c.a: number); // Both Child and MixinA provide `a`, so MixinA wins
(c.b: number); // The same principle holds for `b`, which Child inherits
(c.c: string); // mixins does not copy inherited properties,
               // so `c` comes from Child

/*
  ## Declaring types

  It is similarly useful to declare types. Like other declarations, type
  declarations can also be made visible to all modules in a project.
*/

/*
  Declared types include type aliases and interfaces; in fact, they share the
  same syntax as usual types, so the `declare` keyword is superfluous.
*/
declare type Response = 'yes' | 'no' | 'maybe';

declare interface Stack<T> {
  push(item: T): void;
  pop(): T;
  isEmpty(): bool;
}

/*
  ## Declaring modules

  Finally, modules can be declared as well. Like definitions that should exist
  at run time, it is up to the programmer to ensure that the declared modules
  actually exist at run time.

  Declaring a module consists of naming the module and declaring its exports. Of
  course, the exports can be variables, functions, classes, as well as type
  aliases and interfaces. Thus it is not surprising that they share exactly the
  same syntax as the declarations above, except that they are now scoped to the
  module, instead of globally.
*/
declare module Misc {
  declare var DEBUG: bool;
  declare function isLeapYear(year: string): bool;
  declare class Counter {
    val: number;
    incr(): void;
  }
  declare type Response = 'yes' | 'no' | 'maybe';
  declare interface Stack<T> {
    push(item: T): void;
    pop(): T;
    isEmpty(): bool;
  }
}

/*
  Note that a declared module should have a global name. (This can sometimes be
  a limitation; see [below](#declaration-files).) The global name could be any
  string, though: it need not be an identifier. So the following also works:
*/
declare module "fancy-pants" {
  // ...
}

/*
  A declared module would be looked up by the typechecker only when a file
  implementing that module was not found by [the resolution algorithm of the
  module system](modules.html#module-resolution), or such a file was found but
  not [checked](new-project.html#typechecking-your-files). For example, suppose
  we have the following code in a file `src/LookBeforeYouLeap.js`:
*/
// @flow
// $DocIssue{module should be found given an appropriate setup}
import { isLeapYear } from 'Misc';
if (isLeapYear('2016')) console.log('Yay!');

/*
  If the import successfully resolves to (say) a file
  `src/node_modules/Misc/index.js` with the following code, then we would get a
  Flow error, since the parameter of `isLeapYear` expects a `number`.
*/
// @flow
export function isLeapYear(year: number): bool {
  return (year % 4 == 0); // yeah, this is approximate
}

/*
  But on the other hand, if the above file doesn't exist or we remove the
  `@flow` header then we would *not* get a Flow error, since the parameter of
  `isLeapYear` in the declared module `Misc` expects a `string`.

  ## Declaration files

  Now, let's look at a more general, and sometimes more convenient, way to
  declare types for modules: the ".js.flow" style.

  The exported types of a module may be declared in a _declaration file_ with
  the `.js.flow` extension, colocated with the corresponding _implementation
  file_ with the `.js` extension. A declaration file for a module shadows a
  colocated implementation file for that module when typechecking other code
  that may depend on that module.

  For example, looking back at the file `src/LookBeforeYouLeap.js`, suppose it
  instead had the following code (note the relative-path import):
*/
// @flow
// $DocIssue{module should be found given an appropriate setup}
import { isLeapYear } from './Misc';
if (isLeapYear('2016')) console.log('Yay!');

/*
  Next, suppose that `src/Misc.js` had an incompatible implementation of
  `isLeapYear`, just as above.
*/
// @flow
export function isLeapYear(year: number): bool {
  return (year % 4 == 0); // yeah, this is approximate
}

/*
  If we now create a declaration file `src/Misc.js.flow`, the declarations in it
  will be used instead of the code in `src/Misc.js`. Let's say we have the
  following declarations in `src/Misc.js.flow`. Note that the syntax for
  declarations in a declaration file is the same as we've seen above, except
  that we also write the keyword `export`, like we would in an implementation
  file.
*/
// @flow
declare export function isLeapYear(year: string): bool;

/*
  What do you think will happen? Right, the `isLeapYear` call in
  `src/LookBeforeYouLeap.js` will typecheck.

  As this example shows, declaration files must be written with care: it is up
  to the programmer to ensure they are correct, otherwise they may hide type
  errors.

  That said, declaration files provide a very convenient way to write
  specifications for modular typechecking. Sometimes, the implementation code
  may not yet be free of type errors, but we may want to move on and come back
  to fixing the type errors later. Another important use of this feature is for
  libraries, whose implementation code may be too complex to typecheck
  satisfactorily, but whose clients we still want to typecheck against
  well-defined specifications.

  ## Inlining declarations in regular code

  As noted above, declarations should be distinct from regular code. But
  sometimes, it is useful to do declarations "inline," as part of the source of
  an implementation file. **Proceed with caution!**

  The most common use is writing "work-in-progress" code while ensuring that
  your code typechecks. In the following example, say you want to finish writing
  the function `fooList` without bothering to mock up its dependencies first: a
  function `foo` that takes a `number`, and returns a `string` and a class
  `List` that has a `map` method. Easy! (Just don't forget to replace the
  declarations with proper implementations.)
*/
declare class List<T> {
  map<U>(f: (x: T) => U): List<U>;
}
declare function foo(n: number): string;

function fooList(ns: List<number>): List<string> {
  return ns.map(foo);
}
