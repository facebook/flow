---
title: Nominal & Structural Typing
slug: /lang/nominal-structural
---

An important attribute of every type system is whether they are structural or
nominal, they can even be mixed within a single type system. So it's important
to know the difference.

A type is something like a string, a boolean, an object, or a class. They have
names and they have structures. Primitives like strings or booleans have a very
simple structure and only go by one name.

More complex types like object or classes have more complex structures. They
each get their own name even if they sometimes have the same structure overall.

A static type checker uses either the names or the structure of the types in
order to compare them against other types. Checking against the name is nominal
typing and checking against the structure is structural typing.

## Nominal typing {#toc-nominal-typing}

Languages like C++, Java, and Swift have primarily nominal type systems.

```js
class Foo { method(input: string) { /* ... */ } }
class Bar { method(input: string) { /* ... */ } }

let foo: Foo = new Bar(); // Error!
```

Here you can see a pseudo-example of a nominal type system erroring out when
you're trying to put a `Bar` where a `Foo` is required because they have
different names.

## Structural typing {#toc-structural-typing}

Languages like OCaml, Haskell, and Elm have primarily structural type systems.

```js
class Foo { method(input: string) { /* ... */ } }
class Bar { method(input: string) { /* ... */ } }

let foo: Foo = new Bar(); // Works!
```

Here you can see a pseudo-example of a structural type system passing when
you're trying to put a Bar where a `Foo` is required because their structure is
exactly the same.

But as soon as you change the shape it will start to cause errors.

```js
class Foo { method(input: string) { /* ... */ } }
class Bar { method(input: number) { /* ... */ } }

let foo: Foo = new Bar(); // Error!
```

It can get a little bit more complicated than this.

We've demonstrated both nominal and structure typing of classes, but there are
also other complex types like objects and functions which can also be either
nominal or structural. Even further, they can be different within the same type
system (most of the languages listed before has features of both).

For example, Flow uses structural typing for objects and functions, but nominal
typing for classes.

### Functions are structurally typed {#toc-functions-are-structurally-typed}

When comparing a function type with a function it must have the same structure
in order to be considered valid.

```js flow-check
// @flow
type FuncType = (input: string) => void;
function func(input: string) { /* ... */ }
let test: FuncType = func; // Works!
```

### Objects are structurally typed {#toc-objects-are-structurally-typed}

When comparing an object type with an object it must have the same structure
in order to be considered valid.

```js flow-check
type ObjType = { property: string };
let obj = { property: "value" };
let test: ObjType = obj;
```

### Classes are nominally typed {#toc-classes-are-nominally-typed}

When you have two classes with the same structure, they still are not
considered equivalent because Flow uses nominal typing for classes.

```js flow-check
// @flow
class Foo { method(input: string) { /* ... */ } }
class Bar { method(input: string) { /* ... */ } }
let test: Foo = new Bar(); // Error!
```

If you wanted to use a class structurally you could do that using an interface:

```js flow-check
interface Interface {
  method(value: string): void;
};

class Foo { method(input: string) { /* ... */ } }
class Bar { method(input: string) { /* ... */ } }

let test1: Interface = new Foo(); // Okay.
let test2: Interface = new Bar(); // Okay.
```

## Mixing nominal and structural typing {#toc-mixing-nominal-and-structural-typing}

The design decision in Flow around mixing nominal and structural typing was
chosen based on how objects, functions, and classes are already used in
JavaScript.

The JavaScript language is a bunch of object-oriented ideas and functional
ideas mixed together. Developer's usage of JavaScript tends to be mixed as
well. Classes (or constructor functions) being the more object-oriented side
and functions (as lambdas) and objects tend to be more on the functional side,
developers use both simultaneously.

When someone writes a class, they are declaring a _thing_. This thing might
have the same structure as something else but they still serve different
purposes. Imagine two component classes that both have `render()` methods,
these components could still have totally different purposes, but in a
structural type system they'd be considered exactly the same.

Flow chooses what is natural for JavaScript, and should behave the way you
expect it to.
