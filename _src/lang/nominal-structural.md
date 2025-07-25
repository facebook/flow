---
title: Nominal & Structural Typing
slug: /lang/nominal-structural
---

A static type checker can use either the name (nominal typing) or the structure (structural typing)
of types when comparing them against other types (like when checking if one is a [subtype](../subtypes) of another).

## Nominal typing {#toc-nominal-typing}

Languages like C++, Java, and Swift have primarily nominal type systems.

```js
// Pseudo code: nominal system
class Foo { method(input: string) { /* ... */ } }
class Bar { method(input: string) { /* ... */ } }

let foo: Foo = new Bar(); // Error!
```

In this pseudo-code example, the nominal type system errors even though both classes have a method of the same name and type.
This is because the name (and declaration location) of the classes is different.

## Structural typing {#toc-structural-typing}

Languages like Go and Elm have primarily structural type systems.

```js
// Pseudo code: structural system
class Foo { method(input: string) { /* ... */ } }
class Bar { method(input: string) { /* ... */ } }

let foo: Foo = new Bar(); // Works!
```

In this pseudo-code example, the structural type system allows a `Bar` to be used as a `Foo`,
since both classes have methods and fields of the same name and type.

If the shape of the classes differ however, then a structural system would produce an error:

```js
// Pseudo code
class Foo { method(input: string) { /* ... */ } }
class Bar { method(input: number) { /* ... */ } }

let foo: Foo = new Bar(); // Error!
```

We've demonstrated both nominal and structural typing of classes, but there are
also other complex types like objects and functions which can also be either
nominally or structurally compared.
Additionally, a type system may have aspects of both structural and nominal systems.

## In Flow

Flow uses structural typing for objects and functions, but nominal typing for classes.

### Functions are structurally typed {#toc-functions-are-structurally-typed}

When comparing a [function type](../../types/functions) with a function it must have the same structure
in order to be considered valid.

```js flow-check
type FuncType = (input: string) => void;
function func(input: string) { /* ... */ }
let test: FuncType = func; // Works!
```

### Objects are structurally typed {#toc-objects-are-structurally-typed}

When comparing an [object type](../../types/objects) with an object it must have the same structure
in order to be considered valid.

```js flow-check
type ObjType = {property: string};
let obj = {property: "value"};
let test: ObjType = obj; // Works
```

### Classes are nominally typed {#toc-classes-are-nominally-typed}

When you have two [classes](../../types/classes) with the same structure, they still are not
considered equivalent because Flow uses nominal typing for classes.

```js flow-check
class Foo { method(input: string) { /* ... */ } }
class Bar { method(input: string) { /* ... */ } }
let test: Foo = new Bar(); // Error!
```

If you wanted to use a class structurally you could do that using an [interface](../../types/interfaces):

```js flow-check
interface Interface {
  method(value: string): void;
};

class Foo { method(input: string) { /* ... */ } }
class Bar { method(input: string) { /* ... */ } }

let test1: Interface = new Foo(); // Works
let test2: Interface = new Bar(); // Works
```

### Opaque types
You can use [opaque types](../../types/opaque-types) to turn a previously structurally typed alias into a nominal one (outside of the file that it is defined).

```js flow-check
// A.js
export type MyTypeAlias = string;
export opaque type MyOpaqueType = string;

const x: MyTypeAlias = "hi"; // Works
const y: MyOpaqueType = "hi"; // Works
```

In a different file:

```js
// B.js
import type {MyTypeAlias, MyOpaqueType} from "A.js";

const x: MyTypeAlias = "hi"; // Works
const y: MyOpaqueType = "hi"; // Error! `MyOpaqueType` is not interchangable with `string`
//                      ^^^^ Cannot assign "hi" to y because string is incompatible with MyOpaqueType
```

### Flow Enums

[Flow Enums](../../enums) do not allow enum members with the same value, but which belong to different enums, to be used interchangeably.

```js flow-check
enum A {
  X = "x",
}
enum B {
  X = "x",
}

const a: A = B.X; // Error!
```
