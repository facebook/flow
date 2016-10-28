/* @flow */
/*
---
id: quick-reference
title: Quick Reference
permalink: /docs/quick-reference.html
prev: advanced-configuration.html
next: syntax.html
---
*/
/*
  ## Primitives

  Flow has types for all of the JavaScript **primitive types**.

  * [boolean](builtins.html#boolean)
  * [number](builtins.html#mixed)
  * [string](builtins.html#string)
  * [null](builtins.html#null-and-void)
  * [void](builtins.html#null-and-void)

  See [Built-in Types](builtins.html) for more information and examples.

  ## any

  The `any` type is a supertype *and* subtype of all types.

  Code that uses `any` is effectively unchecked, and should be avoided when
  another type can be used instead.

  It can be useful to opt out of type checking, however. In particular, it is
  very useful when converting an existing code base to use types.

  It is also occasionally necessary to bypass the type checker. There are a
  (decreasing) number of JS idioms which Flow is not able to type statically. In
  these instances, it is practical and reasonable to use `any`.

  See [Built-in Types](builtins.html#any) for more about `any`.

  ## mixed

  The `mixed` type is a supertype of all types.

  This type is particularly useful when paired with [dynamic type
  tests](dynamic-type-tests.html). For example, you can use `mixed` to annotate
  a value of an unknown type, and Flow will ensure that you perform the
  necessary type tests to use the value safely.

  See [Built-in Types](builtins.html#mixed) for more about `mixed`.

  ## Arrays

  This type describes JavaScript array objects and the type of the elements
  contained within the array.

  Indexing into an array with type `Array<T>` will always yield a value with
  type `T`. That is, Flow assumes arrays are dense and does not do bounds
  checking.
*/

let array: number[] = [1, 2, 3.14, 42];
let theAnswer: number = array[3]; // 42
let offTheEnd: number = array[100]; // No error

let array2: Array<string> = ["an alternate", "syntax", "for arrays"];

/*
  ### Tuples

  Tuple types are a kind of array type particularly suited to describe finite,
  heterogeneous collections.
*/

let tuple: [string, number, boolean] = ["foo", 0, true];

// Indexing into the array will return the type at a given index.
(tuple[0]: string);
// $ExpectError
(tuple[1]: string);

// Indexing into an statically unknown index will return a general type.
declare var unknownNumber: number;
// `void` is none of `string`, `number`, or `boolean`
// $ExpectError
(tuple[unknownNumber]: void);
(tuple[unknownNumber]: string|number|boolean); // OK

// Values written must be compatible with the type at that index.
tuple[1] = -1;
// $ExpectError
tuple[0] = false;

/*
  See [Arrays](arrays.html) for more information and examples.

  ## Objects

  This type describes any object values that match a specified shape.
*/

let object: {foo: string, bar: number} = {foo: "foo", bar: 0};
(object.foo: string);

// Property writes must be compatible with the declared type.
// $ExpectError
object.bar = "bar";

/*
  ### Objects as maps

  Objects are often used as lookup tables, or maps. While the `Map` type is more
  suitable for this use case, Flow does support this common idiom.
*/

let coolRating: {[id:string]: number} = {};
coolRating["sam"] = 10; // Yes, it's a 0-10 scale.

/*
  ### Callable objects

  Functions are also objects, and may have other props. Flow models this as an
  object type with a callable property.
*/

function makeCallable(): { (x: number): string; foo: number } {
  function callable(number) {
    return number.toFixed(2);
  }
  callable.foo = 123;
  return callable;
}

var callable = makeCallable();

var callableReturn: string = callable(Math.PI); // "3.14"
var callableFoo: number = callable.foo; // 123

/*
  ### The `Object` type

  The `Object` type is a supertype of all object types. A value of this type
  supports property access by any property name, and will return a value with
  type `any`.

  Like `any`, this type should be used sparingly.
*/

var anyObject: Object = {};
anyObject.foo.bar.baz; // OK

/*
  See [Objects](objects.html) for more information and examples.

  ## Functions

  Functions take zero or more arguments and optionally return a value. In
  addition to "standard" function declarations, Flow supports arrow functions,
  async functions, and generator functions.
*/

function greatestCommonDivisor(a: number, b: number): number {
  if (!b) {
    return a;
  }

  return greatestCommonDivisor(b, a % b);
}

// Annotations included for example purposes only.
[1, 2, 3].map((num: number): number => num * 2)

async function getFriendNames(
  friendIDs: Promise<number[]>,
  getFriendName: (id: number) => Promise<string>,
): Promise<string[]> {
  var ids = await friendIDs;
  var names = await Promise.all(ids.map(getFriendName));
  return names;
}

function *infinity(): Generator<number,void,void> {
  var n = 0;
  while (true) {
    yield n++;
  }
}

/*
  ### The `Function` type

  The `Function` type is a supertype of all function types. A value of this type
  may be called with any number of any type of parameters, and will return a
  value with type `any`.

  Like `any`, this type should be used sparingly.
*/

var anyFunction: Function = () => {};
anyFunction("foo", "bar").baz.quux; // OK

/*
  See [Functions](functions.html) for more information and examples.

  ## Classes

  Defining a class also defines a type, which can be used to annotate instances
  of that class.
*/

class MyClass {
  foo: string;
  constructor(foo: string) {
    this.foo = foo;
  }
  bar(): string {
    return this.foo;
  }
}

var myInstance: MyClass = new MyClass("foo");
(myInstance.foo: string);
(myInstance.bar(): string);

/*
  ### Interfaces

  Classes are nominally typed in Flow. That means that two classes are only
  compatible if they have an explicit subtyping relationship, via `extends`. It
  is often useful to describe a set of types which are structurally similar.
*/

interface Fooable {
  foo(): string;
}

class AFoo {
  foo() { return "foo from A" };
}

class BFoo {
  foo() { return "foo from B" };
}

(new AFoo: Fooable);
(new BFoo: Fooable);

/*
  ### Sugar-free classes

  ES2015 classes formalize the common practice of simulating class-like
  inheritance with functions and prototypes. Flow does have limited support for
  this pattern as well, but the ES2015 class syntax is highly recommended.
*/

function DietClass(foo: string) {
  this.foo = foo;
}

DietClass.prototype.bar = function() {
  return this.foo;
}

var myDietInstance: DietClass = new DietClass("foo");
(myDietInstance.foo: string);
(myDietInstance.bar(): string);

/*
  ### The `Class<T>` type

  Remember that the name of the class as a type annotation represents instances
  of that class. Given a type `T` representing instances of a class `C`, the
  type `Class<T>` is the type of the class `C`.
*/

var myClass: Class<MyClass> = MyClass;
var myInstance2 = new myClass("foo");

/*
  See [Classes](classes.html) for more information and examples.

  ## Type aliases

  Instead of writing out a potentially complex type multiple times, define a
  type alias instead.
*/

type ObjectWithManyProperties = {
  foo: string,
  bar: number,
  baz: boolean,
  qux: (foo: string, bar: number) => boolean;
}

/*
  See [Type Aliases](type-aliases.html) for more information and examples.

  ## Generics

  Generics make it possible to abstract over types. A common example is
  container classes, which store, traverse, and retrieve data without knowledge
  of specifically what is inside.

  Generic object types can be specified using a type alias.
*/

type GenericObject<T> = { foo: T };
var numberObject: GenericObject<number> = { foo: 0 };
var stringObject: GenericObject<string> = { foo: "foo" };

/*
  Type parameters for classes are specified in the class declaration.
*/

class GenericClass<T> {
  x: T;
  constructor(x: T) {
    this.x = x;
  }
}

var numberInstance: GenericClass<number> = new GenericClass(0);
var stringInstance: GenericClass<string> = new GenericClass("");

/*
  Type parameters for functions are specified in the function declaration.
*/

function findMax<T>(arr: T[], compare: (a: T, b: T) => number) {
  var sorted = arr.sort(compare);
  return sorted[sorted.length - 1];
}
