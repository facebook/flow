/* @flow */
/*
---
id: classes
title: Classes
permalink: /docs/classes.html
prev: arrays.html
next: objects.html
---
*/

/*
  Classes were introduced in JavaScript to formalize the common practice of
  simulating class-like inheritance hierarchies in JavaScript with functions
  and prototypes.

  In Flow, a class may be defined using
  [standard syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Classes)
  extended with field declarations.
*/

class C {
  x: string;
  y: number;
  constructor(x) { this.x = x; }
  foo() { return this.x; }
  bar(y) { this.y = y; }
}

class D extends C {
  foo() { return super.foo() + "!"; }
  bar(y) { super.bar(y || 0); }

  static qux() { return new D("hello"); }
}

/*
  In the code above, class `C` defines fields `x` and `y`, which are typed as
  `string` and `number`, respectively. `C` also defines a constructor and a
  few methods. class `D` then overrides those methods, and adds a static method.

  Just like other languages with classes, Flow enforces that the type of
  an overridden method in a superclass (e.g., `bar` in `C`) is compatible with
  the type of an overriding method (e.g., `bar` in `D`). This ensures that
  subclassing implies subtyping, i.e., the following code type checks:
*/

var c: C = new D("D extends C");

/*
  ## Type Annotations vs. Inference

  ### Inference, Locally

  Notice that the class definitions above have no type annotations, apart
  from their field declarations. Flow uses type inference extensively in
  _local_ contexts, to avoid excessive annotation (and annotating).

  In the above classes, all methods are in fact strongly typed: Flow
  has propagated types through method calls and field accesses to infer
  a static type for each method. So, for example, the following code fails
  to typecheck:
*/

// $ExpectError
var n: number = c.foo();  // string is incompatible with number

/*
  ### Annotations, Globally

  In general, type inference adds convenience and helps to keep code
  from becoming cluttered with redundant type annotations. But a lack of
  annotations can also reduce clarity - in particular, when errors occur
  far from their causes.

  For this reason, Flow requires exported definitions
  be more thoroughly annotated than ones used only locally.

  Exported classes must annotate their method signatures - parameter
  and return types - in addition to field declarations:
*/

class ExportC {
  x: string;
  y: number;
  constructor(x: string) { this.x = x; }
  foo(): string { return this.x; }
  bar(y: number): void { this.y = y; }
}

class ExportD extends ExportC {
  foo(): string { return super.foo() + "!"; }
  bar(y?: number): void { super.bar(y || 0); }

  static qux(): ExportD { return new ExportD("hello"); }
}

module.exports.C = ExportC;
module.exports.D = ExportD;

/*
  ## Structural vs. Nominal typing for Classes

  Flow treats classes as **nominal** types: structurally identical classes
  are not interchangeable, and one class is only a subtype of another
  if it has been explicitly declared as a subclass using `extends`.
*/

// structurally identical to C, but nominally unrelated
class E {
  x: string;
  y: number;
  constructor(x) { this.x = x; }
  foo() { return this.x; }
  bar(y) { this.y = y; }
}

// $ExpectError
var eAsC: C = new E("hi"); // nope, E is incompatible with C

/*
  However, [Object](/docs/objects.html) and Interface types
  are structural. Classes implement interfaces, and satisfy object shapes,
  on a structural basis:
*/

// class C has everything it needs to satisfy this interface
interface ILikeC {
  x: string;
  y: number;
  foo(): string;
  bar(y: number): void;
}

function takesAnILikeC(c: ILikeC): string { return c.foo(); }

var c: C = new C("implements ILikeC");

var s: string = takesAnILikeC(c);

/**/

// similarly, C satisfies this object shape
type XY = { x: string; y: number; };

function takesAnXY(xy: XY): number { return xy.y; }

var c: C = new C("satisfies XY");

var n: number = takesAnXY(c);

/*
  ## Polymorphic classes

  Class definitions can be polymorphic, meaning that they can represent
  a family of classes of the same "shape" but differing only in the
  instantiation of their type parameters.

  Consider a polymorphic version of class `C` above:
*/

class PolyC<X> {
  x: X;
  y: number;
  constructor(x) { this.x = x; }
  foo() { return this.x; }
  bar(y) { this.y = y; }
}

/*
  The class `C` is polymorphic in the type parameter `X`. Flow checks that the
  parts of `C` which refer to `X` are correct for any instantiation of `X`.

  Thus, when class `InstanceD` extends `PolyC<string>`,
  Flow can conclude that the latter has a method with signature
  `foo(): string`, and (as usual) check that it is compatible
  with the type of `foo` in `InstanceD`.
*/

class InstanceD extends PolyC<string> {
  foo() { return super.foo() + "!"; }
  bar(y) { super.bar(y || 0); }
}

/*
  ### Bounded polymorphism

  Type parameters can optionally specify constraints that are enforced on
  instantiation. Such constraints can be assumed to hold in the body of a
  polymorphic class definition. See
  [this blog post on bounded polymorphism]({% post_url 2015-03-12-Bounded-Polymorphism %})
  for details.

  ## Polymorphism and Type Parameter Variance

  By default, polymorphic classes are invariant in their type parameters,
  which means that an expression of type `C<T>` may flow to a location typed
  `C<U>` only when `T` and `U` are simultaneously subtypes of each other.

  For example, this read/write map of values of type `V` is only compatible
  with another map of values whose type is *both* a subtype and supertype
  of `V`.
*/

class A { x: number; }
class B extends A { y: string; }

class ReadWriteMap<K, V> {
  store: { [k:K]: V };
  constructor() { this.store = {}; }
  get(k: K): ?V { return this.store[k]; }
  put(k: K, v: V): void { this.store[k] = v; }
}

declare var mapOfB: ReadWriteMap<string, B>;

// error: mapOfB.get(k): A is fine, but consider mapOfB.put(k, (a: A))
// $ExpectError
var mapOfA: ReadWriteMap<string, A> = mapOfB;

/*
  However, a polymorphic class may specify that a given type parameter is
  co- or contravariant, meaning that one instance is compatible with another
  if one type argument is a sub- or supertype, respectively, of the other.

  **Covariance** is useful when a type parameter only appears in output (or
  "positive") positions within a class definition:
*/

class ReadOnlyMap<K, +V> {
  store: { +[k:K]: V };
  constructor(store) { this.store = store; }
  get(k: K): ?V { return this.store[k]; }
}

declare var readOnlyMapOfB: ReadOnlyMap<string, B>;
// ok: B is a subtype of A, and V is a covariant type param.
var readOnlyMapOfA: ReadOnlyMap<string, A> = readOnlyMapOfB;

/*
  Analogously, **contravariance** is useful when a type parameter
  only appears in input (or "negative") positions within a class definition.

  Note: type parameter variance may be specified in any polymorphic
  types, not just polymorphic classes.

  ## This type

  Within a class definition, the `this` type is available for use
  in annotations. `this` can improve the precision of certain types
  in the presence of inheritance.

  Intuitively, the meaning of the `this` type is as follows: consider a class
  `C` and a class `D` extending `C`. At runtime, the value of `this` within
  methods of `C` will sometimes be an instance of `C`, and sometimes an instance
  of `D`.

  Within the body of `C`, then, the `this` type denotes *both* `C` and `D`.
  More generally, the `this` type behaves exactly like a type parameter on
  a class, ranging over that class and all of its subclasses.

  An immediate consequence is that `this` may only appear in output
  (aka covariant, "positive") positions, if subtyping is to be preserved.

  The payoff comes in the form of improved precision in the types of superclass
  methods *from the perspective of subclasses*: for example, a method of `C`
  that returns a value of type `this` can safely be viewed as returning `D` when
  invoked on an instance of `D`.
*/

class ThisA {
  // $ExpectError
  x: this;                            // error: input/output position
  foo(): this { return this; }        // ok: output position
  // $ExpectError
  bar(x: this): void { this.x = x; }  // error: input position
}

class ThisB extends ThisA { }

var b: ThisB = (new ThisB).foo(); // ok: foo() on a ThisB returns a ThisB

/*
  ## `Class<T>` type

  The name of a class, used as a type annotation, represents instances of that
  class, not the class itself. It is often useful, however, to refer to the
  types of classes.

  Given a type `T` representing instances of a class `C`, the type `Class<T>` is
  the type of the class `C`.
*/

var theClass: Class<C> = C;
var anInstance = new C("foo");
