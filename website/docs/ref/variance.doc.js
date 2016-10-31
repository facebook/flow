/* @flow */
/*
---
id: variance
title: Variance
permalink: /docs/variance.html
prev: functions.html
next: nullable-types.html
---
*/

/*
Defining the subtype relationship between types is a core resposibility of Flow
as a type system. These relationships are determined either directly for
simple types or, for complex types, defined in terms of their parts.

Variance describes the subtyping relationship for complex types as it relates to
the subtyping relationships of their parts.

For example, Flow directly encodes the knowledge that `string` is a subtype of
`?string`. Intuitively, a `string` type contains string values while a `?string`
type contains `null`, `undefined`, and also string values, so membership in the
former naturally implies membership in the later.

The subtype relationships between two function types is not as direct. Rather,
it is derived from the subtype relationships between the functions' parameter
and return types.
*/

/*
## Function Types

Consider the two function types below:
*/

type F1 = (x: P1) => R1;
type F2 = (x: P2) => R2;

/*
Whether `F2` is a subtype of `F1` depends on the relationships between `P1` and
`P2` and `R1` and `R2`. Let's use the notation `B <: A` to mean `B` is a subtype
of `A`.

It turns out that `F2 <: F1` if `P1 <: P2` and `R2 <: R1`. Notice that the
relationship for parameters is reversed? In technical terms, we can say that
function types are "contravariant" with respect to their parameter types and
"covariant" with respect to their return types.
*/

class P2 {}
class P1 extends P2 {}

class R1 {}
class R2 extends R1 {}

/*
Let's look at an example:
*/

function f(callback: F1) {
  var result: R1 = callback(new P1);
}

/*
What kinds of functions can we pass to `f`? Based on the subtyping rule above,
then we can pass a function whose parameter type is a supertype of `P1` and
whose return type is a subtype of `R1`.
*/

function g(x: P2) {
  return new R2;
}

f(g);

/*
The body of `f` will only ever pass `P1` values into `g`, which is safe because
`g` takes at least `P1` by taking `P2`. Conversely, `g` will only ever return
`R2` values to `f`, which is safe because `f` handles at least `R2` by handling
`R1`.
*/

/*
### Input and Output

One convenient way to remember when something is covariant vs. contravariant is
to think about "input" and "output."

Parameters are in an *input* position, often called a "negative" position.
Complex types are contravariant in their input positions.

Return is an *output* position, often called a "positive" position. Complex
types are covariant in their output positions.
*/

/*
## Object Types

Just as function types are composed of parameter and return types, so too are
object types composed of property types. Thus, the subtyping relationship
between objects is derived from the subtyping relationships of their properties.

However, unlike functions which have input parameters and an output return,
object properties can be read and written. That is, properties are *both* input
and output.

Let's see how this works for two simple object types:
*/

type O1 = {p: T1};
type O2 = {p: T2};

/*
As with function types, whether `O2` is a subtype of `O1` depends on the
relationship between its parts, `T1` and `T2`.

Here it turns out that `O2 <: O1` if `T2 <: T1` *and* `T1 <: T2`. In technical
terms, object types are "invariant" with respect to their property types.
*/

class T0 {}
class T1 extends T0 { q: ?string }
class T2 extends T1 {}

/*
Let's look at an example:
*/

function h(o: {p: T1}): void {
  // We can read p from o
  let len: number;
  if (o.p.q) {
    len = o.p.q.length;
  } else {
    len = 0;
  }

  // We can also write into p
  o.p = new T1;
}

/*
What kinds of objects can we pass into `h`, then? If we try to pass in an object
with a subtype property, we get an error:
*/

var o1: {p: T2} = {p: new T2};
// $ExpectError
h(o1);

/*
Flow has correctly identified an error here. If the body of `h` writes `T1`
into `o.p`, then `o1.p` would no longer have type `T2`.

If we try to pass an object with a supertype property, we again get an error:
*/

var o2: {p: T0} = {p: new T0};
// $ExpectError
h(o2);

/*
Again, Flow correctly identifies an error, because if `h` tried to read `p` from
`o`, it would find a T0.
*/

/*
So objects have to be invariant with respect to their property types because
properties can be read from and written to. But just because you *can* read and
write, doesn't mean you always do.

Consider a function that gets the length of an nullable string property:
*/

function getLength(o: {+p: ?string}): number {
  return o.p ? o.p.length : 0;
}

/*
Here we've explicitly annotated the property +p, indicating that the property is
covariant. Now we are able to pass in an object with a subtype property:
*/

var o3: {p: string} = {p: "hello"};
getLength(o3);

/*
It's crucial that covariant properties only ever appear in output positions. It
is an error to write to a covariant property:
*/

function covariantPropertyWrite(o: {+p: string}) {
  // $ExpectError
  o.p = "bad";
}

/*
Conversely, if a function only ever writes to a property, we can annotate the
property as contravariant. This might come up in a function that initializes an
object with default values, for example.
*/

function initDefaults(o: {-p: string}): void {
  o.p = "default";
}

var o4: {p: ?string} = {p: null};
initDefaults(o4);

/*
Contravariant properties can only ever appear in input positions. It is an
error to read from a contravariant property:
*/

function contravariantPropertyRead(o: {-p: string}) {
  // $ExpectError
  o.p.length;
}

/*
## User-defined Classes

See [Polymorphism and Type Parameter Variance](/docs/classes.html#polymorphism-and-type-parameter-variance).
*/
