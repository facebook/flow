---
title: Announcing Bounded Polymorphism
short-title: Bounded Polymorphism
author: Avik Chaudhuri
hide_table_of_contents: true
---

As of Flow 0.5.0, you can define polymorphic functions and classes with bounds on their type parameters. This is extremely useful for writing functions and classes that need some constraints on their type parameters. Flow's bounded polymorphism syntax looks like

```JavaScript
class BagOfBones<T: Bone> { ... }
function eat<T: Food>(meal: T): Indigestion<T> { ... }
```

## The problem

Consider the following code that defines a polymorphic function in Flow:

```JavaScript
function fooBad<T>(obj: T): T {
  console.log(Math.abs(obj.x));
  return obj;
}
```

This code does not (and should not!) type check. Not all values `obj: T` have a property `x`, let alone a property `x` that is a `number`, given the additional requirement imposed by `Math.abs()`.

<!--truncate-->

But what if you wanted `T` to not range over all types, but instead over only the types of objects with an `x` property that has the type `number`? Intuitively, given that condition, the body should type check. Unfortunately, the only way you could enforce this condition prior to Flow 0.5.0 was by giving up on polymorphism entirely! For example you could write:

```JavaScript
// Old lame workaround
function fooStillBad(obj: { x: number }): {x: number } {
  console.log(Math.abs(obj.x));
  return obj;
}
```

But while this change would make the body type check, it would cause Flow to lose information across call sites. For example:

```JavaScript
// The return type of fooStillBad() is {x: number}
// so Flow thinks result has the type {x: number}
var result = fooStillBad({x: 42, y: "oops"});

// This will be an error since result's type
// doesn't have a property "y"
var test: {x: number; y: string} = result;
```

## The solution

As of version 0.5.0, such typing problems can be solved elegantly using bounded polymorphism. Type parameters such as `T` can specify bounds that constrain the types that the type parameters range over. For example, we can write:

```JavaScript
function fooGood<T: { x: number }>(obj: T): T {
  console.log(Math.abs(obj.x));
  return obj;
}
```

Now the body type checks under the assumption that `T` is a subtype of `{ x: number }`. Furthermore, no information is lost across call sites. Using the example from above:

```JavaScript
// With bounded polymorphism, Flow knows the return
// type is {x: number; y: string}
var result = fooGood({x: 42, y: "yay"});

// This works!
var test: {x: number; y: string} = result;
```

Of course, polymorphic classes may also specify bounds. For example, the following code type checks:

```JavaScript
class Store<T: { x: number }> {
  obj: T;
  constructor(obj: T) { this.obj = obj; }
  foo() { console.log(Math.abs(this.obj.x)); }
}
```

Instantiations of the class are appropriately constrained. If you write

```JavaScript
var store = new Store({x: 42, y: "hi"});
```

Then `store.obj` has type `{x: number; y: string}`.

Any type may be used as a type parameter's bound. The type does not need to be an object type (as in the examples above). It may even be another type parameter that is in scope. For example, consider adding the following method to the above `Store` class:

```JavaScript
class Store<T: { x: number }> {
  ...
  bar<U: T>(obj: U): U {
    this.obj = obj;
    console.log(Math.abs(obj.x));
    return obj;
  }
}
```

Since `U` is a subtype of `T`, the method body type checks (as you may expect, `U` must also satisfy `T`'s bound, by transitivity of subtyping). Now the following code type checks:

```JavaScript
  // store is a Store<{x: number; y: string}>
  var store = new Store({x: 42, y: "yay"});

  var result = store.bar({x: 0, y: "hello", z: "world"});

  // This works!
  var test: {x: number; y: string; z: string } = result;
```

Also, in a polymorphic definition with multiple type parameters, any type parameter may appear in the bound of any following type parameter. This is useful for type checking examples like the following:

```JavaScript
function copyArray<T, S: T>(from: Array<S>, to: Array<T>) {
  from.forEach(elem => to.push(elem));
}
```

## Why we built this

The addition of bounded polymorphism significantly increases the expressiveness of Flow's type system, by enabling signatures and definitions to specify relationships between their type parameters, without having to sacrifice the benefits of generics. We expect that the increased expressiveness will be particularly useful to library writers, and will also allow us to write better declarations for framework APIs such as those provided by React.

## Transformations

Like type annotations and other Flow features, polymorphic function and class definitions need to be transformed before the code can be run. The transforms are available in react-tools `0.13.0`, which was recently released
