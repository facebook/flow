---
layout: guide
---

## `$Keys<T>` <a class="toc" id="toc-keys" href="#toc-keys"></a>

In Flow you can [use union types similar to enums](../literals/):

```js
// @flow
type Suit = "Diamonds" | "Clubs" | "Hearts" | "Spades";

const clubs: Suit = 'Clubs';
const wrong: Suit = 'wrong'; // 'wrong' is not a Suit
```

This is very handy, but sometimes you need to access the enum definition at runtime (i.e. at a value level).

Suppose for example that you want to associate a value to each suit of the previous example.

You could do

```js
// @flow
type Suit = "Diamonds" | "Clubs" | "Hearts" | "Spades";

const suitNumbers = {
  Diamonds: 1,
  Clubs: 2,
  Hearts: 3,
  Spades: 4
};

function printSuitNumber(suit: Suit) {
  console.log(suitNumbers[suit]);
}

printSuitNumber('Diamonds'); // 2
printSuitNumber('foo'); // 'foo' is not a Suit
```

but this doesn't feel very DRY, as we had to explicitly define the suit names twice.

In situations like this one, you can leverage the `$Keys<T>` operator. Let's see another example, this time using `$Keys`:

```js
// @flow
const countries = {
  US: "United States",
  IT: "Italy",
  FR: "France"
};

type Country = $Keys<typeof countries>;

const italy: Country = 'IT';
const nope: Country = 'nope'; // 'nope' is not a Country
```

In the example above, the type of `Country` is equivalent to `type Country = 'US' | 'IT' | 'FR'`, but Flow was able to extract it from the keys of `countries`.

## `$Diff<A, B>` <a class="toc" id="toc-diff" href="#toc-diff"></a>

As the name hints, `$Diff<A, B>` is the type representing the set difference of `A` and `B`, i.e. `A \ B`, where `A` and `B` are both [Object Types](../objects/). Here's an example:

```js
// @flow
type Props = { name: string, age: number };
type DefaultProps = { age: number };
type RequiredProps = $Diff<Props, DefaultProps>;

function setProps(props: RequiredProps) {
  // ...
}

setProps({ name: 'foo' });
setProps({ name: 'foo', age: 42, baz: false }); // you can pass extra props too
setProps({ age: 42 }); // error, name is required
```

As you may have noticed, the example is not a random one. `$Diff` is exactly what the React definition file uses to define the type of the props accepted by a React Component.

## `Class<T>` <a class="toc" id="toc-class" href="#toc-class"></a>

Given a type `T` representing instances of a class `C`, the type `Class<T>` is the type of the class `C`.
For example:

```js
// @flow
class Store {}
class ExtendedStore extends Store {}
class Model {}

function makeStore(storeClass: Class<Store>) {
  return new storeClass();
}

(makeStore(Store): Store);
(makeStore(ExtendedStore): Store);
(makeStore(Model): Model); // error
(makeStore(ExtendedStore): Model); // Flow infers the return type
```

For classes that take type parameters, you must also provide the parameter. For example:

```js
// @flow
class ParamStore<T> {
  constructor(data: T) {}
}

function makeParamStore<T>(storeClass: Class<ParamStore<T>>, data: T): ParamStore<T> {
  return new storeClass(data);
}
(makeParamStore(ParamStore, 1): ParamStore<number>);
(makeParamStore(ParamStore, 1): ParamStore<boolean>); // failed because of the second parameter
```

## `$Supertype<T>` <a class="toc" id="toc-supertype" href="#toc-supertype"></a>

Work in progress

## `$Subtype<T>` <a class="toc" id="toc-subtype" href="#toc-subtype"></a>

Work in progress

## `$Abstract<T>` <a class="toc" id="toc-abstract" href="#toc-abstract"></a>

Work in progress

## `$PropertyType<T, x>` <a class="toc" id="toc-propertytype" href="#toc-propertytype"></a>

A $PropertyType is the type at a given key.

As of Flow v0.36.0, `x` must be a literal string. In future versions, `x` may be allowed to be any type, as long
as that type exists on the keys of `T`.

```js
// @flow
type Person = {
  name: string,
  age: number,
  parent: Person
};

const newName: $PropertyType<Person, 'name'> = 'Michael Jackson';
const newAge: $PropertyType<Person, 'age'> = 50;
const newParent: $PropertyType<Person, 'parent'> = 'Joe Jackson';
```

This can be especially useful for referring to the type of React props, or, even the entire `props` type itself.

```js
// @flow
import React from 'react';
class Tooltip extends React.Component {
  props: {
    text: string,
    onMouseOver: ({x: number, y: number}) => void
  };
}

const someProps: $PropertyType<Tooltip, 'props'> = {
  text: 'foo',
  onMouseOver: (data: {x: number, y: number}) => undefined
};

const otherProps: $PropertyType<Tooltip, 'props'> = {
  text: 'foo'
  // Missing the `onMouseOver` definition
};
```

You can even nest lookups:

```js
// @flow
type PositionHandler = $PropertyType<$PropertyType<Tooltip, 'props'>, 'onMouseOver'>;
const handler: PositionHandler = (data: {x: number, y: number}) => undefined;
const handler2: PositionHandler = (data: string) => undefined; // wrong parameter types
```

You can use this in combination with `Class<T>` to get static props:

```js
// @flow
class BackboneModel {
  static idAttribute: string | false;
}

type ID = $PropertyType<Class<BackboneModel>, 'idAttribute'>;
const someID: ID = '1234';
const someBadID: ID = true;
```

## The Existential Type (`*`) <a class="toc" id="toc-the-existential-type" href="#toc-the-existential-type"></a>

`*` is known as the existential type.

An existential type is used as a placeholder to tell Flow to infer the type.

For example, in the `Class<ParamStore<T>>` example, we could have used an existential type for the return:

```js
// @flow
function makeParamStore<T>(storeClass: Class<ParamStore<T>>, data: T): * {
  return new storeClass(data);
}
(makeParamStore(ParamStore, 1): ParamStore<number>);
(makeParamStore(ParamStore, 1): ParamStore<boolean>); // failed because of the second parameter
```

The `*` can be thought of as an "auto" instruction to Flow, telling it to fill in the type from context.

In comparison to `any`, `*` may allow you to avoid losing type safety.

The existential operator is also useful for automatically filling in types without unnecessary verbosity:

```js
// @flow
class DataStore {
  data: *; // If this property weren't defined, you'd get an error just trying to assign `data`
  constructor() {
    this.data = {
      name: 'DataStore',
      isOffline: true
    };
  }
  goOnline() {
    this.data.isOffline = false;
  }
  changeName() {
    this.data.isOffline = 'SomeStore'; // oops, wrong key!
  }
}
```

## `$Exact<T>` <a class="toc" id="toc-exact" href="#toc-exact"></a>

`$Exact<{name: string}>` is a synonym for `{| name: string |}` as in the [Object documentation](../objects/#toc-exact-object-types).

```js
// @flow
type ExactUser = $Exact<{name: string}>;
type ExactUserShorthand = {| name: string |};

const user2 = {name: 'John Wilkes Booth'};
// These will both be satisfied because they are equivalent
(user2: ExactUser);
(user2: ExactUserShorthand);
```

## `$ObjMap<T, F>` <a class="toc" id="toc-objmap" href="#toc-objmap"></a>
`ObjMap<T, F>` is the type obtained by taking the type of the values of an object and mapping them with a type function.

> ### Warning
> `$ObjMap` is currently in an unstable state, due to a bug that allows invalid writes.
>
> For more information, please refer to [#2674](https://github.com/facebook/flow/issues/2674).

Let's see an example. Suppose you have a function called `run` that takes an object of thunks (functions in the form `() => A`) in input:

```js
// @flow
function run<A, O: {[key: string]: () => A}>(o: O) {
  return Object.keys(o).reduce((acc, k) => Object.assign(acc, { [k]: o[k]() }), {});
}
```

The function's purpose is to run all the thunks and return an object made of values. What's the return type of this function?

The keys are the same, but the values have a different type, namely the return type of each function. At a value level (the implementation of the function) we're essentially mapping over the object to produce new values for the keys. How to express this at a type level?

This is where `ObjMap<T, F>` comes in handy.

```js
// @flow

// let's write a typelevel function that takes a `() => V` and returns a `V` (its return type)
type ExtractReturnType = <V>(() => V) => V 

function run<A, O: {[key: string]: () => A}>(o: O): $ObjMap<O, ExtractReturnType> {
  return Object.keys(o).reduce((acc, k) => Object.assign(acc, { [k]: o[k]() }), {});
}
```

Let's try this out

```js
// @flow
const o = {
  a: () => true,
  b: () => 'foo'
};
      
(run(o).a: boolean); // Ok
(run(o).b: string);  // Ok
// $ExpectError
(run(o).b: boolean); // Nope, b is a string
// $ExpectError
run(o).c;            // Nope, c was not in the original object
```

This is extremely useful for expressing the return type of functions that manipulate objects values. You could use a similar approach (for instance) to provide the return type of bluebird's [`Promise.props`](http://bluebirdjs.com/docs/api/promise.props.html) function, which is like `Promise.all` but takes an object as input.

Here's a possible declaration of this function, which is very similar to our first example:

```js
// @flow
declare function props<A, O: { [key: string]: A }>(promises: O): Promise<$ObjMap<O, typeof $await>>;
```

And use:

```js
// @flow
const promises = { a: Promise.resolve(42) };
props(promises).then(o => {
  (o.a: 42); // Ok
  // $ExpectError
  (o.a: 43); // Error, flow knows it's 42
});
```
