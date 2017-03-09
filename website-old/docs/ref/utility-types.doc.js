/* @flow */
/*
---
id: utility-types
title: Utility Types
permalink: /docs/utility-types.html
prev: declarations.html
next: react.html
---
*/

/*
## `$Keys<T>`
In Flow you can [use union types similar to enums](builtins.html#literal-types):
*/

type Suit = "Diamonds" | "Clubs" | "Hearts" | "Spades";

const clubs: Suit = 'Clubs';
// $ExpectError
const wrong: Suit = 'wrong'; // 'wrong' is not a Suit

/*
This is very handy, but sometimes you need to access the enum definition at runtime (i.e. at a value level).

Suppose for example that you want to associate a value to each suit of the previous example.

You could do
*/

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
// $ExpectError
printSuitNumber('foo'); // 'foo' is not a Suit

/*
but this doesn't feel very DRY, as we had to explicitly define the suit names twice.

In situations like this one, you can leverage the `$Keys<T>` operator. Let's see another example, this time using `$Keys`:
*/

const countries = {
  US: "United States",
  IT: "Italy",
  FR: "France"
};

type Country = $Keys<typeof countries>;

const italy: Country = 'IT';
// $ExpectError
const nope: Country = 'nope'; // 'nope' is not a Country

/*
In the example above, the type of `Country` is equivalent to `type Country = 'US' | 'IT' | 'FR'`, but Flow was able to extract it from the keys of `countries`.
*/

/*
## `$Diff<A, B>`
As the name hints, `$Diff<A, B>` is the type representing the set difference of `A` and `B`, i.e. `A \ B`, where `A` and `B` are both [Object Types](objects.html). Here's an example:
*/
type Props = { name: string, age: number };
type DefaultProps = { age: number };
type RequiredProps = $Diff<Props, DefaultProps>;

function setProps(props: RequiredProps) {
  // ...
}

setProps({ name: 'foo' });
setProps({ name: 'foo', age: 42, baz: false }); // you can pass extra props too
// $ExpectError
setProps({ age: 42 }); // error, name is required
/*

As you may have noticed, the example is not a random one. `$Diff` is exactly what the React definition file uses to define the type of the props accepted by a React Component.
*/

/*
## `Class<T>`
Given a type `T` representing instances of a class `C`, the type `Class<T>` is the type of the class `C`.
For example:
*/

class Store {}
class ExtendedStore extends Store {}
class Model {}

function makeStore(storeClass: Class<Store>) {
  return new storeClass();
}

(makeStore(Store): Store);
(makeStore(ExtendedStore): Store);
// $ExpectError
(makeStore(Model): Model); // error, Class<Model> does not satisfy Class<Store>
// $ExpectError
(makeStore(ExtendedStore): Model); // Flow infers the return type

/*
For classes that take type parameters, you must also provide the parameter. For example:
*/

class ParamStore<T> {
  constructor(data: T) {}
}

function makeParamStore<T>(storeClass: Class<ParamStore<T>>, data: T): ParamStore<T> {
  return new storeClass(data);
}
(makeParamStore(ParamStore, 1): ParamStore<number>);
// $ExpectError
(makeParamStore(ParamStore, 1): ParamStore<boolean>); // failed because of the second parameter

/*
## `$Supertype<T>`
Work in progress

## `$Subtype<T>`
Work in progress

## `$Abstract<T>`
Work in progress

## `$PropertyType<T, x>`
A $PropertyType is the type at a given key.

As of Flow v0.36.0, `x` must be a literal string. In future versions, `x` may be allowed to be any type, as long
as that type exists on the keys of `T`.
*/

type Person = {
  name: string,
  age: number,
  parent: Person
};

const newName: $PropertyType<Person, 'name'> = 'Michael Jackson';
const newAge: $PropertyType<Person, 'age'> = 50;
// $ExpectError
const newParent: $PropertyType<Person, 'parent'> = 'Joe Jackson';

/*
This can be especially useful for referring to the type of React props, or, even the entire `props` type itself.
*/

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

// $ExpectError
const otherProps: $PropertyType<Tooltip, 'props'> = {
  text: 'foo'
  // Missing the `onMouseOver` definition
};

/*
You can even nest lookups:
*/
type PositionHandler = $PropertyType<$PropertyType<Tooltip, 'props'>, 'onMouseOver'>;
const handler: PositionHandler = (data: {x: number, y: number}) => undefined;
// $ExpectError
const handler2: PositionHandler = (data: string) => undefined; // wrong parameter types

/*
You can use this in combination with `Class<T>` to get static props:
*/

class BackboneModel {
  static idAttribute: string | false;
}

type ID = $PropertyType<Class<BackboneModel>, 'idAttribute'>;
const someID: ID = '1234';
// $ExpectError
const someBadID: ID = true;

/*
## `*`
`*` is known as the existential type.

An existential type is used as a placeholder to tell Flow to infer the type.

For example, in the `Class<ParamStore<T>>` example, we could have used an existential type for the return:
*/
function makeParamStore<T>(storeClass: Class<ParamStore<T>>, data: T): * {
  return new storeClass(data);
}
(makeParamStore(ParamStore, 1): ParamStore<number>);
// $ExpectError
(makeParamStore(ParamStore, 1): ParamStore<boolean>); // failed because of the second parameter

/*
The `*` can be thought of as an "auto" instruction to Flow, telling it to fill in the type from context.

In comparison to `any`, `*` may allow you to avoid losing type safety.

The existential operator is also useful for automatically filling in types without unnecessary verbosity:
*/

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
    // $ExpectError
    this.data.isOffline = 'SomeStore'; // oops, wrong key!
  }
}

/*
## `$Exact<T>`
`$Exact<{name: string}>` is a synonym for `{| name: string |}` as in the [Object documentation](objects.html#exact-object-types).
*/

type ExactUser = $Exact<{name: string}>;
type ExactUserShorthand = {| name: string |};

const user2 = {name: 'John Wilkes Booth'};
// These will both be satisified because they are equivalent
(user2: ExactUser);
(user2: ExactUserShorthand);
