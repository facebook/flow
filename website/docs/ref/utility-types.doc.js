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
# Utility Types
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
Work in progress

## `$Supertype<T>`
Work in progress

## `$Subtype<T>`
Work in progress

## `$Abstract<T>`
Work in progress

## `#PropertyType<T, x>`
Work in progress
*/
