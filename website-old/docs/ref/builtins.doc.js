/* @flow */
/*
---
id: builtins
title: Built-in Types
permalink: /docs/builtins.html
prev: syntax.html
next: arrays.html
---
*/

/*
  Flow includes many built-in types, which can be used to describe values in
  JavaScript.

  There are types for primitive values, like `number` and `string`. Types like
  `any` and `mixed` describe more flexible constraints on values, whereas
  literal types describe specifically a single value.

  Flow comes out of the box with support for the JavaScript standard library,
  Browser APIs like the DOM, and the Node.js standard library.
*/

/*
  #### Note about typecast syntax

  The following examples make extensive use of typecasts in order to demonstrate
  type compatibility.

  In Flow, typecasts are sound, meaning that the statement `(e:T)` for some
  expression `e` and type `T` is a type error unless the inferred type of `e` is
  a valid subtype of `T`.

  In other words, with respect to validity a typecast behaves just like a type
  annotation on a variable or parameter. If `var x:T = e` would be valid, then
  `(e:T)` would be as well.
*/

// $ExpectError
(1 + 1: string); // Error: Numbers are not strings
("Hello, World": string); // OK: Strings are strings

class A {}
class B extends A {}
let a = new A(),
    b = new B();
(b: A); // OK: B is a subclass of A
// $ExpectError
(a: B); // Error: A is not a subclass of B

/*
  ## boolean

  This type describes a boolean value in JavaScript. The possible values of this
  type are `true` and `false`.
*/

(true: boolean);
(false: boolean);
// $ExpectError
("foo": boolean); // strings are not booleans

/*
  JavaScript specifies many implicit conversions, which provide boolean
  semantics to values of other types. Flow understands this and allows any
  expression to be used as a conditional in an `if` statement or as an operand
  to `&&`. However, if you need to cast an object specifically to the `boolean`
  type, you can use the built-in `Boolean` function
*/

function takes_boolean(x: boolean): void {}
// $ExpectError
takes_boolean(0); // Implicit casting is an error.
takes_boolean(Boolean(0)); // Adding an explicit cast type checks.

/*
  Note that `boolean` and `Boolean` are separate types. The former is the type
  of primitive booleans which appear in programs as literals `true` and `false`,
  or as the result of expressions like `a === b`. The latter is the type of
  Boolean wrapper objects, which are rarely used.
*/

// $ExpectError
(true: Boolean);
(new Boolean(false): Boolean);

/*
  ## number

  JavaScript has a single number type, which is IEEE 754 floating point numbers.
  The `number` type describes these values, which includes `Infinity` and `NaN`.
*/

(3.14: number);
(42: number);
(NaN: number);

(parseFloat("not a number"): number); // hint: NaN

/*
  Note that `number` and `Number` are separate types. The former is the type of
  primitive numbers which appear in programs as literals, like `3.14` and `42`,
  or as the result of expressions like `parseFloat(input.value)`.  The latter is
  the type of Number wrapper objects, which are rarely used.
*/

// $ExpectError
(0: Number);
(new Number(0): Number);

/*
  ## string
*/

("foo": string);
("bar": string);

/*
  Generally, implicit type casting is an error with Flow. However, it is a
  fairly common JavaScript idiom to produce a string by combining a `string` and
  a `number` with the binary operator `+`, so Flow accepts it.
*/

((100 + "%") : string);

/*
  Note that `string` and `String` are separate types. The former is the type of
  primitive strings which appear in programs as literals, like `"foo"` and
  `"bar"`, or as the result of expressions like `"" + 42`. The latter is the
  type of String wrapper objects, which are rarely used.
*/

// $ExpectError
("foo": String);
(new String("foo"): String);

/*
  ## null and void

  JavaScript has both `null` and `undefined`, which Flow is careful to treat
  separately. `null` (the value) has the type `null`. `undefined` has the type
  `void`.
*/

(null: null); // yup
// $ExpectError
(null: void); // nope

(undefined: void); // yup
// $ExpectError
(undefined: null); // nope

/*
  Optional object properties and optional function parameters have the type
  `T|void`, for some type `T`.
*/

function optional_fun(foo?: string) {
  (foo: string|void);
}
optional_fun("foo");
optional_fun(undefined);
optional_fun();
// $ExpectError
optional_fun(null); // null is not a string, nor void

type optional_obj = { foo?: string }
({foo: "foo"}: optional_obj);
({foo: undefined}: optional_obj);
({}: optional_obj);
// $ExpectError
({foo: null}: optional_obj); // null is not a string, nor void

/*
  Function parameters that have a default are optional as well, but only for
  callers. Within the function body, the binding has a non-`void` type.
*/

function default_fun(foo: string = "default foo") {
  (foo: string);
}
default_fun("foo");
default_fun(undefined);
default_fun();

/*
  Maybe types have the type `T|void|null` for some type `T`.
*/

function maybe_fun(foo: ?string) {
  (foo: string|void|null);
}
maybe_fun("foo");
maybe_fun(undefined);
maybe_fun();
maybe_fun(null);

/*
  ## any

  `any` is simultaneously a supertype of all types and a subtype of all types.
  Intuitively, an `any` value can take the place of "any" other value, and Flow
  will understand that to be well-typed.
*/

function takes_any(x: any): void {}
takes_any(0);
takes_any("");
takes_any({ foo: "bar" });

declare var unsafe: any;
(unsafe: number);
(unsafe: string);
(unsafe: { foo: string });

/*
  In addition to compatibility between types, it's useful to think of `any` in
  terms of operations that can be performed on values. Accessing any property on
  an `any` will yield an `any`. It is possible to call an `any` as a function,
  taking any number arguments of any type, which will also return `any`.
*/

unsafe.foo.bar.baz;
(unsafe("foo"): string);
(unsafe("bar"): number);

/*
  You can think of `any` as a kind of "backdoor" in the type system. Use of
  `any` is inherently unsafe and should be avoided whenever possible. However,
  it can also be incredibly convenient.

  For example, when adding types to existing code, using `any` can help make the
  gradual transition from untyped to typed code. Similarly, modeling third-party
  APIs with `any` can ease integration.

  Lastly, due to the highly dynamic nature of JavaScript, there are some idioms
  which Flow does not yet understand. Principled use of `any` makes it possible
  to wrap untyped code in types.

  ## mixed

  Like `any`, `mixed` is a supertype of all types. Unlike `any`, however,
  `mixed` is not a subtype of all types. This means `mixed` is like a safe
  but somewhat annoying version of `any`. It should be preferred over `any`
  whenever possible.
*/

function takes_mixed(x: mixed): void {}
takes_mixed(0);
takes_mixed("");
takes_mixed({ foo: "bar" });

function returns_mixed(): mixed {}
// $ExpectError
(returns_mixed(): number);
// $ExpectError
(returns_mixed(): string);
// $ExpectError
(returns_mixed(): { foo: string });

/*
  It's still possible to use a value with a `mixed`, but you must first
  [refine](/docs/dynamic-type-tests.html) the value.

  For example, let's construct a type of values which can be expressed directly
  as JSON. We can express this type quite naturally.
*/

type JSON = | string | number | boolean | null | JSONObject | JSONArray;
type JSONObject = { [key:string]: JSON };
type JSONArray = Array<JSON>;

/*
  Now let's write a function that verifies that a given value is a JSON value.
  If we annotated the parameter as `any`, we could just return the parameter and
  Flow would accept that without error because `any` is a subtype of all types,
  including `JSON`.

  If we use `mixed`, however, we can still pass any value into our function, as
  `mixed` is a supertype of all types. But in order to satisfy the `JSON` return
  type, Flow requires us to implement the necessary runtime type checks.
*/

function typedJSON(x: mixed): JSON {
  if (typeof x === "object" && x !== null) {
    let o: JSONObject = {};
    for (let k of Object.keys(x)) {
      o[k] = typedJSON(x[k]);
    }
    return o;
  }

  if (Array.isArray(x)) {
    return x.map(typedJSON);
  }

  if (x === null ||
      typeof x === "string" ||
      typeof x === "number" ||
      typeof x === "boolean") {
    return x;
  }

  throw new Error("Invalid JSON");
}

/*
  ## literal types

  While type `boolean`, `number`, and `string` types admit `true` and `false`,
  any number, and any string, respectively, it can also be useful to specify a
  type that admits a single value. This feature turns out to be surprisingly
  [versatile](disjoint-unions.html): literal types can be used to build enums
  and other disjoint unions, as well as express some common forms of method
  overloading for which the types `boolean`, `number`, and `string` are not
  adequate.
*/

("foo": "foo");
// $ExpectError
("bar": "foo"); // `"bar"` is not exactly `"foo"`
// $ExpectError
("fo"+"o": "foo"); // even simple expressions lose literal information

(1: 1);
// $ExpectError
(2: 1); // `2` is not exactly `1`
// $ExpectError
(1+1: 2); // even simple expressions lose literal information

(true: true);
// $ExpectError
(true: false); // `true` is not exactly `false`

// boolean expressions *do* preserve literal information
(!true: false);
(true && false: false);
(true || false: true);

/*
  Let's have a little fun with literal types. The following program shows how
  literals can be used to statically index into objects. It also shows how
  tuples of unions can be used to represent enums.
*/

type Suit =
  | "Diamonds"
  | "Clubs"
  | "Hearts"
  | "Spades";
type Rank =
  | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10
  | "Jack"
  | "Queen"
  | "King"
  | "Ace";
type Card = {
  suit: Suit,
  rank: Rank,
}

declare var cards: Card[];
cards.sort((a, b) => cardComparator(a, b));
cards.sort((a, b) => cardComparator(a, b, true)); // Aces high

function suitOrder(suit) {
  return {
    Diamonds: 0,
    Clubs: 1,
    Hearts: 3,
    Spades: 4,
  }[suit];
}

function rankOrder(rank, aceHigh = false) {
  if (typeof rank === "string") {
    return {
      Jack: 11,
      Queen: 12,
      King: 13,
      Ace: aceHigh ? 14 : 1,
    }[rank];
  } else {
    return rank;
  }
}

function cardComparator(a, b, aceHigh?) {
  return (rankOrder(a.rank, aceHigh) - rankOrder(b.rank, aceHigh))
      || (suitOrder(a.suit) - suitOrder(b.suit));
}

/*
  Note that Flow is able to infer the parameter and return types of `suitOrder`,
  `rankOrder`, and `cardComparator` correctly. Try copying that into your text
  editor and introducing type errors to see what Flow can catch!
*/

/*
  ## JavaScript standard library

  Type declarations for the [JavaScript standard library][CORE] are included
  with Flow.

  [CORE]: https://github.com/facebook/flow/blob/master/lib/core.js

  Flow has broad support for the standard library, including support for
  Iterables, Iterators, and Generators.

  ## Browser APIs

  Type declarations for the [Document Object Model][DOM] (DOM), [Browser Object
  Model][BOM] (BOM), and [CSS Object Model][CSSOM] (CSSOM) are all included with
  Flow.

  [DOM]: https://github.com/facebook/flow/blob/master/lib/dom.js
  [BOM]: https://github.com/facebook/flow/blob/master/lib/bom.js
  [CSSOM]: https://github.com/facebook/flow/blob/master/lib/cssom.js

  ## Node.js

  Type declarations for the [Node.js standard library][NODE] are included with
  Flow.

  [NODE]: https://github.com/facebook/flow/blob/master/lib/node.js
*/
