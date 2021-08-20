---
title: Union Types
slug: /types/unions
---

Sometimes it's useful to create a type which is ***one of*** a set of other
types. For example, you might want to write a function which accepts a set of
primitive value types. For this Flow supports **union types**.

```js flow-check
// @flow
function toStringPrimitives(value: number | boolean | string) {
  return String(value);
}

toStringPrimitives(1);       // Works!
toStringPrimitives(true);    // Works!
toStringPrimitives('three'); // Works!

// $ExpectError
toStringPrimitives({ prop: 'val' }); // Error!
// $ExpectError
toStringPrimitives([1, 2, 3, 4, 5]); // Error!
```

## Union type syntax {#toc-union-type-syntax}

Union types are any number of types which are joined by a vertical bar `|`.

```js
Type1 | Type2 | ... | TypeN
```

You may also add a leading vertical bar which is useful when breaking union
types onto multiple lines.

```js
type Foo =
  | Type1
  | Type2
  | ...
  | TypeN
```

Each of the members of a union type can be any type, even another union type.

```js
type Numbers = 1 | 2;
type Colors = 'red' | 'blue'

type Fish = Numbers | Colors;
```

## Union types requires one in, but all out {#toc-union-types-requires-one-in-but-all-out}

When calling our function that accepts a union type we must pass in ***one of
those types***. But inside of our function we are required to handle ***all of
the possible types***.

Let's rewrite our function to handle each type individually.

```js flow-check
// @flow
// $ExpectError
function toStringPrimitives(value: number | boolean | string): string { // Error!
  if (typeof value === 'number') {
    return String(value);
  } else if (typeof value === 'boolean') {
    return String(value);
  }
}
```

You'll notice that if we do not handle each possible type of our value, Flow
will give us an error.

## Unions & Refinements {#toc-unions-refinements}

When you have a value which is a union type it's often useful to break it apart
and handle each individual type separately. With union types in Flow you can
"refine" the value down to a single type.

For example, if we have a value with a union type that is a `number`, a
`boolean`, or a `string`, we can treat the number case separately by using
JavaScript's `typeof` operator.

```js flow-check
// @flow
function toStringPrimitives(value: number | boolean | string) {
  if (typeof value === 'number') {
    return value.toLocaleString([], { maximumSignificantDigits: 3 }); // Works!
  }
  // ...
}
```

By checking the typeof our value and testing to see if it is a number, Flow
knows that inside of that block it is only a number. We can then write code
which treats our value as a number inside of that block.

### Disjoint Unions {#toc-disjoint-unions}

There's a special type of union in Flow known as a "disjoint union" which can
be used in [refinements](../../lang/refinements/). These disjoint unions are
made up of any number of object types which are each tagged by a single
property.

For example, imagine we have a function for handling a response from a server
after we've sent it a request. When the request is successful, we'll get back
an object with a `success` property which is `true` and a `value` that we've
updated.

```js
{ success: true, value: false };
```

When the request fails, we'll get back an object with `success` set to `false`
and an `error` property describing the error.

```js
{ success: false, error: 'Bad request' };
```

We can try to express both of these objects in a single object type. However,
we'll quickly run into issues where we know a property exists based on the
success property but Flow does not.

```js flow-check
// @flow
type Response = {
  success: boolean,
  value?: boolean,
  error?: string
};

function handleResponse(response: Response) {
  if (response.success) {
    // $ExpectError
    var value: boolean = response.value; // Error!
  } else {
    // $ExpectError
    var error: string = response.error; // Error!
  }
}
```

Trying to combine these two separate types into a single one will only cause us
trouble.

Instead, if we create a union type of both object types, Flow will be able to
know which object we're using based on the success property.

```js flow-check
// @flow
type Success = { success: true, value: boolean };
type Failed  = { success: false, error: string };

type Response = Success | Failed;

function handleResponse(response: Response) {
  if (response.success) {
    var value: boolean = response.value; // Works!
  } else {
    var error: string = response.error; // Works!
  }
}
```

### Disjoint unions with exact types {#toc-disjoint-unions-with-exact-types}

Disjoint unions require you to use a single property to distinguish each object
type. You cannot distinguish two different objects by different properties.

```js flow-check
// @flow
type Success = { success: true, value: boolean };
type Failed  = { error: true, message: string };

function handleResponse(response:  Success | Failed) {
  if (response.success) {
    // $ExpectError
    var value: boolean = response.value; // Error!
  }
}
```

This is because in Flow it is okay to pass an object value with more properties
than the object type expects (because of width subtyping).

```js flow-check
// @flow
type Success = { success: true, value: boolean };
type Failed  = { error: true, message: string };

function handleResponse(response:  Success | Failed) {
  // ...
}

handleResponse({
  success: true,
  error: true,
  value: true,
  message: 'hi'
});
```

Unless the objects somehow conflict with one another there is no way to
distinguish them.

However, to get around this you could use **exact object types**.

```js flow-check
// @flow
type Success = {| success: true, value: boolean |};
type Failed  = {| error: true, message: string |};

type Response = Success | Failed;

function handleResponse(response: Response) {
  if (response.success) {
    var value: boolean = response.value;
  } else {
    var message: string = response.message;
  }
}
```

With exact object types, we cannot have additional properties, so the objects
conflict with one another and we are able to distinguish which is which.
