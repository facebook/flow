---
layout: guide
---

Sometimes it is useful to create a type which is ***all of*** a set of other
types. For example, you might want to write a function which accepts an object
which is the combination of other object types. For this Flow supports
**intersection types**.

```js
// @flow
type A = { a: number };
type B = { b: boolean };
type C = { c: string };

function method(value: A & B & C) {
  // ...
}

// $ExpectError
method({ a: 1 }); // Error!
// $ExpectError
method({ a: 1, b: true }); // Error!
method({ a: 1, b: true, c: 'three' }); // Works!
```

## Intersection type syntax <a class="toc" id="toc-intersection-type-syntax" href="#toc-intersection-type-syntax"></a>

Intersection types are any number of types which are joined by an ampersand `&`.

```js
Type1 & Type2 & ... & TypeN
```

You may also add a leading ampersand which is useful when breaking intersection
types onto multiple lines.

```js
type Foo =
  & Type1
  & Type2
  & ...
  & TypeN
```

Each of the members of a intersection type can be any type, even another
intersection type.

```js
type Numbers = 1 & 2;
type Colors = 'red' & 'blue'

type Fish = Numbers & Colors;
```

Intersection types require all in, but one out

Intersections types are the opposite of union types. When calling a function
that accepts an intersection type we must pass in ***all of those types***. But
inside of our function we only have to treat it as ***any one of those
types***.

```js
// @flow
type A = { a: number };
type B = { b: boolean };
type C = { c: string };

function method(value: A & B & C) {
  var a: A = value;
  var b: B = value;
  var c: C = value;
}
```

Even as we treat our value as just one of the types we do not get an error
because it satisfies all of them.

### Impossible intersection types <a class="toc" id="toc-impossible-intersection-types" href="#toc-impossible-intersection-types"></a>

Using intersection types it is possible to create types which are not possible
to create at runtime. Intersection types will allow you to combine any set of
types, even ones that conflict with one another.

For example, you can create an intersection of a number and a string.

```js
// @flow
type NumberAndString = number & string;

function method(value: NumberAndString) {
  // ...
}

// $ExpectError
method(3.14); // Error!
// $ExpectError
method('hi'); // Error!
```

But you can't possibly create a value which is both a *number and a string*,
but you can create a type for it. There's no practical use for creating types
like this, but it's a side effect of how intersection types work.

### Intersections of object types <a class="toc" id="toc-intersections-of-object-types" href="#toc-intersections-of-object-types"></a>

When you create an intersection of object types you merge all of their
properties together.

For example, when you create an intersection of two objects with different sets
of properties, it will result in an object with all of the properties.

```js
// @flow
type One = { foo: number };
type Two = { bar: boolean };

type Both = One & Two;

var value: Both = {
  foo: 1,
  bar: true
};
```

But when you have properties that overlap by having the same name, it creates
an intersection of the property type as well.

For example, if you merge two objects with a property named prop, one with a
type of number and another with a type of boolean, the resulting object will
have an intersection of number and boolean.

```js
// @flow
type One = { prop: number };
type Two = { prop: boolean };

type Both = One & Two;

// $ExpectError
var value: Both = {
  prop: 1 // Error!
};
```
