---
title: FAQ
description: Have a question about using Flow? Check here first!
slug: /faq
---

## I checked that `foo.bar` is not `null`, but Flow still thinks it is. Why does this happen and how can I fix it?

Flow does not keep track of side effects, so any function call may potentially nullify your check.
This is called [refinement invalidation](../lang/refinements/#toc-refinement-invalidations). Example:

```js flow-check
type Param = {
  bar: ?string,
}
function myFunc(foo: Param): string {
  if (foo.bar) {
    console.log("checked!");
    return foo.bar; // Flow errors. If you remove the console.log, it works
  }

  return "default string";
}
```

You can get around this by storing your checked values in local variables:

```js flow-check
type Param = {
  bar: ?string,
}
function myFunc(foo: Param): string {
  if (foo.bar) {
    const bar = foo.bar;
    console.log("checked!");
    return bar; // Ok!
  }

  return "default string";
}
```

## I checked that my value is of type A, so why does Flow still believe it's A | B?

Refinement invalidation can also occur variables are updated:

```js flow-check
type T = string | number;

let x: T = 'hi';

function f() {
  x = 1;
}

if (typeof x === 'string') {
  f();
  x as string;
}
```

A work around would be to make the variable `const` and refactor your code to avoid the reassignment:

```js flow-check
type T = string | number;

const x: T = 'hi';

function f(x: T): number {
  return 1;
}

if (typeof x === 'string') {
  const xUpdated = f(x);
  xUpdated as number;
  x as string;
}
```

## I'm in a closure and Flow ignores the if check that asserts that `foo.bar` is defined. Why?

In the previous section we showed how refinements are lost after a function call. The exact same thing happens within closures, since
Flow does not track how your value might change before the closure is called:

```js flow-check
const people = [{age: 12}, {age: 18}, {age: 24}];
const oldPerson: {age: ?number} = {age: 70};
if (oldPerson.age) {
  people.forEach(person => {
    console.log(`The person is ${person.age} and the old one is ${oldPerson.age}`);
  })
}
```

The solution here is to move the if check in the `forEach`, or to assign the `age` to an intermediate variable:

```js flow-check
const people = [{age: 12}, {age: 18}, {age: 24}];
const oldPerson: {age: ?number} = {age: 70};
if (oldPerson.age) {
  const age = oldPerson.age;
  people.forEach(person => {
    console.log(`The person is ${person.age} and the old one is ${age}`);
  })
}
```

## But Flow should understand that this function cannot invalidate this refinement, right?

Flow is not [complete](../lang/types-and-expressions/#toc-soundness-and-completeness), so it cannot check all code perfectly. Instead,
Flow will make conservative assumptions to try to be sound.

## Why can't I use a function in my if-clause to check the type of a property?

Flow doesn't track [refinements](.././lang/refinements/) made in separate function calls:

```js flow-check
const add = (first: number, second: number) => first + second;
const val: string | number = 1;
const isNumber = (x: mixed): boolean => typeof x === 'number';
if (isNumber(val)) {
  add(val, 2);
}
```

However, you can annotate your function with a [type guard](../types/type-guards/) to get this behavior:

```js flow-check
const add = (first: number, second: number) => first + second;
const val: string | number = 1;
// Return annotation updated:
const isNumber = (x: mixed): x is number => typeof x === 'number';
if (isNumber(val)) {
  add(val, 2);
}
```

## Why can't I pass an `Array<string>` to a function that takes an `Array<string | number>`

The function's argument allows `string` values in its array, but in this case Flow prevents the original array from receiving a `number`.
Inside the function, you would be able to push a `number` to the argument array, causing the type of the original array to no longer be accurate.

You can fix this error by changing the type of the argument to `$ReadOnlyArray<string | number>`, making it [covariant](../lang/variance/#toc-covariance).
This prevents the function body from pushing anything to the array, allowing it to accept narrower types.

As an example, this would not work:

```js flow-check
const fn = (arr: Array<string | number>) => {
  arr.push(123); // Oops! Array<string> was passed in - now inaccurate
  return arr;
};

const arr: Array<string> = ['abc'];

fn(arr); // Error!
```

but with `$ReadOnlyArray` you can achieve what you were looking for:

```js flow-check
const fn = (arr: $ReadOnlyArray<string | number>) => {
  // arr.push(321); NOTE! Since you are using $ReadOnlyArray<...> you cannot push anything to it
  return arr;
};

const arr: Array<string> = ['abc'];

fn(arr);
```

## Why can't I pass `{a: string}` to a function that takes `{a: string | number}`

The function argument allows `string` values in its field, but in this case Flow prevents the original object from having a `number` written to it.
Within the body of the function you would be able to mutate the object so that the property `a` would receive a `number`, causing the type of the original object to no longer be accurate.

You can fix this error by making the property [covariant](../lang/variance/#toc-covariance) (read-only): `{+a: string | number}`.
This prevents the function body from writing to the property, making it safe to pass more restricted types to the function.

As an example, this would not work:

```js flow-check
const fn = (obj: {a: string | number}) => {
  obj.a = 123; // Oops! {a: string} was passed in - now inaccurate
  return obj;
};

const object: {a: string} = {a: 'str' };

fn(object); // Error!
```

but with a covariant property you can achieve what you were looking for:

```js flow-check
const fn = (obj: {+a: string | number}) => {
  // obj.a = 123 NOTE! Since you are using covariant {+a: string | number}, you can't mutate it
  return obj;
};

const object: {a: string} = { a: 'str' };

fn(object);
```

## Why can't I refine a union of objects?

There are two potential reasons:
1. You are using inexact objects.
2. You are destructuring the object. When destructuring, Flow loses track of object properties.

Broken example:

```js flow-check
type Action =
  | {type: 'A', payload: string}
  | {type: 'B', payload: number};

// Not OK
const fn = ({type, payload}: Action) => {
  switch (type) {
    case 'A': return payload.length; // Error!
    case 'B': return payload + 10;
  }
}
```

Fixed example:

```js flow-check
type Action =
  | {type: 'A', payload: string}
  | {type: 'B', payload: number};

// OK
const fn = (action: Action) => {
  switch (action.type) {
    case 'A': return action.payload.length;
    case 'B': return action.payload + 10;
  }
}
```

## I got a "Missing type annotation" error. Where does it come from?

Flow requires type annotations at module boundaries to make sure it can scale. To read more about that, check out our [blog post](https://medium.com/flow-type/asking-for-required-annotations-64d4f9c1edf8) about that.

The most common case you'll encounter is when exporting a function or React component. Flow requires you to annotate inputs. For instance in this example, Flow will complain:

```js flow-check
export const add = a => a + 1;
```

The fix here is to add types to the parameters of `add`:

```js flow-check
export const add = (a: number): number => a + 1;
```

To see how you can annotate exported React components, check out our docs on [HOCs](../react/hoc/#toc-exporting-wrapped-components).

There are other cases where this happens, and they might be harder to understand. You'll get an error like `Missing type annotation for U` For instance, you wrote this code:

```js flow-check
const array = ['a', 'b']
export const genericArray = array.map(a => a)
```

Here, Flow will complain on the `export`, asking for a type annotation. Flow wants you to annotate exports returned by a generic function. The type of `Array.prototype.map` is `map<U>(callbackfn: (value: T, index: number, array: Array<T>) => U, thisArg?: any): Array<U>`. The `<U>` corresponds to what is called a [generic](../types/generics/), to express the fact that the type of the function passed to map is linked to the type of the array.

Understanding the logic behind generics might be useful, but what you really need to know to make your typings valid is that you need to help Flow to understand the type of `genericArray`.

You can do that by annotating the exported constant:

```js flow-check
const array = ['a', 'b']
export const genericArray: Array<string> = array.map(a => a)
```
