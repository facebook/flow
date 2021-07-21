---
layout: guide
---

Flow’s Indexed Access Types allow you to get the type of a property from an [object](../objects), [array](../arrays), or [tuple](../tuples) type.

Indexed Access Types are a replacement for the [`$PropertyType`](../utilities/#toc-propertytype) and [`$ElementType`](../utilities/#toc-elementtype) utility types.
If you're familiar with those utility types already, here is a quick conversion guide:
- `$PropertyType<Obj, 'prop'>` &rarr;  `Obj['prop']`
- `$ElementType<Obj, T>` &rarr; `Obj[T]`
- `$ElementType<$PropertyType<Obj, 'prop'>, T>` &rarr; `Obj['prop'][T]`


## Indexed Access Type Usage <a class="toc" id="toc-indexed-access-type-usage" href="#toc-indexed-access-type-usage"></a>

Access an object type's property:
```js
// @flow
type Cat = {
  name: string,
  age: number,
  hungry: boolean,
};

type Hungry = Cat['hungry']; // type Hungry = boolean
const isHungry: Hungry = true; // OK - `Hungry` is an alias for `boolean`

// The index can be a type, not just a literal:
type AgeProp = 'age';
type Age = Cat[AgeProp]; // type Age = number
const catAge: Age = 6; // OK - `Age` is an alias for `number`
```

Access an array type's element, by getting the type at the array's indices (which are `number`s):
```js
// @flow
type CatNames = Array<string>;

type CatName = CatNames[number]; // type CatName = string
const myCatsName: CatName = 'whiskers'; // OK - `CatName` is an alias for `string`
```

Access a tuple type's elements:
```js
// @flow
type Pair = [string, number];

const name: Pair[0] = 'whiskers'; // OK - `Pair[0]` is an alias for `string`
const age: Pair[1] = 6; // OK - `Pair[1]` is an alias for `number`
const wrong: Pair[2] = true; // Error - `Pair` only has two elements
```

The index can be a union, including the result of calling [`$Keys<...>`](../utilities/#toc-keys):
```js
// @flow
type Cat = {
  name: string,
  age: number,
  hungry: boolean,
};

type Values = Cat[$Keys<Cat>]; // type Values = string | number | boolean
```

The index can also be a generic:
```js
// @flow
function getProp<O: {+[string]: mixed}, K: $Keys<O>>(o: O, k: K): O[K] {
  return o[k];
}

const x: number = getProp({a: 42}, 'a'); // OK
const y: string = getProp({a: 42}, 'a'); // Error - `number` is not a `string`
getProp({a: 42}, 'b'); // Error - `b` does not exist in object type
```

You can nest these accesses:
```js
// @flow
type Cat = {
  name: string,
  age: number,
  hungry: boolean,
  personality: {
    friendly: boolean,
    hungerLevel: number,
  }
};

type Friendly = Cat['personality']['friendly']; // type Friendly = boolean
const isFriendly: Friendly = true; // Pet the cat
```


## Optional Indexed Access Types <a class="toc" id="toc-optional-indexed-access-types" href="#toc-optional-indexed-access-types"></a>

Optional Indexed Access Types work like optional chaining. They allow you to access properties from nullable object types.
If before you did:

```js
type T = $ElementType<$NonMaybeType<Obj>, 'prop'> | void;
```

You can now do:

```js
type T = Obj?.['prop'];
```

Like optional chaining, the resulting types of Optional Indexed Access Types include `void`.
If you have a long chain of nested optional accesses, you can wrap the entire thing with a `$NonMaybeType<...>` if you don’t want `void` in your resulting type.

Example:

```js
// @flow
type TasksContent = ?{
  tasks?: Array<{
    items?: {
      metadata?: {
        title: string,
        completed: boolean,
      },
    },
  }>,
};

type TaskData = TasksContent?.['tasks']?.[number]?.['items']?.['metadata'];
```

There is one small difference between optional chaining and Optional Indexed Access Types.
If the object type you access is not nullable, the resulting type in optional chaining will not include `void`.
With Optional Indexed Access Types, for implementation reasons, the resulting type will always include `void`.
However, if your object type is not nullable then you don’t need to use an Optional Indexed Access Type, but should just use a regular Indexed Access Type.


## Adoption <a class="toc" id="toc-adoption" href="#toc-adoption"></a>

To use Indexed Access Types, you need to upgrade your infrastructure so that it supports the syntax:

- `flow` and `flow-parser`: 0.155
- `prettier`: 2.3.2
- `babel`: 7.14

We have created an ESLint rule that warns on `$ElementType` and `$PropertyType` usage and recommends Indexed Access Types instead.
It includes an auto-fixer that can handle most cases. You can simply enable this rule on your codebase and autofix all existing issues.

Install [`eslint-plugin-fb-flow`](https://www.npmjs.com/package/eslint-plugin-fb-flow), and add `fb-flow` to your ESLint plugin list.
Then enable the rule in your ESLint config:

```
'fb-flow/use-indexed-access-type': 1,
```
