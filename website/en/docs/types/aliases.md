---
layout: guide
---

When you have complicated types that you want to reuse in multiple places, you
can alias them in Flow using a **type alias**.

```js
// @flow
type MyObject = {
  foo: number,
  bar: boolean,
  baz: string,
};
```

These type aliases can be used anywhere a type can be used.

```js
// @flow
type MyObject = {
  // ...
};

var val: MyObject = { /* ... */ };
function method(val: MyObject) { /* ... */ }
class Foo { constructor(val: MyObject) { /* ... */ } }
```

## Type Alias Syntax <a class="toc" id="toc-type-alias-syntax" href="#toc-type-alias-syntax"></a>

Interfaces are created using the keyword `type` followed by it's name, an
equals sign `=`, and a type definition.

```js
type Alias = Type;
```

Any type can appear inside a type alias.

```js
type NumberAlias = number;
type ObjectAlias = {
  property: string,
  method(): number,
};
type UnionAlias = 1 | 2 | 3;
type AliasAlias = ObjectAlias;
```

#### Type Alias Generics <a class="toc" id="toc-type-alias-generics" href="#toc-type-alias-generics"></a>

Type aliases can also have their own [generics](../generics/).

```js
type MyObject<A, B, C> = {
  property: A,
  method(val: B): C,
};
```

Type alias generics are [parameterized](../generics/#toc-parameterized-generics).
When you use a type alias you need to pass parameters for each of its generics.

```js
// @flow
type MyObject<A, B, C> = {
  foo: A,
  bar: B,
  baz: C,
};

var val: MyObject<number, boolean, string> = {
  foo: 1,
  bar: true,
  baz: 'three',
};
```
