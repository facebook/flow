---
title: Comment Types
slug: /types/comments
---

Flow supports a comment-based syntax, which makes it possible to use Flow
without having to compile your files.

```js flow-check
// @flow

/*::
type MyAlias = {
  foo: number,
  bar: boolean,
  baz: string,
};
*/

function method(value /*: MyAlias */) /*: boolean */ {
  return value.bar;
}

method({ foo: 1, bar: true, baz: ["oops"] });
```

These comments allow Flow to work in plain JavaScript files without any
additional work.

## Comment types syntax {#toc-comment-types-syntax}

There are two primary pieces of the syntax: type includes and type annotations.

### Comment type include {#toc-comment-type-include}

If you want to have Flow treat a comment as if it were normal syntax, you can
do so by adding a double colon `::` to the start of the comment.

```js
/*::
type Foo = {
  foo: number,
  bar: boolean,
  baz: string
};
*/

class MyClass {
  /*:: prop: string; */
}
```

This includes the code into the syntax that Flow sees.

```js
type Foo = {
  foo: number,
  bar: boolean,
  baz: string
};

class MyClass {
  prop: string;
}
```

But JavaScript ignores these comments, so all it has is the valid syntax.

```js
class MyClass {

}
```

This syntax is also available in a `flow-include` form.

```js
/*flow-include
type Foo = {
  foo: number,
  bar: boolean,
  baz: string
};
*/

class MyClass {
  /*flow-include prop: string; */
}
```

### Comment type annotation {#toc-comment-type-annotation}

Instead of typing out a full include every time, you can also use the type
annotation shorthand with a single colon `:` at the start of the comment.

```js
function method(param /*: string */) /*: number */ {
  // ...
}
```

This would be the same as including a type annotation inside an include
comment.

```js
function method(param /*:: : string */) /*:: : number */ {
  // ...
}
```

> **Note:** If you want to use optional function parameters you'll need to use
> the include comment form.

---

> **Special thanks to**: [Jarno Rantanen](https://github.com/jareware) for
> building [flotate](https://github.com/jareware/flotate) and supporting us
> merging his syntax upstream into Flow.
