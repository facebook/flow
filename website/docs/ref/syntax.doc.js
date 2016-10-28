/* @flow */
/*
---
id: syntax
title: Syntax
permalink: /docs/syntax.html
prev: quick-reference.html
next: builtins.html
---
*/
/*
  To bring static typing to JavaScript, Flow specifies a number of syntax
  extensions, which are used to describe types, annotate programs, and share
  types between modules.

  Flow's syntax extensions are only additions which can be easily stripped away
  and don't change the runtime behavior of JavaScript in any way.

  This page gives a high-level overview of the various syntax elements
  introduced by Flow.

  Remember that Flow has sophisticated type inference and it is often
  unnecessary to explicitly annotate your program with types.

  ## Stripping Flow syntax

  Before type-annotated code can run in a browser or Node, the Flow syntax must
  be stripped away.

  ### Babel 5.x

  Babel 5.x supports Flow's syntax extensions out of the box.

  ### Babel 6.x

  Babel 6.x introduced a plugin architecture, which makes it easier to customize
  to your particular flavor of JavaScript.

  To use Flow syntax with Babel 6.x, you need to include the
  [`transform-flow-strip-types` plugin][transform-flow-strip-types].

  Another option is to use the [React preset][preset-es2015] which has Flow
  support (including [`transform-flow-strip-types`][transform-flow-strip-types]) already built-in in addition to React-specific plugins.

  [transform-flow-strip-types]: https://babeljs.io/docs/plugins/transform-flow-strip-types/
  [preset-es2015]: https://babeljs.io/docs/plugins/preset-react/

  ## Comment syntax

  Flow also supports a comment-based syntax, which makes it possible to use Flow
  without requiring any compilation.

  See [Comment Syntax](/blog/2015/02/20/Flow-Comments.html) for more information
  and examples.

  ## Variable declarations

  To specify a type for a binding introduced in a variable declaration
  statement, add a type annotation after the binding.
*/

var foo: string = "Hello, World!";

/*
  When declaring multiple variables at once, type annotations can be provided
  for each variable.
*/

var bar: number = 0,
    baz: boolean = true;

/*
  ## Function declarations

  Type annotations for function parameters and the return type are supplied
  inline.
*/

function numVowels(word: string): number {
  const vowels = new Set("aeiou");
  let count = 0;
  for (let char of word)
    if (vowels.has(char))
      count++;
  return count;
}

/*
  Type parameters for generic functions are also provided inline.
*/

function reversed<T>(array: T[]): T[] {
  let ret = [];
  let i = array.length;
  while (i--)
    ret.push(array[i]);
  return ret;
}

/*
  Rest parameters should be annotated with an array type.
*/

function sum(...xs: number[]): number {
  return xs.reduce((a,b) => a + b);
}

/*
  Arrow functions also support inline annotation syntax.
*/

const flip = <A,B>([a,b]: [A,B]): [B,A] => [b,a];

/*
  See [Functions](functions.html) for more information and examples.
*/

/*
  ## Class declarations

  Type annotations for class fields are specified inside the class body. This
  syntax works well with the [class properties proposal][class-properties]
  currently being reviewed by TC39 for inclusion in a future JavaScript standard.

  Methods can also specify types for their parameter and return types.

  [class-properties]: https://github.com/jeffmo/es-class-fields-and-static-properties
*/

class Point {
  x: number;
  y: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }

  move(x: number, y: number) {
    this.x += x;
    this.y += y;
  }

  copy(): Point {
    return new Point(this.x, this.y);
  }
}

/*
  Type parameters for generic classes can also be specified inline.
*/

class Box<T> {
  _value: T;

  constructor(value: T) {
    this._value = value;
  }

  get(): T {
    return this._value;
  }
}

/*
  See [Classes](classes.html) for more information and examples.
*/

/*
  ## Type aliases

  Type aliases make it easy to refer to a potentially complex type by a simple
  name. Type aliases are completely removed during the compile step.

  See [Type Aliases](type-aliases.html) for more information and examples.
*/

/*
  ## Object types

  The type of an object specifies the type of each of its properties.
*/

type Person = {
  name: string,
  age: number,
};

/*
  Objects types can include a callable property, which allows values of that
  type be called like a function.
*/

type Callable = {
  (x: string): number,
  foo: string,
};

/*
  Object types can include an indexer property, which allows values of that type
  to be used like a dictionary.
*/

type Dictionary = {
  [x: number]: string,
  foo: string,
}

/*
  See [Objects](objects.html) for more information and examples.
*/

/*
  ## Function types

  In addition to the syntax for annotating a function inline, it's possible to
  express the type of a function in isolation.
*/

type TimesTwo = (value: number) => number;

/*
  Type parameters for generic function types are specified before the parameter
  list.
*/

type Identity = <T>(x: T) => T;

/*
  See [Functions](functions.html) for more information and examples.
*/

/*
  ## Array types

  Array types can be specified by adding `[]` to the end of a type, or using
  `Array<T>` syntax. Note that maybe types of the short-hand syntax apply to the
  entire array, not the contained value type.
*/

var array_of_num: number[] = [];
var array_of_num_alt: Array<number> = [];
var optional_array_of_num: ?number[] = null;
var array_of_optional_num: Array<?number> = [null, 0];

/*
  Tuple types describe arrays holding heterogeneous values.
*/

var tuple_of_str_and_num: [string, number] = ["Hi", 42];

/*
  See [Arrays](arrays.html) for more information and examples.
*/

/*
  ## Interface declarations

  Declaring an interface creates a type that multiple classes can satisfy
  without being part of the same inheritance hierarchy.

  Like type aliases, interfaces are entirely erased at compile time and have no
  runtime presence.
*/

interface Comparable<T> {
  compare(a: T, b: T): number;
}

/*
  ## Importing and exporting types

  It's possible to export types defined in one file for use in another. `import
  type` and `export type` are entirely erased at compile time and have no
  runtime presence.

  ```js
  # foo.js
  export type Foo = string;
  ```

  ```js
  import type { Foo } from "./foo";
  var foo: Foo = "Hello";
  ```

  See [Modules](modules.html) for more information and examples.

  ## Destructured bindings

  JavaScript's destructuring syntax can be used to introduce many bindings at
  once. Currently, type annotations must be applied to the entire pattern, not
  the individual bindings.
*/

var {a, b: {c}}: {a: string, b: {c: number}} = {a: "", b: {c: 0}};

/*
  See [Destructuring](destructuring.html) for more information and examples.
*/

/*
  ## Typecasts

  A typecast expression is a simple way to annotate any JavaScript expression.
*/

(1 + 1 : number);

var obj = {
  name: (null: ?string)
};

([1, "a", true]: Array<mixed>).map(x => x);

/*
  See [Typecasts](/blog/2015/02/18/Typecasts.html) for more information and
  examples.
*/
